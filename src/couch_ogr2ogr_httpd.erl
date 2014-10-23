-module(couch_ogr2ogr_httpd).

-export([handle_req/1]).

-include_lib("couch/include/couch_db.hrl").

handle_req(#httpd{method='POST'}=Req) ->
  validate_config(),
  verify_roles(Req),
  couch_httpd:validate_ctype(Req, "application/json"),
  TempDir = mochitemp:mkdtemp(),
  try
    BaseName = filename:join(TempDir, "remove-me"),
    {Props} = couch_httpd:json_body_obj(Req),
    lists:foreach( fun({Extension, Content}) ->
      file:write_file(BaseName ++ "." ++ binary_to_list(Extension), base64:decode(Content))
    end, Props ),
    Command0 = get_config("command")
      ++ " -f GeoJSON "
      ++ BaseName ++ ".geojson "
      ++ BaseName ++ ".shp "
      ++ case proplists:is_defined(<<"qpj">>, Props) of
        true -> " -a_srs " ++ BaseName ++ ".qpj ";
        false -> ""
      end,
    AcquireGeoJSON = fun(Command) ->
      os:cmd(Command),
      case file:read_file( BaseName ++ ".geojson") of
        {ok, GeoJSON} -> ejson:decode(GeoJSON);
        {error, _} -> throw({bad_request, "Failed to convert your input into a GeoJSON."})
      end
    end,
    {GeoJSON} = AcquireGeoJSON(Command0),
    couch_httpd:send_json( Req, case proplists:is_defined(<<"crs">>, GeoJSON) of
      true -> {GeoJSON};
      false ->
        file:delete(BaseName ++ ".geojson"),
        Command1 = Command0
          ++ " -t_srs "
          ++ get_config("fallback_crs"),
        AcquireGeoJSON(Command1)
      end
    )
  after
    mochitemp:rmtempdir(TempDir)
  end;

handle_req(#httpd{method='GET'}=Req) ->
  validate_config(),
  verify_roles(Req),
  couch_httpd:send_response(Req, 200,
    [{"Content-type", "application/json;charset=utf-8"}],
    os:cmd(get_config("command"))
  );

handle_req(Req) ->
  couch_httpd:send_method_not_allowed(Req, "GET,POST").

get_config(Name) ->
  case couch_config:get("ogr2ogr", Name) of
    "undefined" -> undefined;
    "" -> undefined;
    Config -> Config
  end.

validate_config() ->
  lists:foreach( fun ({Name, _}) ->
    case {Name, get_config(Name), os:getenv("GDAL_DATA")} of
      {"GDAL_DATA", undefined, false} -> throw({internal_server_error,
        "Found GDAL_DATA neither in config nor system env."});
      {"GDAL_DATA", undefined, _} -> ok;
      {"GDAL_DATA", GDAL_DATA, _} -> os:putenv("GDAL_DATA", GDAL_DATA);
      {_, undefined, _} -> throw({internal_server_error,
        "Uninitialized config parameter " ++ Name ++ "."});
      _ -> ok
    end
  end, couch_config:get("ogr2ogr") ),
  try
    lists:foreach( fun(Role) when is_binary(Role) -> ok
    end, ejson:decode(get_config("roles")) )
  catch
    _:_ -> throw({internal_server_error,
      "Roles must be a JSON array of strings."})
  end.

verify_roles(#httpd{ user_ctx=#user_ctx{ roles=Roles } }) ->
  case ejson:decode(get_config("roles")) of
    [] -> ok;
    AllowedRoles -> verify_roles( Roles, [ <<"_admin">> | AllowedRoles] )
  end.
verify_roles(Roles, [ Role | AllowedRoles] ) ->
  case lists:member(Role, Roles) of
    true -> ok;
    _ -> verify_roles(Roles, AllowedRoles)
  end;
verify_roles(_, [] ) ->
  throw({unauthorized, "You are not authorized to access this service."}).
