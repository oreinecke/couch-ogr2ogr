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
    Command0 = get_command(BaseName)
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
  TempDir = mochitemp:mkdtemp(),
  try
    BaseName = filename:join(TempDir, "remove-me"),
    lists:foreach( fun({Extension, Content}) ->
      file:write_file(BaseName ++ "." ++ Extension, base64:decode(Content))
    end, [
      {"shx","AAAnCgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAANugDAAABAAAAHdwpRWmyVEFSycOdQ3"
          ++ "xVQR3cKUVpslRBUsnDnUN8VUEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          ++ "AAAAADIAAAAK"},
      {"shp","AAAnCgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQOgDAAABAAAAHdwpRWmyVEFSycOdQ3"
          ++ "xVQR3cKUVpslRBUsnDnUN8VUEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          ++ "AAAAAAEAAAAKAQAAAB3cKUVpslRBUsnDnUN8VUE="},
      {"dbf","A18HGgEAAABhAKEAAAAAAAAAAAAAAAAAAAAAAAAAAABDUlMAAAAAAAAAAEMAAAAAUA"
          ++ "AAAAAAAAAAAAAAAAAAAHR5cGUAAAAAAAAAQwAAAABQAAAAAAAAAAAAAAAAAAAADSBF"
          ++ "UFNHOjMxNDY5ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC"
          ++ "AgICAgICAgICAgICAgICAgICAgICAgICAgICAgIENSUyAgICAgICAgICAgICAgICAg"
          ++ "ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC"
          ++ "AgICAgICAgICAg"}
    ]),
    file:write_file(BaseName ++ ".prj", "PROJCS[\"Germany_Zone_5\"," ++
      "GEOGCS[\"GCS_Deutsches_Hauptdreiecksnetz\"," ++
      "DATUM[\"D_Deutsches_Hauptdreiecksnetz\"," ++
      "SPHEROID[\"Bessel_1841\",6377397.155,299.1528128]],PRIMEM[\"Greenwich\",0.0]," ++
      "UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Transverse_Mercator\"]," ++
      "PARAMETER[\"False_Easting\",5500000.0],PARAMETER[\"False_Northing\",0.0]," ++
      "PARAMETER[\"Central_Meridian\",15.0],PARAMETER[\"Scale_Factor\",1.0]," ++
      "PARAMETER[\"Latitude_Of_Origin\",0.0],UNIT[\"Meter\",1.0]]"),
    Command0 = get_command(BaseName),
    try
      os:cmd(Command0),
      {ok, Result} = file:read_file(BaseName ++ ".geojson"),
      {GeoJSON} = ejson:decode(Result)
    catch
      _:_ -> throw({internal_server_error,
        "Error trying to run " ++ get_config("command") ++ "."})
    end,
    couch_httpd:send_json(Req, {[{<<"ok">>,true}]})
  after
    mochitemp:rmtempdir(TempDir)
  end;

handle_req(Req) ->
  couch_httpd:send_method_not_allowed(Req, "GET,POST").

get_command(BaseName) ->
  get_config("command")
    ++ " -f GeoJSON "
    ++ BaseName ++ ".geojson "
    ++ BaseName ++ ".shp".

get_config(Name) ->
  Error = {internal_server_error,
    "Uninitialized config parameter " ++ Name ++ "."},
  case couch_config:get("ogr2ogr", Name) of
    "undefined" -> throw(Error);
    undefined -> throw(Error);
    "" -> throw(Error);
    Config -> Config
  end.

validate_config() ->
  case {os:getenv("GDAL_DATA"), couch_config:get("ogr2ogr", "GDAL_DATA")} of
    {false, _} -> os:putenv("GDAL_DATA", get_config("GDAL_DATA"));
    {_, ""} -> ok;
    {_, undefined} -> ok;
    {_, "undefined"} -> ok;
    {_, GDAL_DATA} -> os:putenv("GDAL_DATA", GDAL_DATA)
  end,
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
