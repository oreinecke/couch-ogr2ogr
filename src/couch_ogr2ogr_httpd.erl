-module(couch_ogr2ogr_httpd).

-export([handle_req/1, get_allowed_roles/0]).

-include_lib("couch/include/couch_db.hrl").

handle_req(#httpd{method='POST'}=Req) ->
  verify_roles(Req),
  validate_config(),
  couch_httpd:validate_ctype(Req, "application/json"),
  TempDir = mochitemp:mkdtemp(),
  BaseName = filename:join(TempDir, "remove-me"),
  {Props} = couch_httpd:json_body_obj(Req),
  lists:foreach( fun({Extension, Content}) ->
    file:write_file(BaseName ++ "." ++ binary_to_list(Extension), base64:decode(Content))
  end, Props ),
  os:cmd(get_command() 
    ++ " -f GeoJSON " 
    ++ case proplists:is_defined(<<"qpj">>, Props) of
      true -> " -a_srs " ++ BaseName ++ ".qpj ";
      false -> "" end
    ++ BaseName ++ ".geojson " 
    ++ BaseName ++ ".shp"
  ),
  {ok, GeoJSON} = file:read_file( BaseName ++ ".geojson"),
  mochitemp:rmtempdir(TempDir),
  couch_httpd:send_response(Req, 200, [{"Content-type", "application/json;charset=utf-8"}], GeoJSON);

handle_req(#httpd{method='GET'}=Req) ->
  verify_roles(Req),
  validate_config(),
  couch_httpd:send_response(Req, 200,
    [{"Content-type", "application/json;charset=utf-8"}],
    os:cmd(get_command())
  );

handle_req(Req) ->
  couch_httpd:send_method_not_allowed(Req, "GET,POST").

get_command() ->
  case couch_config:get("ogr2ogr", "command") of
    "null" -> undefined;
    "undefined" -> undefined;
    Command -> Command
  end.

validate_config() ->
  case get_command() of 
    undefined -> throw({internal_server_error, "Undefined path to ogr2ogr comand."});
    _-> ok
  end.

get_allowed_roles() ->
  case couch_config:get("ogr2ogr", "roles") of
    undefined -> [];
    Roles -> [ list_to_binary(Role) || Role <- string:tokens(Roles, ", ") ]
  end.

verify_roles(#httpd{user_ctx=UserCtx}) ->
  verify_roles(UserCtx);
verify_roles(#user_ctx{roles=Roles}) ->
  case get_allowed_roles() of
    [] -> ok;
    AllowedRoles -> verify_roles( Roles, [ <<"_admin">> | AllowedRoles] )
  end.
verify_roles(Roles, [ Role | AllowedRoles] ) -> 
  case lists:member(Role, Roles) of
    true -> ok;
    _ -> verify_roles(Roles, AllowedRoles)
  end;
verify_roles(_Roles, [] ) -> 
  throw({unauthorized, "You are not authorized to access this service."}).
