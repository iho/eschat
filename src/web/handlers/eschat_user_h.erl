%%%-------------------------------------------------------------------
%%% @author student
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2024 11:58 am
%%%-------------------------------------------------------------------
-module(eschat_user_h).
-author("student").
-behavior(cowboy_handler).

-define(RESP_HEADERS(LINE), #{
  <<"content-type">> => <<"application/json">>,
  <<"x-api">> => erlang:atom_to_binary(?MODULE),
  <<"x-init">> => LINE
}).

%% API
-export([init/2]).
%% "/api/:vsn/user/:action[/:id]"

init(#{method := <<"GET">>, bindings := #{action := _Action, id := _Id} = Det} = Req, Env) ->
  JSB = eschat_json:encode(#{<<"status">> => <<"ok">>, <<"details">> => Det}),
  Req1 = cowboy_req:reply(200, ?RESP_HEADERS(<<"1">>), JSB, Req),
  {ok, Req1, Env};

init(#{method := <<"POST">>, bindings := #{action:= <<"new">>} = Det} = Req, Env) ->
  CT = cowboy_req:header(<<"content-type">>, Req),
  lager:debug("CT ~p", [CT]),
  Result = case CT of
    <<"application/json">>    ->
      {ok, Body, Req2} = eschat_http_body:read(Req),
      {ok, eschat_json:decode(Body), Req2};
    <<"application/x-www-form-urlencoded">> ->
      {ok, Body, _Req2} = cowboy_req:read_urlencoded_body(Req);
    _ -> {error, unxepected_format}
  end,
  lager:debug("Result ~p", [Result]),

  case Result of
    {ok, Struct, _} ->
      Login = eschat_xpath:get_val(<<"login">>, Struct),
      Passw = eschat_xpath:get_val(<<"password">>, Struct),
      lager:info("Struct: ~p -> Login: ~p, Password: ~p", [Struct, Login, Passw]);
    _ ->
      ok
  end,

  eschat_user:new(login, password),
  JSB = eschat_json:encode(#{<<"status">> => <<"ok">>, <<"details">> => Det}),
  Req1 = cowboy_req:reply(200, ?RESP_HEADERS(<<"2">>), JSB, Req),
  {ok, Req1, Env};

init(Req, Env) ->
  eschat_notfound_h:init(cowboy_req:set_resp_headers(?RESP_HEADERS(<<"3">>), Req), Env).