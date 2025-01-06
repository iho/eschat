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

-include("eschat_user_h.hrl").


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
  Req4 = case Result of
    {ok, Data, Req3} ->
      case eschat_user:get_user(Data) of
        {error, Type} ->
          Req31 = cowboy_req:set_resp_headers(?RESP_HEADERS(<<"1">>), Req3),
          JSB = eschat_json:encode(#{<<"status">> => <<"ok">>, <<"details">> => Type}),
          cowboy_req:set_resp_body(JSB, Req31);
        {Status, UserData} ->
          Req31 = cowboy_req:set_resp_headers(?RESP_HEADERS(<<"1">>), Req3),
          Req32 = cowboy_req:set_resp_header(<<"x-cache">>, erlang:atom_to_binary(Status), Req31),
          JSB = eschat_json:encode(#{<<"status">> => <<"ok">>, <<"details">> => UserData}),
          cowboy_req:set_resp_body(JSB, Req32)
      end;
      _ -> Req
  end,
  Req1 = cowboy_req:reply(200, Req4),
  {ok, Req1, Env};

init(#{method := <<"POST">>, bindings := #{action:= <<"register">>} = Det} = Req, Env) ->
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

  Req4 = case Result of
    {ok, Data, Req3} ->
      case eschat_user:register_user(Data) of
        {error, Type} ->
          Req31 = cowboy_req:set_resp_headers(?RESP_HEADERS(<<"1">>), Req3),
          JSB = eschat_json:encode(#{<<"status">> => <<"ok">>, <<"details">> => Type}),
          cowboy_req:set_resp_body(JSB, Req31);
        {Status, UserData} ->
          Req31 = cowboy_req:set_resp_headers(?RESP_HEADERS(<<"1">>), Req3),
          Req32 = cowboy_req:set_resp_header(<<"x-cache">>, erlang:atom_to_binary(Status), Req31),
          JSB = eschat_json:encode(#{<<"status">> => <<"ok">>, <<"details">> => UserData}),
          cowboy_req:set_resp_body(JSB, Req32)
      end;
      _ -> Req
    end,

  Req1 = cowboy_req:reply(200, Req4),
  {ok, Req1, Env};

init(Req, Env) ->
  eschat_notfound_h:init(cowboy_req:set_resp_headers(?RESP_HEADERS(<<"3">>), Req), Env).