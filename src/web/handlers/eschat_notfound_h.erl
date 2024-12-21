%%%-------------------------------------------------------------------
%%% @author student
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2024 12:14 pm
%%%-------------------------------------------------------------------
-module(eschat_notfound_h).
-author("student").
-behavior(cowboy_handler).
%% API
-export([init/2]).


init(#{resp_headers := RH} = Req, Env) ->
  {ok, cowboy_req:reply(404, RH#{<<"content-type">> => <<"plain/text">>}, <<"PAGE NOT FOUND">>, Req), Env};

init(Req, Env) ->
  {ok, cowboy_req:reply(404, #{<<"content-type">> => <<"plain/text">>}, <<"PAGE NOT FOUND">>, Req), Env}.