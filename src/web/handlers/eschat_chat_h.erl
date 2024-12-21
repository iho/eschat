%%%-------------------------------------------------------------------
%%% @author student
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2024 12:14 pm
%%%-------------------------------------------------------------------
-module(eschat_chat_h).
-author("student").
-behavior(cowboy_handler).
%% API
-export([init/2]).


init(Req, Env) ->
  {ok, Req, Env}.