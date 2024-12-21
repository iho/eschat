%%%-------------------------------------------------------------------
%%% @author student
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2024 1:14 pm
%%%-------------------------------------------------------------------
-module(eschat_http_body).
-author("student").

%% API
-export([read/1]).


read(Req) ->
  do_read(cowboy_req:read_body(Req), <<>>).

do_read({more, Chunk, Req}, Buff) ->
  do_read(cowboy_req:read_body(Req), <<Buff/binary, Chunk/binary>>);
do_read({ok, Chunk, Req}, Buff) ->
  {ok, <<Buff/binary, Chunk/binary>>, Req}.