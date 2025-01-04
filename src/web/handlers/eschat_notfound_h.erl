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

-include_lib("epgsql/include/epgsql.hrl").

%% API
-export([init/2]).


init(#{resp_headers := RH} = Req, Env) ->
  {ok, cowboy_req:reply(404, RH#{<<"content-type">> => <<"plain/text">>}, <<"PAGE NOT FOUND">>, Req), Env};

init(Req, Env) ->
  Fun = fun(ConnPid) ->
    {ok, _, [{Version}]} =  epgsql:squery(ConnPid, <<"SELECT version()">>),
    Version
  end,
  Version = sherlock:transaction(database, Fun),
  {ok, cowboy_req:reply(404, #{<<"content-type">> => <<"plain/text">>}, Version, Req), Env}.