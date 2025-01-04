%%%-------------------------------------------------------------------
%%% @author student
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Dec 2024 11:57 am
%%%-------------------------------------------------------------------
-author("student").

-define(NOW_SEC, erlang:system_time(second)).
-define(CACHE_TTL, 10).

-record(user, {
  id,
  login,
  password,
  ttl = (?NOW_SEC + ?CACHE_TTL)
}).
