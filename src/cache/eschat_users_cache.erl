-module(eschat_users_cache).
-export([init/0, put/2, get/1, remove/1]).

init() ->
    ets:new(?MODULE, [named_table, set, public]).

put(Id, Data) ->
    ets:insert(?MODULE, {Id, Data}).

get(Id) ->
    case ets:lookup(?MODULE, Id) of
        [{Id, Data}] -> {ok, Data};
        [] -> not_found
    end.

remove(Id) ->
    ets:delete(?MODULE, Id).