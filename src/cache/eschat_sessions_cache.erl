-module(eschat_sessions_cache).
-export([init/0, put/2, get/1, remove/1]).

init() ->
    ets:new(?MODULE, [named_table, set, public]).

put(SessionId, Data) ->
    ets:insert(?MODULE, {SessionId, Data}).

get(SessionId) ->
    case ets:lookup(?MODULE, SessionId) of
        [{SessionId, Data}] -> {ok, Data};
        [] -> not_found
    end.

remove(SessionId) ->
    ets:delete(?MODULE, SessionId).