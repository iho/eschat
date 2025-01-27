-module(eschat_sessions_cache).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([put/2, get/1, remove/1, get_all_by_user_id/1, clean_up/0]).

-record(state, {table}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Table = ets:new(?MODULE, [set, protected, named_table]),
    timer:send_interval(600000, self(), clean_up),
    {ok, #state{table = Table}}.

put(SessionId, Data) ->
    gen_server:cast(?MODULE, {put, SessionId, Data}).

get(SessionId) ->
    gen_server:call(?MODULE, {get, SessionId}).

get_all_by_user_id(SessionId) ->
    gen_server:call(?MODULE, {get_all_by_user_id, SessionId}).

remove(SessionId) ->
    gen_server:cast(?MODULE, {remove, SessionId}).

clean_up() ->
    gen_server:call(?MODULE, clean_up).

handle_call({get, SessionId}, _From, State) ->
    Result =
        case ets:lookup(State#state.table, SessionId) of
            [{SessionId, Data}] ->
                ActiveTo = maps:get(active_to, Data),
                Now = calendar:datetime_to_gregorian_seconds(
                          calendar:universal_time()),
                case ActiveTo > Now of
                    true ->
                        {ok, Data};
                    false ->
                        ets:delete(State#state.table, SessionId),
                        not_found
                end;
            [] ->
                not_found
        end,
    lager:debug("eschat_sessions_cache:get: ~p", [Result]),
    {reply, Result, State};
handle_call({get_all_by_user_id, UserId}, _From, State) ->
    Now = calendar:datetime_to_gregorian_seconds(
              calendar:universal_time()),
    Result =
        ets:foldl(fun({SessionID, Data}, Acc) ->
                     ActiveTo = maps:get(active_to, Data),
                     UserID = maps:get(user_id, Data),
                     case ActiveTo > Now of
                         true ->
                            case UserID == UserId of
                                true -> [SessionID | Acc];
                                false -> Acc
                            end; 
                         false ->
                             ets:delete(State#state.table, SessionID),
                             Acc
                     end
                  end,
                  [],
                  State#state.table),
    lager:debug("eschat_sessions_cache:get_all: ~p", [Result]),
    {reply, {ok, Result}, State};
handle_call(clean_up, _From, State) ->
    Result = eschat_db_sessions:clean_up(),
    lager:debug("eschat_sessions_cache:clean_up: ~p", [Result]),
    Now = calendar:datetime_to_gregorian_seconds(
              calendar:universal_time()),
    ets:foldl(fun({SessionId, Data}, _) ->
                 ActiveTo = maps:get(active_to, Data),
                 case ActiveTo < Now of
                     true -> ets:delete(State#state.table, SessionId);
                     false -> ok
                 end
              end,
              ok,
              State#state.table),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({put, SessionId, Data}, State) ->
    ets:insert(State#state.table, {SessionId, Data}),
    {noreply, State};
handle_cast({remove, SessionId}, State) ->
    ets:delete(State#state.table, SessionId),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
