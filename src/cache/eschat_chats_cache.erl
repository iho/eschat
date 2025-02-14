-module(eschat_chats_cache).
-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export([
    put/2,
    get/1,
    update_name/2,
    remove/1
]).

-record(state, {
    table
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Table = ets:new(?MODULE, [set, protected, named_table, {read_concurrency, true}]),
    {ok, #state{table = Table}}.

put(ChatId, #{name := _, owner_id := _} = Data) ->
    gen_server:cast(?MODULE, {put, ChatId, Data}).

get(ChatId) ->
    case ets:lookup(?MODULE, ChatId) of
        [{ChatId, Data}] -> {ok, Data};
        [] -> not_found
    end.

update_name(ChatId, NewName) ->
    gen_server:cast(?MODULE, {update_name, ChatId, NewName}).

remove(ChatId) ->
    gen_server:cast(?MODULE, {remove, ChatId}).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({put, ChatId, Data}, State) ->
    ets:insert(State#state.table, {ChatId, Data}),
    {noreply, State};

handle_cast({update_name, ChatId, NewName}, State) ->
    case ets:lookup(State#state.table, ChatId) of
        [{ChatId, Data}] ->
            NewData = Data#{name => NewName},
            ets:insert(State#state.table, {ChatId, NewData});
        [] -> 
            ok
    end,
    {noreply, State};

handle_cast({remove, ChatId}, State) ->
    ets:delete(State#state.table, ChatId),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.