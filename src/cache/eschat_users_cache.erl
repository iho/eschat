-module(eschat_users_cache).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([put/2, get/1, remove/1]).

-record(state, {table}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Table = ets:new(?MODULE, [set, protected, named_table]),
    {ok, #state{table = Table}}.

put(Id, Data) ->
    gen_server:cast(?MODULE, {put, Id, Data}).

get(Id) ->
    gen_server:call(?MODULE, {get, Id}).

remove(Id) ->
    gen_server:cast(?MODULE, {remove, Id}).

handle_call({get, Id}, _From, State) ->
    Result =
        case ets:lookup(State#state.table, Id) of
            [{Id, Data}] ->
                {ok, Data};
            [] ->
                not_found
        end,
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({put, Id, Data}, State) ->
    ets:insert(State#state.table, {Id, Data}),
    {noreply, State};
handle_cast({remove, Id}, State) ->
    ets:delete(State#state.table, Id),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
