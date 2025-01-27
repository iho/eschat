-module(eschat_users_cache).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([put/2, get/1, get_by_login/1, remove/1]).

-record(state, {table}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Table = ets:new(?MODULE, [set, protected, named_table, {read_concurrency, true}]),
    {ok, #state{table = Table}}.

put(Id, Data) ->
    gen_server:cast(?MODULE, {put, Id, Data}).

get(Id) ->
    case ets:lookup(?MODULE, {id, Id}) of
        [{_, Data}] ->
            {ok, Data};
        [] ->
            not_found
    end.

get_by_login(Login) ->
    case ets:lookup(?MODULE, {login, Login}) of
        [{_, Id}] ->
            {ok, Id};
        [] ->
            not_found
    end.

remove(Id) ->
    gen_server:cast(?MODULE, {remove, Id}).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({put, Id, #{login := Login} = Data}, State) ->
    ets:insert(State#state.table, [{{id, Id}, Data}, {{login, Login}, Id}]),
    {noreply, State};
handle_cast({remove, Id}, State) ->
    case ets:lookup(State#state.table, {id, Id}) of
        [{_, #{login := Login}}] ->
            ets:delete(State#state.table, {id, Id}),
            ets:delete(State#state.table, {login, Login});
        [] ->
            ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
