-module(eschat_chatmembers_cache).
-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export([
    put/3,
    get_members/1,
    is_owner/2,
    is_member/2,
    remove_member/2,
    remove_chat/1
]).

-record(state, {
    table
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Table = ets:new(?MODULE, [bag, protected, named_table, {read_concurrency, true}]),
    {ok, #state{table = Table}}.

put(ChatId, UserId, IsOwner) ->
    gen_server:cast(?MODULE, {put, ChatId, UserId, IsOwner}).

get_members(ChatId) ->
    ets:select(?MODULE, [{{{ChatId, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}]).

is_owner(ChatId, UserId) ->
    case ets:lookup(?MODULE, {ChatId, UserId}) of
        [{_, true}] -> true;
        _ -> false
    end.

is_member(ChatId, UserId) ->
    case ets:lookup(?MODULE, {ChatId, UserId}) of
        [{_, _}] -> true;
        _ -> false
    end.

remove_member(ChatId, UserId) ->
    gen_server:cast(?MODULE, {remove_member, ChatId, UserId}).

remove_chat(ChatId) ->
    gen_server:cast(?MODULE, {remove_chat, ChatId}).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({put, ChatId, UserId, IsOwner}, State) ->
    ets:insert(State#state.table, {{ChatId, UserId}, IsOwner}),
    {noreply, State};

handle_cast({remove_member, ChatId, UserId}, State) ->
    ets:delete(State#state.table, {ChatId, UserId}),
    {noreply, State};

handle_cast({remove_chat, ChatId}, State) ->
    ets:match_delete(State#state.table, {{ChatId, '_'}, '_'}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.