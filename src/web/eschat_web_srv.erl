%%%-------------------------------------------------------------------
%%% @author student
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2024 11:46 am
%%%-------------------------------------------------------------------
-module(eschat_web_srv).
-author("student").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(eschat_web_srv_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #eschat_web_srv_state{}} | {ok, State :: #eschat_web_srv_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  self() ! restart,
  {ok, #eschat_web_srv_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #eschat_web_srv_state{}) ->
  {reply, Reply :: term(), NewState :: #eschat_web_srv_state{}} |
  {reply, Reply :: term(), NewState :: #eschat_web_srv_state{}, timeout() | hibernate} |
  {noreply, NewState :: #eschat_web_srv_state{}} |
  {noreply, NewState :: #eschat_web_srv_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #eschat_web_srv_state{}} |
  {stop, Reason :: term(), NewState :: #eschat_web_srv_state{}}).
handle_call(_Request, _From, State = #eschat_web_srv_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #eschat_web_srv_state{}) ->
  {noreply, NewState :: #eschat_web_srv_state{}} |
  {noreply, NewState :: #eschat_web_srv_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #eschat_web_srv_state{}}).
handle_cast(_Request, State = #eschat_web_srv_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #eschat_web_srv_state{}) ->
  {noreply, NewState :: #eschat_web_srv_state{}} |
  {noreply, NewState :: #eschat_web_srv_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #eschat_web_srv_state{}}).
handle_info(restart, State = #eschat_web_srv_state{}) ->
  restart(),
  {noreply, State};
handle_info(_Info, State = #eschat_web_srv_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #eschat_web_srv_state{}) -> term()).
terminate(_Reason, _State = #eschat_web_srv_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #eschat_web_srv_state{},
    Extra :: term()) ->
  {ok, NewState :: #eschat_web_srv_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #eschat_web_srv_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


restart() ->
  Rotes = [
    {'_', [
      {"/api/:vsn/user/:action[/:id]", eschat_user_h,     []},
      {"/api/:vsn/chat[/:id]",         eschat_chat_h,     []},
      {"/api/:vsn/session[/:id]",         eschat_session_h,     []},
      {"/api/:vsn/session[/:id]",         eschat_session_h,     []},
      {'_',                            eschat_notfound_h, []}
    ]}
  ],
  Dispatch = cowboy_router:compile(Rotes),
  Settings = [{port, 8998}],
  Status = cowboy:start_clear(?MODULE, Settings, #{env => #{dispatch => Dispatch}}),
  lager:info("Web server ~p -> ~p : ~p",[?MODULE, Status, Settings]).