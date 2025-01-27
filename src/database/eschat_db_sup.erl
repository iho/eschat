%%%-------------------------------------------------------------------
%%% @author student
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Dec 2024 11:18 am
%%%-------------------------------------------------------------------
-module(eschat_db_sup).
-author("student").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init([]) ->
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  AChild = #{id => 'eschat_db_user_cache_srv',
    start => {'eschat_db_user_cache_srv', start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => dynamic},
  BChild = #{id => 'eschat_users_cache',
    start => {'eschat_users_cache', start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => dynamic},

  CChild = #{
    id => 'eschat_sessions_cache',
    start => {'eschat_sessions_cache', start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => dynamic
  },


  {ok, {SupFlags, [AChild, BChild, CChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
