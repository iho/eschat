-module(eschat_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		#{
			id => eschat_db_sup,             % mandatory
			start => {eschat_db_sup, start_link, []},            % mandatory
			restart => permanent,         % optional
%%			significant => significant(), % optional
			shutdown => 5000,   % optional
			type => supervisor,             % optional
			modules => dynamic
		},
		#{
			id => eschat_web_srv,             % mandatory
			start => {eschat_web_srv, start_link, []},            % mandatory
			restart => permanent,         % optional
%%			significant => significant(), % optional
			shutdown => 5000,   % optional
			type => worker,             % optional
			modules => dynamic
		}
	],
	{ok, {{one_for_one, 10, 30}, Procs}}.
