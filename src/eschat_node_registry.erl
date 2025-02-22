
-module(eschat_node_registry).
-export([start_link/0, init/1, register_node/0, cleanup_nodes/0]).

-define(HEARTBEAT_INTERVAL, 10000). % 10 seconds
-define(CLEANUP_INTERVAL, 30000).   % 30 seconds
-define(NODE_TIMEOUT, 30).          % 30 seconds for node timeout

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    schedule_heartbeat(),
    schedule_cleanup(),
    {ok, #{}}.

register_node() ->
    NodeId = node(),
    {Host, Port} = eschat_config:get_web_config(),
    Query = "INSERT INTO nodes (node_id, host, web_port, last_seen) 
             VALUES ($1, $2, $3, NOW())
             ON CONFLICT (node_id) 
             DO UPDATE SET last_seen = NOW()",
    case sherlock:query(Query, [NodeId, Host, Port]) of
        {ok, _} -> ok;
        Error -> Error
    end.

cleanup_nodes() ->
    Query = "DELETE FROM nodes WHERE last_seen < NOW() - INTERVAL '$1 seconds'",
    Fun = fun(Pid) ->
              case epgsql:equery(Pid, Query, [?NODE_TIMEOUT]) of
                  {ok, _} -> ok;
                  Error -> Error
              end
          end,

    case sherlock:transaction(database, Fun) of
        ok -> ok;
        {error, _Type} = Err -> Err
    end.
handle_info(heartbeat, State) ->
    register_node(),
    schedule_heartbeat(),
    {noreply, State};

handle_info(cleanup, State) ->
    cleanup_nodes(),
    schedule_cleanup(),
    {noreply, State}.

schedule_heartbeat() ->
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat).

schedule_cleanup() ->
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup).