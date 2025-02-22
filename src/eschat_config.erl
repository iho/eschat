-module(eschat_config).

-export([get_web_config/0]).

get_web_config() ->
    ExtractHostPort =
        fun([{_, Props}]) ->
           {Ip, Port} = {proplists:get_value(ip, Props), proplists:get_value(port, Props)},
           {Ip, Port}
        end,
    ExtractHostPort(ranch:info()).
