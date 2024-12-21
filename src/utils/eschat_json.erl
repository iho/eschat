%%%-------------------------------------------------------------------
%%% @author student
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Dec 2024 1:12 pm
%%%-------------------------------------------------------------------
-module(eschat_json).
-author("student").

%% API
-export([encode/1]).
-export([decode/1]).

encode(JEterms) ->
  jsone:encode(JEterms, []).

decode(JBin) ->
  jsone:decode(JBin, []).