-module(eschat_chats_h).

-behavior(cowboy_handler).

-include("eschat_chat_h.hrl").

%% API
-export([init/2]).

%% /api/:vsn/chat/:action/:id"
-define(ROUTES,
        #{{<<"v1">>, <<"GET">>, undefined} =>
              #{handler => fun handle_get_chats/2,
                middlewares => [ fun eschat_webutils:with_session/3]}}).

init(Req0, State) ->
    Vsn = cowboy_req:binding(vsn, Req0),
    Method = cowboy_req:method(Req0),
    Action = cowboy_req:binding(action, Req0),
    handle_request(Vsn, Method, Action, Req0, State).

handle_request(Vsn, Method, Action, Req, State) ->
    lager:debug("Vsn: ~p~n", [Vsn]),
    lager:debug("Method: ~p~n", [Method]),
    lager:debug("Action: ~p~n", [Action]),
    case maps:find({Vsn, Method, Action}, ?ROUTES) of
        {ok, #{handler := Handler, middlewares := Middlewares}} ->
            apply_middleware(Middlewares, Handler, Req, State);
        error ->
            eschat_webutils:error_response(not_found, Req, State)
    end.

%% Middleware application
apply_middleware([], Handler, Req, State) ->
    Handler(Req, State);
apply_middleware([Middleware | Rest], Handler, Req, State) ->
    Middleware(fun(Req1, State1) -> apply_middleware(Rest, Handler, Req1, State1) end,
               Req,
               State).

handle_get_chats(Req = #{user_id := UserId}, State) ->
    lager:debug("Getting chats for user ~p", [UserId]),
    Res = eschat_db_chats:get_user_chats(UserId),
    lager:debug("Got chats: ~p", [Res]),
    case Res of
        {ok, Chats} ->
            eschat_webutils:success_response(#{status => <<"success">>,
                                               chats => Chats},
                                             Req,
                                             State);
        {error, _} ->
            eschat_webutils:error_response(fetch_failed, Req, State)
    end.

format_chats(Chats) ->
    lists:map(fun(#chat{id = Id,
                        name = Name,
                        owner_id = OwnerId}) ->
                 #{id => Id,
                   name => Name,
                   owner_id => OwnerId}
              end,
              Chats).
