-module(eschat_messages_h).

%% API
-export([init/2]).

%% /api/:vsn/messages/:action/:id"
-define(ROUTES,
        #{{<<"v1">>, <<"PUT">>, undefined} =>
              #{handler => fun handle_set_last_read/2,
                middlewares => [ fun eschat_webutils:with_session/3, fun eschat_webutils:with_json_body/3]},
            {<<"v1">>, <<"GET">>, <<"create">>} =>
                #{handler => fun handle_get_last_read/2,
                    middlewares => [ fun eschat_webutils:with_session/3, fun eschat_webutils:with_json_body/3]}}).
            

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


handle_set_last_read(Req = #{user_id := UserId, json_data := #{<<"chat_id">> := ChatId, <<"message_id">> := MessageId}}, State) ->
    case eschat_db_messages:set_last_read(UserId, ChatId, MessageId) of
        ok ->
            eschat_webutils:success_response(#{status => <<"success">>}, Req, State);
        {error, not_found} ->
            eschat_webutils:error_response(not_found, Req, State);
        {error, _} ->
            eschat_webutils:error_response(operation_failed, Req, State)
    end.

handle_get_last_read(Req = #{user_id := UserId, json_data := #{<<"chat_id">> := ChatId}}, State) ->
    case eschat_db_messages:get_last_read(UserId, ChatId) of
        {ok, MessageId} ->
            eschat_webutils:success_response(#{status => <<"success">>, message_id => MessageId}, Req, State);
        {error, not_found} ->
            eschat_webutils:error_response(not_found, Req, State);
        {error, _} ->
            eschat_webutils:error_response(operation_failed, Req, State)
    end.