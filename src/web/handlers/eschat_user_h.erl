-module(eschat_user_h).

-behavior(cowboy_handler).

-include("eschat_user_h.hrl").

-export([init/2]).

-import(eschat_webutils, [reply/4, error_response/3, success_response/3]).

%% "/api/:vsn/user/:action[/:id]"

-define(ROUTES,
        #{{<<"POST">>, <<"v1">>, <<"register">>} =>
              #{handler => fun handle_register/2,
                middlewares => [fun eschat_webutils:with_json_body/3]},
          {<<"POST">>, <<"v1">>, <<"login">>} =>
              #{handler => fun handle_login/2,
                middlewares => [fun eschat_webutils:with_json_body/3]},
          {<<"GET">>, <<"v1">>, <<"sessions">>} =>
              #{handler => fun handle_sessions/2,
                middlewares => [fun eschat_webutils:with_session/3]},
          {<<"GET">>, <<"v1">>, <<"session">>} =>
              #{handler => fun handle_session/2,
                middlewares => [fun eschat_webutils:with_session/3]}}).

init(Req0, State) ->
    Version = cowboy_req:binding(vsn, Req0),
    Action = cowboy_req:binding(action, Req0),
    Method = cowboy_req:method(Req0),
    handle_request(Method, Version, Action, Req0, State).

handle_request(Method, Version, Path, Req, State) ->
    case maps:find({Method, Version, Path}, ?ROUTES) of
        {ok, #{handler := Handler, middlewares := Middlewares}} ->
            apply_middleware(Middlewares, Handler, Req, State);
        error ->
            error_response(not_found, Req, State)
    end.

apply_middleware([], Handler, Req, State) ->
    Handler(Req, State);
apply_middleware([Middleware | Rest], Handler, Req, State) ->
    Middleware(fun(Req1, State1) -> apply_middleware(Rest, Handler, Req1, State1) end,
               Req,
               State).

%% Handlers
handle_register(#{json_data := #{<<"login">> := Login, <<"password">> := Password}} =Req, State) ->
    case eschat_db_users:create_user(Login, Password) of
        {ok, UserId} ->
            create_session_and_respond(UserId, Req, State);
        {error, user_exists} ->
            error_response(user_exists, Req, State);
        {error, _} ->
            error_response(registration_failed, Req, State)
    end.

handle_login(#{json_data := #{<<"login">> := Login, <<"password">> := Password} } = Req, State) ->
    case eschat_db_users:get_user_by_cred(Login, Password) of
        {ok, UserId} ->
            create_session_and_respond(UserId, Req, State);
        {error, invalid_credentials} ->
            error_response(invalid_credentials, Req, State);
        {error, _} ->
            error_response(login_failed, Req, State)
    end.

handle_sessions(#{user_id := UserId} = Req, State) ->
    case eschat_db_sessions:get_sessions(UserId) of
        {ok, Sessions} ->
            success_response(#{sessions => Sessions}, Req, State);
        {error, _} ->
            error_response(fetch_failed, Req, State)
    end.

handle_session(#{user_id := UserId} = Req, State) ->
    case eschat_db_users:get_user_by_id(UserId) of
        {ok, #user{id = Id, login = Login}} ->
            success_response(#{sessions => #{id => Id, login => Login}}, Req, State);
        {error, _} ->
            error_response(fetch_failed, Req, State)
    end.

create_session_and_respond(UserId, Req, State) ->
    case eschat_db_sessions:create_session(UserId, 3600) of
        {ok, SessionId} ->
            success_response(#{user_id => UserId, session_id => SessionId}, Req, State);
        {error, _} ->
            error_response(session_creation_failed, Req, State)
    end.
