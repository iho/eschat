%%%-------------------------------------------------------------------
%%% @author student
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2024 11:58 am
%%%-------------------------------------------------------------------
-module(eschat_user_h).

-author("student").

-behavior(cowboy_handler).

-include("eschat_user_h.hrl").

-export([init/2]).

%% "/api/:vsn/user/:action[/:id]"

init(Req0, State) ->
  Vsn = cowboy_req:binding(vsn, Req0),
  Action = cowboy_req:binding(action, Req0),
  Action = cowboy_req:binding(action, Req0),
  Method = cowboy_req:method(Req0),
  handle_request(Method, Vsn, Action, Req0, State).

handle_request(<<"POST">>, <<"v1">>, <<"register">>, Req0, State) ->
    with_json_body(fun handle_register/3, Req0, State);

handle_request(<<"POST">>, <<"v1">>, <<"login">>, Req0, State) ->
    with_json_body(fun handle_login/3, Req0, State);

handle_request(<<"GET">>, <<"v1">>, <<"sessions">>, Req, State) ->
    with_session(fun handle_sessions/3, Req, State);

handle_request(<<"GET">>, <<"v1">>, <<"session">>, Req, State) ->
    with_session(fun handle_session/3, Req, State);

handle_request(_, _, _, Req, State) ->
    error_response(not_found, Req, State).

%% Body parsing middleware
with_json_body(Handler, Req0, State) ->
    case read_body(Req0) of
        {ok, Data, Req1} -> Handler(Data, Req1, State);
        {error, Reason} -> error_response(Reason, Req0, State)
    end.

read_body(Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req1} ->
            case eschat_json:decode(Body) of
                #{<<"login">> := _, <<"password">> := _} = Data -> 
                    {ok, Data, Req1};
                _ -> 
                    {error, invalid_request}
            end;
        Error -> Error
    end.

%% Session middleware
with_session(Handler, Req, State) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined -> 
            error_response(no_session, Req, State);
        SessionId -> 
            case validate_session(SessionId) of
                {ok, UserId} -> Handler(UserId, Req, State);
                {error, Reason} -> error_response(Reason, Req, State)
            end
    end.

%% Handlers
handle_register(#{<<"login">> := Login, <<"password">> := Password}, Req, State) ->
    case eschat_db_users:create_user(Login, Password) of
        {ok, UserId} ->
            create_session_and_respond(UserId, Req, State);
        {error, user_exists} ->
            error_response(user_exists, Req, State);
        {error, _} ->
            error_response(registration_failed, Req, State)
    end.

handle_login(#{<<"login">> := Login, <<"password">> := Password}, Req, State) ->
    case eschat_db_users:get_user_by_cred(Login, Password) of
        {ok, UserId} ->
            create_session_and_respond(UserId, Req, State);
        {error, invalid_credentials} ->
            error_response(invalid_credentials, Req, State);
        {error, _} ->
            error_response(login_failed, Req, State)
    end.

handle_sessions(UserId, Req, State) ->
    case eschat_db_sessions:get_sessions(UserId) of
        {ok, Sessions} -> 
            success_response(#{sessions => Sessions}, Req, State);
        {error, _} -> 
            error_response(fetch_failed, Req, State)
    end.

handle_session(UserId, Req, State) ->
    case eschat_db_users:get_user_by_id(UserId) of
        {ok, #user{id = Id, login = Login}} ->
            success_response(#{sessions => #{id => Id, login => Login}}, Req, State);
        {error, _} ->
            error_response(fetch_failed, Req, State)
    end.

%% Helper functions
validate_session(SessionId) ->
    eschat_db_sessions:get_session_by_id(SessionId).

create_session_and_respond(UserId, Req, State) ->
    case eschat_db_sessions:create_session(UserId, 3600) of
        {ok, SessionId} ->
            success_response(#{
                user_id => UserId,
                session_id => SessionId
            }, Req, State);
        {error, _} ->
            error_response(session_creation_failed, Req, State)
    end.

%% Response formatters
success_response(Data, Req, State) ->
    reply(200, Data#{status => <<"success">>}, Req, State).

error_response(invalid_request, Req, State) ->
    reply(400, #{status => <<"error">>, message => <<"Invalid request format">>}, Req, State);
error_response(no_session, Req, State) ->
    reply(401, #{status => <<"error">>, message => <<"No session provided">>}, Req, State);
error_response(invalid_session, Req, State) ->
    reply(401, #{status => <<"error">>, message => <<"Invalid session">>}, Req, State);
error_response(invalid_credentials, Req, State) ->
    reply(401, #{status => <<"error">>, message => <<"Invalid credentials">>}, Req, State);
error_response(user_exists, Req, State) ->
    reply(409, #{status => <<"error">>, message => <<"User already exists">>}, Req, State);
error_response(not_found, Req, State) ->
    reply(404, #{status => <<"error">>, message => <<"Not found">>}, Req, State);
error_response(_, Req, State) ->
    reply(500, #{status => <<"error">>, message => <<"Internal server error">>}, Req, State).

reply(Status, Response, Req, State) ->
    Req1 = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        eschat_json:encode(Response),
        Req),
    {ok, Req1, State}.