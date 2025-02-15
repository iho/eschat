%%%-------------------------------------------------------------------
%%% @author student
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2024 12:14 pm
%%%-------------------------------------------------------------------
-module(eschat_chat_h).
-author("student").
-behavior(cowboy_handler).
%% API
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    ChatId = cowboy_req:binding(id, Req0),
    handle_request(Method, ChatId, Req0, State).


%% API
handle_request(<<"POST">>, undefined, Req, State) ->
    with_auth(fun handle_create_chat/3, Req, State);

handle_request(<<"POST">>, ChatId, Req, State) ->
    Action = cowboy_req:binding(action, Req),
    handle_chat_action(Action, ChatId, Req, State);

handle_request(<<"GET">>, ChatId, Req, State) ->
    with_auth(fun(_UserId, Req1, State1) -> 
        handle_get_members(ChatId, Req1, State1)
    end, Req, State);

handle_request(<<"DELETE">>, ChatId, Req, State) ->
    with_auth(fun(UserId, Req1, State1) ->
        handle_delete_chat(ChatId, UserId, Req1, State1)
    end, Req, State).

%% Action handlers
handle_chat_action(<<"add_member">>, ChatId, Req, State) ->
    with_auth_and_body(fun(UserId, Body, Req1, State1) ->
        handle_add_member(ChatId, UserId, Body, Req1, State1)
    end, Req, State);

handle_chat_action(<<"remove_member">>, ChatId, Req, State) ->
    with_auth_and_body(fun(UserId, Body, Req1, State1) ->
        handle_remove_member(ChatId, UserId, Body, Req1, State1)
    end, Req, State);

handle_chat_action(<<"update_name">>, ChatId, Req, State) ->
    with_auth_and_body(fun(UserId, Body, Req1, State1) ->
        handle_update_name(ChatId, UserId, Body, Req1, State1)
    end, Req, State);

handle_chat_action(<<"last_read">>, ChatId, Req, State) ->
    with_auth(fun(UserId, Req1, State1) ->
        Method = cowboy_req:method(Req1),
        handle_last_read(Method, ChatId, UserId, Req1, State1)
    end, Req, State).

%% Chat operations
handle_create_chat(UserId, Req, State) ->
    with_body(fun(Body, Req1) ->
        case Body of
            #{<<"name">> := Name} ->
                case eschat_db_chats:create_chat(Name, UserId) of
                    {ok, ChatId} -> success_response(#{chat_id => ChatId}, Req1, State);
                    {error, _} -> error_response(create_failed, Req1, State)
                end;
            _ -> 
                error_response(invalid_format, Req1, State)
        end
    end, Req).

handle_get_members(ChatId, Req, State) ->
    case eschat_db_chatmembers:get_members(ChatId) of
        {ok, Members} -> 
            success_response(#{members => Members}, Req, State);
        {error, not_found} -> 
            error_response(chat_not_found, Req, State);
        {error, _} -> 
            error_response(operation_failed, Req, State)
    end.

handle_delete_chat(ChatId, UserId, Req, State) ->
    case eschat_db_chats:delete_chat(ChatId, UserId) of
        ok -> 
            success_response(#{}, Req, State);
        {error, not_owner} -> 
            error_response(not_owner, Req, State);
        {error, not_found} -> 
            error_response(chat_not_found, Req, State);
        {error, _} -> 
            error_response(operation_failed, Req, State)
    end.

handle_add_member(ChatId, UserId, #{<<"login">> := UserLogin}, Req, State) ->
    case eschat_db_chats:add_member(ChatId, UserLogin, UserId) of
        {ok, added} -> 
            success_response(#{}, Req, State);
        {error, not_owner} -> 
            error_response(not_owner, Req, State);
        {error, already_member} -> 
            error_response(already_member, Req, State);
        {error, _} -> 
            error_response(operation_failed, Req, State)
    end;
handle_add_member(_, _, _, Req, State) ->
    error_response(invalid_format, Req, State).

handle_remove_member(ChatId, UserId, #{<<"user_id">> := MemberId}, Req, State) ->
    case eschat_db_chats:remove_member(ChatId, MemberId, UserId) of
        ok -> 
            success_response(#{}, Req, State);
        {error, not_owner} -> 
            error_response(not_owner, Req, State);
        {error, not_found} -> 
            error_response(member_not_found, Req, State);
        {error, _} -> 
            error_response(operation_failed, Req, State)
    end;
handle_remove_member(_, _, _, Req, State) ->
    error_response(invalid_format, Req, State).

handle_update_name(ChatId, UserId, #{<<"name">> := NewName}, Req, State) ->
    case eschat_db_chats:update_name(ChatId, NewName, UserId) of
        ok -> 
            success_response(#{}, Req, State);
        {error, not_owner} -> 
            error_response(not_owner, Req, State);
        {error, not_found} -> 
            error_response(chat_not_found, Req, State);
        {error, _} -> 
            error_response(operation_failed, Req, State)
    end;
handle_update_name(_, _, _, Req, State) ->
    error_response(invalid_format, Req, State).

handle_last_read(<<"GET">>, ChatId, UserId, Req, State) ->
    case eschat_db_messages:get_last_read(ChatId, UserId) of
        {ok, MessageId} -> 
            success_response(#{message_id => MessageId}, Req, State);
        {error, not_found} -> 
            error_response(no_last_read, Req, State);
        {error, _} -> 
            error_response(operation_failed, Req, State)
    end;

handle_last_read(<<"PUT">>, ChatId, UserId, Req, State) ->
    with_body(fun(Body, Req1) ->
        case Body of
            #{<<"message_id">> := MessageId} ->
                case eschat_db_messages:update_last_read(ChatId, UserId, MessageId) of
                    ok -> 
                        success_response(#{}, Req1, State);
                    {error, _} -> 
                        error_response(operation_failed, Req1, State)
                end;
            _ -> 
                error_response(invalid_format, Req1, State)
        end
    end, Req).

%% Middleware
with_auth(Handler, Req, State) ->
    case check_auth(Req) of
        {ok, UserId} -> Handler(UserId, Req, State);
        {error, Reason} -> error_response(Reason, Req, State)
    end.

with_auth_and_body(Handler, Req, State) ->
    with_auth(fun(UserId, Req1, State1) ->
        with_body(fun(Body, Req2) ->
            Handler(UserId, Body, Req2, State1)
        end, Req1)
    end, Req, State).

with_body(Handler, Req) ->
    case read_body(Req) of
        {ok, Body, Req1} -> Handler(Body, Req1);
        {error, Reason} -> error_response(Reason, Req, undefined)
    end.

%% Helpers
read_body(Req) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, Req1} ->
            case eschat_json:decode(Data) of
                Map when is_map(Map) -> {ok, Map, Req1};
                _ -> {error, invalid_format}
            end;
        Error -> Error
    end.

check_auth(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined -> 
            {error, no_session};
        SessionId ->
            case eschat_db_sessions:get_session_by_id(SessionId) of
                {ok, UserId} -> {ok, UserId};
                {error, _} -> {error, invalid_session}
            end
    end.

%% Response formatters
success_response(Data, Req, State) ->
    reply(200, Data#{status => <<"success">>}, Req, State).

error_response(no_session, Req, State) ->
    reply(401, #{status => <<"error">>, message => <<"No session provided">>}, Req, State);
error_response(invalid_session, Req, State) ->
    reply(401, #{status => <<"error">>, message => <<"Invalid session">>}, Req, State);
error_response(invalid_format, Req, State) ->
    reply(400, #{status => <<"error">>, message => <<"Invalid request format">>}, Req, State);
error_response(not_owner, Req, State) ->
    reply(403, #{status => <<"error">>, message => <<"Not chat owner">>}, Req, State);
error_response(chat_not_found, Req, State) ->
    reply(404, #{status => <<"error">>, message => <<"Chat not found">>}, Req, State);
error_response(member_not_found, Req, State) ->
    reply(404, #{status => <<"error">>, message => <<"Member not found">>}, Req, State);
error_response(already_member, Req, State) ->
    reply(409, #{status => <<"error">>, message => <<"User already in chat">>}, Req, State);
error_response(no_last_read, Req, State) ->
    reply(404, #{status => <<"error">>, message => <<"No last read message">>}, Req, State);
error_response(_, Req, State) ->
    reply(500, #{status => <<"error">>, message => <<"Operation failed">>}, Req, State).

reply(Status, Response, Req, State) ->
    Req1 = cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, eschat_json:encode(Response), Req),
    {ok, Req1, State}.