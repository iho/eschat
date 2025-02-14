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

handle_request(<<"POST">>, undefined, Req0, State) ->
    case check_auth(Req0) of
        {ok, UserId} ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            case eschat_json:decode(Body) of
                #{<<"name">> := Name} ->
                    case eschat_db_chats:create_chat(Name, UserId) of
                        {ok, ChatId} ->
                            reply(200, #{status => <<"success">>, chat_id => ChatId}, Req1, State);
                        _ ->
                            reply(500, #{status => <<"error">>, message => <<"Failed to create chat">>}, Req1, State)
                    end;
                _ ->
                    reply(400, #{status => <<"error">>, message => <<"Invalid request format">>}, Req1, State)
            end;
        {error, Reason} ->
            reply(401, #{status => <<"error">>, message => Reason}, Req0, State)
    end;

handle_request(<<"POST">>, ChatId, Req0, State) ->
    Action = cowboy_req:binding(action, Req0),
    handle_chat_action(Action, ChatId, Req0, State);

handle_request(<<"GET">>, ChatId, Req0, State) ->
    case check_auth(Req0) of
        {ok, UserId} ->
            case eschat_db_chatmembers:get_members(ChatId) of
                {ok, Members} ->
                    reply(200, #{status => <<"success">>, members => Members}, Req0, State);
                {error, not_found} ->
                    reply(404, #{status => <<"error">>, message => <<"Chat not found">>}, Req0, State);
                {error, _} ->
                    reply(500, #{status => <<"error">>, message => <<"Failed to get members">>}, Req0, State)
            end;
        {error, Reason} ->
            reply(401, #{status => <<"error">>, message => Reason}, Req0, State)
    end;

handle_request(<<"DELETE">>, ChatId, Req0, State) ->
    case check_auth(Req0) of
        {ok, UserId} ->
            case eschat_db_chats:delete_chat(ChatId, UserId) of
                ok ->
                    reply(200, #{status => <<"success">>}, Req0, State);
                {error, not_owner} ->
                    reply(403, #{status => <<"error">>, message => <<"Not chat owner">>}, Req0, State);
                {error, not_found} ->
                    reply(404, #{status => <<"error">>, message => <<"Chat not found">>}, Req0, State);
                {error, _} ->
                    reply(500, #{status => <<"error">>, message => <<"Failed to delete chat">>}, Req0, State)
            end;
        {error, Reason} ->
            reply(401, #{status => <<"error">>, message => Reason}, Req0, State)
    end.

handle_chat_action(<<"add_member">>, ChatId, Req0, State) ->
    case check_auth(Req0) of
        {ok, UserId} ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            case jsx:decode(Body, [return_maps]) of
                #{<<"login">> := UserLogin} ->
                    case eschat_db_chats:add_member(ChatId, UserLogin, UserId) of
                        {ok, added} ->
                            reply(200, #{status => <<"success">>}, Req1, State);
                        {error, not_owner} ->
                            reply(403, #{status => <<"error">>, message => <<"Not chat owner">>}, Req1, State);
                        {error, already_member} ->
                            reply(409, #{status => <<"error">>, message => <<"User already in chat">>}, Req1, State);
                        {error, _} ->
                            reply(500, #{status => <<"error">>, message => <<"Failed to add member">>}, Req1, State)
                    end;
                _ ->
                    reply(400, #{status => <<"error">>, message => <<"Invalid request format">>}, Req1, State)
            end;
        {error, Reason} ->
            reply(401, #{status => <<"error">>, message => Reason}, Req0, State)
    end;

handle_chat_action(<<"remove_member">>, ChatId, Req0, State) ->
    case check_auth(Req0) of
        {ok, UserId} ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            case jsx:decode(Body, [return_maps]) of
                #{<<"user_id">> := MemberId} ->
                    case eschat_db_chats:remove_member(ChatId, MemberId, UserId) of
                        ok ->
                            reply(200, #{status => <<"success">>}, Req1, State);
                        {error, not_owner} ->
                            reply(403, #{status => <<"error">>, message => <<"Not chat owner">>}, Req1, State);
                        {error, not_found} ->
                            reply(404, #{status => <<"error">>, message => <<"Member not found">>}, Req1, State);
                        {error, _} ->
                            reply(500, #{status => <<"error">>, message => <<"Failed to remove member">>}, Req1, State)
                    end;
                _ ->
                    reply(400, #{status => <<"error">>, message => <<"Invalid request format">>}, Req1, State)
            end;
        {error, Reason} ->
            reply(401, #{status => <<"error">>, message => Reason}, Req0, State)
    end;

handle_chat_action(<<"update_name">>, ChatId, Req0, State) ->
    case check_auth(Req0) of
        {ok, UserId} ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            case jsx:decode(Body, [return_maps]) of
                #{<<"name">> := NewName} ->
                    case eschat_db_chats:update_name(ChatId, NewName, UserId) of
                        ok ->
                            reply(200, #{status => <<"success">>}, Req1, State);
                        {error, not_owner} ->
                            reply(403, #{status => <<"error">>, message => <<"Not chat owner">>}, Req1, State);
                        {error, not_found} ->
                            reply(404, #{status => <<"error">>, message => <<"Chat not found">>}, Req1, State);
                        {error, _} ->
                            reply(500, #{status => <<"error">>, message => <<"Failed to update name">>}, Req1, State)
                    end;
                _ ->
                    reply(400, #{status => <<"error">>, message => <<"Invalid request format">>}, Req1, State)
            end;
        {error, Reason} ->
            reply(401, #{status => <<"error">>, message => Reason}, Req0, State)
    end;

handle_chat_action(<<"last_read">>, ChatId, Req0, State) ->
    case check_auth(Req0) of
        {ok, UserId} ->
            Method = cowboy_req:method(Req0),
            handle_last_read(Method, ChatId, UserId, Req0, State);
        {error, Reason} ->
            reply(401, #{status => <<"error">>, message => Reason}, Req0, State)
    end.

handle_last_read(<<"GET">>, ChatId, UserId, Req0, State) ->
    case eschat_db_messages:get_last_read(ChatId, UserId) of
        {ok, MessageId} ->
            reply(200, #{status => <<"success">>, message_id => MessageId}, Req0, State);
        {error, not_found} ->
            reply(404, #{status => <<"error">>, message => <<"No last read message">>}, Req0, State);
        {error, _} ->
            reply(500, #{status => <<"error">>, message => <<"Failed to get last read">>}, Req0, State)
    end;

handle_last_read(<<"PUT">>, ChatId, UserId, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"message_id">> := MessageId} ->
            case eschat_db_messages:update_last_read(ChatId, UserId, MessageId) of
                ok ->
                    reply(200, #{status => <<"success">>}, Req1, State);
                {error, _} ->
                    reply(500, #{status => <<"error">>, message => <<"Failed to update last read">>}, Req1, State)
            end;
        _ ->
            reply(400, #{status => <<"error">>, message => <<"Invalid request format">>}, Req1, State)
    end.

check_auth(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            {error, <<"No session provided">>};
        SessionId ->
            case eschat_db_sessions:get_session_by_id(SessionId) of
                {ok, UserId} -> {ok, UserId};
                {error, _} -> {error, <<"Invalid session">>}
            end
    end.

reply(Status, Response, Req, State) ->
    Req1 = cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, eschat_json:encode(Response), Req),
    {ok, Req1, State}.
