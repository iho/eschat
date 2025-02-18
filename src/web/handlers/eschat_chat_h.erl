-module(eschat_chat_h).

-behavior(cowboy_handler).

%% API
-export([init/2]).

%% /api/:vsn/chat/:action/:id"
-define(ROUTES,
    #{{<<"v1">>, <<"POST">>, undefined, undefined} =>
        #{handler => fun handle_create_chat/2,
          middlewares => [fun eschat_webutils:with_session/3, fun eschat_webutils:with_json_body/3]},
      
      {<<"v1">>, <<"GET">>, '_', undefined} =>
        #{handler => fun handle_get_members/2,
          middlewares => [fun eschat_webutils:with_session/3]},
      
      {<<"v1">>, <<"DELETE">>, '_', undefined} =>
        #{handler => fun handle_delete_chat/2,
          middlewares => [fun eschat_webutils:with_session/3]},
      
      {<<"v1">>, <<"POST">>, <<"add_member">>, '_'} =>
        #{handler => fun handle_add_member/2,
          middlewares => [fun eschat_webutils:with_session/3, fun eschat_webutils:with_json_body/3]},
      
      {<<"v1">>, <<"POST">>, <<"remove_member">>, '_'} =>
        #{handler => fun handle_remove_member/2,
          middlewares => [fun eschat_webutils:with_session/3, fun eschat_webutils:with_json_body/3]},
      
      {<<"v1">>, <<"POST">>, <<"update_name">>, '_'} =>
        #{handler => fun handle_update_name/2,
          middlewares => [fun eschat_webutils:with_session/3, fun eschat_webutils:with_json_body/3]},
      
      {<<"v1">>, <<"GET">>, <<"last_read">>, '_'} =>
        #{handler => fun handle_last_read_get/2,
          middlewares => [fun eschat_webutils:with_session/3]},
      
      {<<"v1">>, <<"PUT">>, <<"last_read">>, '_'} =>
        #{handler => fun handle_last_read_put/2,
          middlewares => [fun eschat_webutils:with_session/3, fun eschat_webutils:with_json_body/3]}
    }).

init(Req0, State) ->
    Vsn = cowboy_req:binding(vsn, Req0),
    Method = cowboy_req:method(Req0),
    Action = cowboy_req:binding(action, Req0),
    ChatId = cowboy_req:binding(id, Req0),
    handle_request(Vsn, Method, Action, ChatId, Req0, State).

handle_request(Vsn, Method, Action, ChatId, Req, State) ->
    case maps:find({Vsn, Method, Action, ChatId}, ?ROUTES) of
        {ok, #{handler := Handler, middlewares := Middlewares}} ->
            apply_middleware(Middlewares, Handler, Req#{chat_id => ChatId}, State);
        error ->
           eschat_webutils:error_response(not_found, Req, State)
    end.

%% Middleware application
apply_middleware([], Handler, Req, State) ->
    Handler(Req, State);
apply_middleware([Middleware | Rest], Handler, Req, State) ->
    Middleware(
        fun(Req1, State1) -> apply_middleware(Rest, Handler, Req1, State1) end,
        Req,
        State
    ).

%% Handler implementations remain the same as in your original code
handle_create_chat(Req=#{user_id := UserId, json_data := #{<<"name">> := Name}}, State) ->
    lager:debug("Creating chat with name ~p~n", [Name]),
    lager:debug("User ID: ~p~n", [UserId]),
    lager:debug("Request: ~p~n", [Req]),
    case eschat_db_chats:create_chat(Name, UserId) of
        {ok, ChatId} -> 
            eschat_webutils:success_response(#{chat_id => ChatId}, Req, State);
        {error, _} -> 
            eschat_webutils:error_response(create_failed, Req, State)
    end;
handle_create_chat(Req, State) ->
    eschat_webutils:error_response(invalid_format, Req, State).
handle_get_members(Req=#{chat_id := ChatId}, State) ->
    case eschat_db_chatmembers:get_members(ChatId) of
        {ok, Members} ->
            eschat_webutils:success_response(#{members => Members}, Req, State);
        {error, not_found} ->
            eschat_webutils:error_response(chat_not_found, Req, State);
        {error, _} ->
            eschat_webutils:error_response(operation_failed, Req, State)
    end.

handle_delete_chat(Req=#{chat_id := ChatId, user_id := UserId}, State) ->
    case eschat_db_chats:delete_chat(ChatId, UserId) of
        ok ->
           eschat_webutils:success_response(#{}, Req, State);
        {error, not_owner} ->
           eschat_webutils:error_response(not_owner, Req, State);
        {error, not_found} ->
           eschat_webutils:error_response(chat_not_found, Req, State);
        {error, _} ->
           eschat_webutils:error_response(operation_failed, Req, State)
    end.

handle_add_member(Req=#{
    chat_id := ChatId,
    user_id := UserId,
    json_data := #{<<"login">> := UserLogin}
}, State) ->
    case eschat_db_chats:add_member(ChatId, UserLogin, UserId) of
        {ok, added} ->
           eschat_webutils:success_response(#{}, Req, State);
        {error, not_owner} ->
           eschat_webutils:error_response(not_owner, Req, State);
        {error, already_member} ->
           eschat_webutils:error_response(already_member, Req, State);
        {error, _} ->
           eschat_webutils:error_response(operation_failed, Req, State)
    end;
handle_add_member(Req, State) ->
   eschat_webutils:error_response(invalid_format, Req, State).

handle_remove_member(Req=#{
    chat_id := ChatId,
    user_id := UserId,
    json_data := #{<<"user_id">> := MemberId}
}, State) ->
    case eschat_db_chats:remove_member(ChatId, MemberId, UserId) of
        ok ->
           eschat_webutils:success_response(#{}, Req, State);
        {error, not_owner} ->
           eschat_webutils:error_response(not_owner, Req, State);
        {error, not_found} ->
           eschat_webutils:error_response(member_not_found, Req, State);
        {error, _} ->
           eschat_webutils:error_response(operation_failed, Req, State)
    end;
handle_remove_member(Req, State) ->
   eschat_webutils:error_response(invalid_format, Req, State).

handle_update_name(Req=#{
    chat_id := ChatId,
    user_id := UserId,
    json_data := #{<<"name">> := NewName}
}, State) ->
    case eschat_db_chats:update_name(ChatId, NewName, UserId) of
        ok ->
           eschat_webutils:success_response(#{}, Req, State);
        {error, not_owner} ->
           eschat_webutils:error_response(not_owner, Req, State);
        {error, not_found} ->
           eschat_webutils:error_response(chat_not_found, Req, State);
        {error, _} ->
           eschat_webutils:error_response(operation_failed, Req, State)
    end;
handle_update_name(Req, State) ->
   eschat_webutils:error_response(invalid_format, Req, State).

handle_last_read_get(Req=#{chat_id := ChatId, user_id := UserId}, State) ->
    case eschat_db_messages:get_last_read(ChatId, UserId) of
        {ok, MessageId} ->
           eschat_webutils:success_response(#{message_id => MessageId}, Req, State);
        {error, not_found} ->
           eschat_webutils:error_response(no_last_read, Req, State);
        {error, _} ->
           eschat_webutils:error_response(operation_failed, Req, State)
    end.

handle_last_read_put(Req=#{
    chat_id := ChatId,
    user_id := UserId,
    json_data := #{<<"message_id">> := MessageId}
}, State) ->
    case eschat_db_messages:update_last_read(ChatId, UserId, MessageId) of
        ok -> 
           eschat_webutils:success_response(#{}, Req, State);
        {error, _} -> 
           eschat_webutils:error_response(operation_failed, Req, State)
    end;
handle_last_read_put(Req, State) ->
   eschat_webutils:error_response(invalid_format, Req, State).
