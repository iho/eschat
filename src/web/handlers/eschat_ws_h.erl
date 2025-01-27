-module(eschat_ws_h).
-behaviour(cowboy_websocket).

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

-record(state, {
    user_id,
    active_chats = sets:new()
}).

init(Req, _State) ->
    SessionId = cowboy_req:header(<<"authorization">>, Req),
    case eschat_db_sessions:get_session_by_id(SessionId) of
        {ok, UserId} ->
            {cowboy_websocket, Req, #state{user_id = UserId}};
        _ ->
            {ok, cowboy_req:reply(401, Req), undefined}
    end.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Message}, State) ->
    DecodedMsg = eschat_json:decode(Message),
    handle_ws_message(DecodedMsg, State).

websocket_info({chat_msg, ChatId, Msg}, State) ->
    case sets:is_element(ChatId, State#state.active_chats) of
        true ->
            {reply, {text, eschat_json:encode(Msg)}, State};
        false ->
            {ok, State}
    end;

websocket_info(_, State) ->
    {ok, State}.

handle_ws_message(#{<<"type">> := <<"join_chat">>, <<"chat_id">> := ChatId}, State) ->
    case eschat_db_chats:get_chat_members(ChatId) of
        {ok, Members} ->
            IsMember = lists:keymember(State#state.user_id, 1, Members),
            case IsMember of
                true ->
                    NewChats = sets:add_element(ChatId, State#state.active_chats),
                    NewState = State#state{active_chats = NewChats},
                    Response = #{
                        type => <<"joined">>,
                        chat_id => ChatId
                    },
                    {reply, {text, eschat_json:encode(Response)}, NewState};
                false ->
                    Response = #{
                        type => <<"error">>,
                        message => <<"Not a member of this chat">>
                    },
                    {reply, {text, eschat_json:encode(Response)}, State}
            end;
        {error, _} ->
            Response = #{
                type => <<"error">>,
                message => <<"Failed to get chat members">>
            },
            {reply, {text, eschat_json:encode(Response)}, State}
    end;

handle_ws_message(#{<<"type">> := <<"send_message">>,
                   <<"chat_id">> := ChatId,
                   <<"message">> := Message}, State) ->
    case sets:is_element(ChatId, State#state.active_chats) of
        true ->
            ReplyFor = maps:get(<<"reply_for">>, Message, null),
            case eschat_db:create_message(ChatId, State#state.user_id, Message, ReplyFor) of
                {ok, MsgId, CreatedAt} ->
                    Response = #{
                        type => <<"message_sent">>,
                        message_id => MsgId,
                        created_at => CreatedAt
                    },
                    {reply, {text, eschat_json:encode(Response)}, State};
                _ ->
                    Response = #{
                        type => <<"error">>,
                        message => <<"Failed to send message">>
                    },
                    {reply, {text, eschat_json:encode(Response)}, State}
            end;
        false ->
            Response = #{
                type => <<"error">>,
                message => <<"Not joined to this chat">>
            },
            {reply, {text, eschat_json:encode(Response)}, State}
    end;

handle_ws_message(#{<<"type">> := <<"leave_chat">>, <<"chat_id">> := ChatId}, State) ->
    NewChats = sets:del_element(ChatId, State#state.active_chats),
    NewState = State#state{active_chats = NewChats},
    Response = #{
        type => <<"left">>,
        chat_id => ChatId
    },
    {reply, {text, eschat_json:encode(Response)}, NewState}.