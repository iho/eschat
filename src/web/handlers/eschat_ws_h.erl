-module(eschat_ws_h).

-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

-record(state, {user_id, active_chats = sets:new(), all_chats = sets:new()}).

-include("eschat_user_h.hrl").

-record(chat, {id, name, owner_id, users = sets:new()}).

init(Req, State) ->
    SessionId = cowboy_req:header(<<"authorization">>, Req),
    lager:debug("Session ID: ~p~n", [SessionId]),
    lager:debug("State: ~p~n", [State]),
    case eschat_db_sessions:get_session_by_id(SessionId) of
        {ok, UserId} ->
            % Chats = sets:new(),
            % NewChats = sets:add_element(Element, Set1),
            {cowboy_websocket, Req, #state{user_id = UserId}};
        _ ->
            {ok, cowboy_req:reply(401, Req), undefined}
    end.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Message}, State) ->
    try
        DecodedMsg = eschat_json:decode(Message),
        handle_ws_message(DecodedMsg, State)
    catch
        error ->
            Response = #{type => <<"error">>, message => <<"Invalid JSON">>},
            {reply, {text, eschat_json:encode(Response)}, State}
    end.

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
            lager:debug("Members: ~p~n", [Members]),
            lager:debug("User ID: ~p~n", [State]),
            % Check if user is a member of the chat
            IsMember =
                lists:any(fun(Member) -> maps:get(id, Member, 0) == State#state.user_id end,
                          Members),

            lager:debug("Is member: ~p~n", [IsMember]), 
            case IsMember of
                true ->
                    NewChats = sets:add_element(ChatId, State#state.active_chats),
                    NewState = State#state{active_chats = NewChats},
                    Response = #{type => <<"joined">>, chat_id => ChatId},
                    {reply, {text, eschat_json:encode(Response)}, NewState};
                false ->
                    Response = #{type => <<"error">>, message => <<"Not a member of this chat">>},
                    {reply, {text, eschat_json:encode(Response)}, State}
            end;
        {error, _} ->
            Response = #{type => <<"error">>, message => <<"Failed to get chat members">>},
            {reply, {text, eschat_json:encode(Response)}, State}
    end;
handle_ws_message(#{<<"type">> := <<"send_message">>,
                    <<"chat_id">> := ChatId,
                    <<"message">> := Message},
                  State) ->
    case sets:is_element(ChatId, State#state.active_chats) of
        true ->
            case eschat_db_messages:create_message(ChatId, State#state.user_id, Message) of
                {ok, MsgId} ->
                    Response =
                        #{type => <<"message_sent">>,
                          message_id => MsgId},
                    {reply, {text, eschat_json:encode(Response)}, State};
                _ ->
                    Response = #{type => <<"error">>, message => <<"Failed to send message">>},
                    {reply, {text, eschat_json:encode(Response)}, State}
            end;
        false ->
            Response = #{type => <<"error">>, message => <<"Not joined to this chat">>},
            {reply, {text, eschat_json:encode(Response)}, State}
    end;
handle_ws_message(#{<<"type">> := <<"leave_chat">>, <<"chat_id">> := ChatId}, State) ->
    eschat_db_chats:remove_member(ChatId, State#state.user_id, State#state.user_id),
    NewChats = sets:del_element(ChatId, State#state.active_chats),
    NewState = State#state{active_chats = NewChats},
    Response = #{type => <<"left">>, chat_id => ChatId},
    {reply, {text, eschat_json:encode(Response)}, NewState};
handle_ws_message(_, State) ->
    Response = #{status => <<"error">>, message => <<"Invalid message type">>},
    {reply, {text, eschat_json:encode(Response)}, State}.
