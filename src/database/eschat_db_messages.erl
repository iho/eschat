-module(eschat_db_messages).

-export([create_message/3, get_last_read/2, set_last_read/3]).

create_message(ChatId, UserId, Text) ->
    Fun = fun(Pid) ->
             Query =
                 "INSERT INTO chat_messages (chat_id, author_id, message, created_at) "
                 "VALUES ($1, $2, $3, now()) RETURNING id",
             case epgsql:equery(Pid, Query, [ChatId, UserId, Text]) of
                 {ok, 1, _, [{Id}]} -> {ok, Id};
                 Error -> Error
             end
          end,

    case sherlock:transaction(database, Fun) of
        {ok, Id} ->
            {ok, Id};
        {error, _Type} = Err ->
            Err
    end.

set_last_read(ChatId, UserId, MessageId) ->
    Fun = fun(Pid) ->
             UpdateQuery =
                 "UPDATE last_read_msg_in_chat SET chat_message_id = $1 WHERE chat_id = $2 AND user_id = $3",
             case epgsql:equery(Pid, UpdateQuery, [MessageId, ChatId, UserId]) of
                 {ok, 0} ->
                     InsertQuery =
                         "INSERT INTO last_read_msg_in_chat (chat_id, user_id, chat_message_id) VALUES ($1, $2, $3)",
                     case epgsql:equery(Pid, InsertQuery, [ChatId, UserId, MessageId]) of
                         {ok, 1} -> ok;
                         Error -> Error
                     end;
                 {ok, _} -> ok;
                 Error -> Error
             end
          end,
    case sherlock:transaction(database, Fun) of
        ok ->
            ok;
        {error, _Type} = Err ->
            Err
    end.

get_last_read(ChatId, UserId) ->
    Fun = fun(Pid) ->
             Query =
                 "SELECT chat_message_id FROM last_read_msg_in_chat WHERE chat_id "
                 "= $1 AND user_id = $2",
             case epgsql:equery(Pid, Query, [ChatId, UserId]) of
                 {ok, _, [{MessageId}]} -> {ok, MessageId};
                 {ok, _, []} -> {error, not_found};
                 Error -> Error
             end
          end,

    case sherlock:transaction(database, Fun) of
        {ok, MessageId} ->
            {ok, MessageId};
        {error, _Type} = Err ->
            Err
    end.
