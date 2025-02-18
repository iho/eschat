-module(eschat_db_chats).

-include("eschat_chat_h.hrl").
-include("eschat_user_h.hrl").

-export([create_chat/2, add_member/3, remove_member/3, update_name/3, delete_chat/2,
         get_chat_by_id/1, get_user_by_login/1, is_owner/2]).

create_chat(Name, OwnerId) ->
    Fun = fun(Connection) ->
             Query =
                 "INSERT INTO chats (name, created_at, creator_id) VALUES ($1, "
                 "NOW(), $2) RETURNING id",
             Result = epgsql:equery(Connection, Query, [Name, OwnerId]),
             lager:info("Chat creation result: ~p", [Result]),
             case Result of
                 {ok, 1, _, [{ChatId}]} ->
                     MemberQuery =
                         "INSERT INTO chat_members (chat_id, user_id, is_owner) VALUES "
                         "($1, $2, true)",
                     case epgsql:equery(Connection, MemberQuery, [ChatId, OwnerId]) of
                         {ok, 1} ->
                             #chat{id = ChatId,
                                   name = Name,
                                   owner_id = OwnerId};
                         _ -> {error, chat_member_error}
                     end;
                 _ -> {error, chat_creation_error}
             end
          end,

    case sherlock:transaction(database, Fun) of
        #chat{id = ChatId} ->
            eschat_chats_cache:put(ChatId, #{name => Name, owner_id => OwnerId}),
            eschat_chatmembers_cache:put(ChatId, OwnerId, true),
            {ok, ChatId};
        {error, _Type} = Err ->
            Err
    end.

add_member(ChatId, UserLogin, RequesterId) ->

    % Convert ChatId to integer if it's binary
    ChatIdInt =
        case is_binary(ChatId) of
            true ->
                binary_to_integer(ChatId);
            false ->
                ChatId
        end,
    case eschat_db_chats:is_owner(ChatId, RequesterId) of
        true ->
            case eschat_db_users:get_user_by_login(UserLogin) of
                {ok, Rec} ->
                    Fun = fun(Connection) ->
                             UserId = Rec#user.id,
                             MemberQuery =
                                 "INSERT INTO chat_members (chat_id, user_id, is_owner) VALUES "
                                 "($1, $2, false)",
                             case epgsql:equery(Connection, MemberQuery, [ChatIdInt, UserId]) of
                                 {ok, 1} ->
                                    %  eschat_chatmembers_cache:put(ChatId, UserId, false),
                                     {ok, added};
                                 {error, {error, error, _, unique_violation, _, _}} ->
                                     {error, already_member};
                                 Error -> Error
                             end
                          end,
                    sherlock:transaction(database, Fun);
                Error ->
                    Error
            end;
        false ->
            {error, not_owner}
    end.

remove_member(ChatId, UserId, RequesterId) ->
    case eschat_chatmembers_cache:is_owner(ChatId, RequesterId) of
        true ->
            Fun = fun(Connection) ->
                     Query =
                         "DELETE FROM chat_members WHERE chat_id = $1 AND user_id = $2 "
                         "AND NOT is_owner",
                     case epgsql:equery(Connection, Query, [ChatId, UserId]) of
                         {ok, 1} ->
                             eschat_chatmembers_cache:remove_member(ChatId, UserId),
                             ok;
                         {ok, 0} -> {error, not_found};
                         Error -> Error
                     end
                  end,
            sherlock:transaction(database, Fun);
        false ->
            {error, not_owner}
    end.

update_name(ChatId, NewName, RequesterId) ->
    case eschat_chatmembers_cache:is_owner(ChatId, RequesterId) of
        true ->
            Fun = fun(Connection) ->
                     Query = "UPDATE chats SET name = $1 WHERE id = $2",
                     case epgsql:equery(Connection, Query, [NewName, ChatId]) of
                         {ok, 1} ->
                             eschat_chats_cache:update_name(ChatId, NewName),
                             ok;
                         {ok, 0} -> {error, not_found};
                         Error -> Error
                     end
                  end,
            sherlock:transaction(database, Fun);
        false ->
            {error, not_owner}
    end.

delete_chat(ChatId, RequesterId) ->
    Result = is_owner(ChatId, RequesterId),
    lager:debug("is_owner result: ~p", [Result]),
    ChatIdInt =
        case is_binary(ChatId) of
            true ->
                binary_to_integer(ChatId);
            false ->
                ChatId
        end,

    case Result of
        true ->
            Fun = fun(Connection) ->
                     Query = "DELETE FROM chats WHERE id = $1",
                     case epgsql:equery(Connection, Query, [ChatIdInt]) of
                         {ok, 1} ->
                             eschat_chats_cache:remove(ChatIdInt),
                             ok;
                         {ok, 0} -> {error, not_found};
                         Error -> Error
                     end
                  end,
            sherlock:transaction(database, Fun);
        false ->
            {error, not_owner}
    end.

get_chat_by_id(ChatId) ->
    case eschat_chats_cache:get(ChatId) of
        {ok, ChatData} ->
            {ok,
             #chat{id = ChatId,
                   name = maps:get(name, ChatData),
                   owner_id = maps:get(owner_id, ChatData)}};
        not_found ->
            Fun = fun(Connection) ->
                     Query =
                         "SELECT name, user_id FROM chats c \n                       "
                         " JOIN chat_members m ON c.id = m.chat_id \n                "
                         "        WHERE c.id = $1 AND m.is_owner = true",
                     case epgsql:equery(Connection, Query, [ChatId]) of
                         {ok, _, [{Name, OwnerId}]} ->
                             Chat =
                                 #chat{id = ChatId,
                                       name = Name,
                                       owner_id = OwnerId},
                             eschat_chats_cache:put(ChatId, #{name => Name, owner_id => OwnerId}),
                             {ok, Chat};
                         {ok, _, []} -> {error, not_found};
                         Error -> Error
                     end
                  end,
            sherlock:transaction(database, Fun)
    end.

get_user_by_login(Login) ->
    case eschat_users_cache:get_by_login(Login) of
        {ok, UserId} ->
            {ok, UserId};
        not_found ->
            Fun = fun(Connection) ->
                     Query = "SELECT id FROM users WHERE login = $1",
                     case epgsql:equery(Connection, Query, [Login]) of
                         {ok, _, [{Id}]} ->
                             eschat_users_cache:put(Id, #{login => Login}),
                             {ok, Id};
                         {ok, _, []} -> {error, user_not_found};
                         Error -> Error
                     end
                  end,
            sherlock:transaction(database, Fun)
    end.

is_owner(ChatId, UserId) ->
    lager:debug("Checking if ~p is owner of chat ~p", [UserId, ChatId]),

    % Convert ChatId to integer if it's binary
    ChatIdInt =
        case is_binary(ChatId) of
            true ->
                binary_to_integer(ChatId);
            false ->
                ChatId
        end,

    % Convert UserId to integer if it's binary
    UserIdInt =
        case is_binary(UserId) of
            true ->
                binary_to_integer(UserId);
            false ->
                UserId
        end,

    lager:debug("ChatIdInt: ~p", [ChatIdInt]),
    lager:debug("UserIdInt: ~p", [UserIdInt]),

    Fun = fun(Connection) ->
             Query =
                 "SELECT is_owner FROM chat_members WHERE chat_id = $1 AND user_id "
                 "= $2",
             case epgsql:equery(Connection, Query, [ChatIdInt, UserIdInt]) of
                 {ok, _, [{IsOwner}]} -> {ok, IsOwner};
                 {ok, _, []} -> {error, not_found};
                 Error -> Error
             end
          end,

    case sherlock:transaction(database, Fun) of
        {ok, true} ->
            true;
        _ ->
            false
    end.
