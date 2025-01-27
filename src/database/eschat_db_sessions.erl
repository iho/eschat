-module(eschat_db_sessions).

-export([create_session/2, get_sessions/1, clean_up_old_sessions/0, get_session_by_id/1]).

-record(session, {id, user_id, active_to}).

create_session(UserId, Duration) ->
    Fun = fun(Pid) ->
             SessionId =
                 base64:encode(
                     crypto:strong_rand_bytes(32)),
             lager:debug("SessionId: ~p~n", [SessionId]),
             ActiveTo =
                 calendar:datetime_to_gregorian_seconds(
                     calendar:universal_time())
                 + Duration,
             lager:debug("ActiveTo: ~p~n", [ActiveTo]),
             Query =
                 "INSERT INTO sessions (id, user_id, active_to) VALUES ($1, $2, "
                 "to_timestamp($3)) RETURNING id",
             case epgsql:equery(Pid, Query, [SessionId, UserId, ActiveTo]) of
                 {ok, 1, _, [{Id}]} ->
                     #session{id = Id,
                              user_id = UserId,
                              active_to = ActiveTo};
                 Error -> Error
             end
          end,

    case sherlock:transaction(database, Fun) of
        #session{id = Id} = Rec ->
            eschat_sessions_cache:put(Id,
                                      #{user_id => UserId,
                                        active_to =>
                                            calendar:gregorian_seconds_to_datetime(Rec#session.active_to)}),
            {ok, Id};
        {error, _Type} = Err ->
            Err
    end.

get_sessions(UserId) ->
    case eschat_sessions_cache:get_all_by_user_id(UserId) of
        {ok, Sessions} ->
            {ok, Sessions};
        not_found ->
            Fun = fun(Pid) ->
                     Query =
                         "SELECT id, active_to FROM sessions WHERE user_id = $1 AND active_to "
                         "> NOW()",
                     case epgsql:equery(Pid, Query, [UserId]) of
                         {ok, _, Rows} ->
                             {ok,
                              [#session{id = Id,
                                        user_id = UserId,
                                        active_to = ActiveTo}
                               || {Id, ActiveTo} <- Rows]};
                         Error -> Error
                     end
                  end,

            case sherlock:transaction(database, Fun) of
                {ok, Sessions} ->
                    {ok, [SessionId || #session{id = SessionId} <- Sessions]};
                {error, _Type} = Err ->
                    Err
            end
    end.

get_session_by_id(SessionId) ->
    case eschat_sessions_cache:get(SessionId) of
        {ok, #{user_id := UserId}} ->
            {ok, UserId};
        not_found ->
            Fun = fun(Pid) ->
                     Query =
                         "SELECT user_id, active_to FROM sessions WHERE id = $1 AND active_to "
                         "> NOW()",
                     case epgsql:equery(Pid, Query, [SessionId]) of
                         {ok, _, [{UserId, ActiveTo}]} ->
                             {ok,
                              #session{id = SessionId,
                                       user_id = UserId,
                                       active_to = ActiveTo}};
                         {ok, _, []} -> {error, session_not_found};
                         Error -> Error
                     end
                  end,

            case sherlock:transaction(database, Fun) of
                {ok, #session{user_id = UserId} = Session} ->
                    eschat_sessions_cache:put(SessionId,
                                              #{user_id => UserId,
                                                active_to => Session#session.active_to}),
                    {ok, UserId};
                {error, _Type} = Err ->
                    Err
            end
    end.

clean_up_old_sessions() ->
    Fun = fun(Pid) ->
             Query = "DELETE FROM sessions WHERE active_to < NOW()",
             case epgsql:equery(Pid, Query, []) of
                 {ok, _, _} -> ok;
                 Error -> Error
             end
          end,

    case sherlock:transaction(database, Fun) of
        ok ->
            ok;
        {error, _Type} = Err ->
            Err
    end.
