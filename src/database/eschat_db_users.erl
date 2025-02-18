-module(eschat_db_users).

-include("eschat_user_h.hrl").

-export([create_user/2]).
-export([get_user_by_cred/2]).
-export([get_user_by_id/1]).
-export([get_user_by_login/1]).

create_user(Login, Pass) ->
    Fun = fun(Pid) ->
             Query = "INSERT INTO users (login, pass) VALUES ($1, $2) RETURNING id",
             case epgsql:equery(Pid, Query, [Login, Pass]) of
                 {ok, 1, _, [{Id}]} ->
                     #user{id = Id,
                           login = Login,
                           password = Pass};
                 {error, {error, error, _, unique_violation, _, _}} -> {error, user_exists};
                 Error -> Error
             end
          end,

    case sherlock:transaction(database, Fun) of
        #user{id = Id} = Rec ->
            eschat_users_cache:put(Id, #{login => Login, password => Pass}),
            {ok, Id};
        {error, _Type} = Err ->
            Err
    end.

get_user_by_cred(Login, Pass) ->
    Fun = fun(Pid) ->
             Query = "SELECT id FROM users WHERE login = $1 AND pass = $2 limit 1",
             case epgsql:equery(Pid, Query, [Login, Pass]) of
                 {ok, _, [{Id}]} ->
                     #user{id = Id,
                           login = Login,
                           password = Pass};
                 {ok, _, []} -> {error, not_found};
                 Error -> Error
             end
          end,

    case sherlock:transaction(database, Fun) of
        #user{id = Id} = Rec ->
            eschat_users_cache:put(Id, Rec),
            {ok, Id};
        {error, _Type} = Err ->
            Err
    end.

get_user_by_id(UserID) ->
    Fun = fun(Pid) ->
             Query = "SELECT id, login, pass FROM users WHERE id = $1 limit 1",
             case epgsql:equery(Pid, Query, [UserID]) of
                 {ok, _, [{Id, Login, Pass}]} ->
                     #user{id = Id,
                           login = Login,
                           password = Pass};
                 {ok, _, []} -> {error, not_found};
                 Error -> Error
             end
          end,

    case sherlock:transaction(database, Fun) of
        #user{id = Id} = Rec ->
            lager:debug("User Rec: ~p~n", [Rec]),
            eschat_users_cache:put(Id, Rec),
            {ok, Rec};
        {error, _Type} = Err ->
            Err
    end.

get_user_by_login(UserLogin) ->
    Fun = fun(Pid) ->
             Query = "SELECT id, login, pass FROM users WHERE login = $1 limit 1",
             case epgsql:equery(Pid, Query, [UserLogin]) of
                 {ok, _, [{Id, Login, Pass}]} ->
                     Res = #user{id = Id,
                                 login = Login,
                                 password = Pass},
                     lager:debug("User Map: ~p~n", [Res]),
                     Res;
                 {ok, _, []} -> {error, not_found};
                 Error -> Error
             end
          end,

    case sherlock:transaction(database, Fun) of
        Rec ->
            lager:debug("User Rec: ~p~n", [Rec]),
            {ok, Rec};
        {error, _Type} = Err ->
            Err
    end.
