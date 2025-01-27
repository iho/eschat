-module(eschat_db_users).

-record(user, {id, login, pass}).

-export([create_user/2]).
-export([
    get_user_by_cred/2
]). 
create_user(Login, Pass) ->
    Fun = fun(ConnPid) ->
             Query = "INSERT INTO users (login, pass) VALUES ($1, $2) RETURNING id",
             case epgsql:equery(ConnPid, Query, [Login, Pass]) of
                 {ok, 1, _, [{Id}]} ->
                     #user{id = Id,
                           login = Login,
                           pass = Pass};
                 {error, {error, error, _, unique_violation, _, _}} -> {error, user_exists};
                 Error -> Error
             end
          end,

    case sherlock:transaction(database, Fun) of
        #user{id = Id} = Rec ->
            eschat_users_cache:put(Id, #{login => Login, pass => Pass}),
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
                           pass = Pass};
                 {ok, _, []} -> {error, not_found};
                 Error -> Error
             end
          end,

    case sherlock:transaction(database, Fun) of
        #user{id = Id} = Rec ->
            eschat_users_cache:put(Id, #{login => Login, pass => Pass}),
            {ok, Id};
        {error, _Type} = Err ->
            Err
    end.
