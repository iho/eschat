%%%-------------------------------------------------------------------
%%% @author student
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2024 1:03 pm
%%%-------------------------------------------------------------------
-module(eschat_user).
-author("student").

-include("eschat_user_h.hrl").

%% API
-export([name/0]).
-export([new/0]).
-export([now/0, traverse_fun/0]).

-export([get_user/1]).
-export([register_user/1]).
%%-export([get_user/2]).
%%
%%
%%get_user(Login, Password) ->
%%  UserRec = #user{login = Login, password = Password},
%%  Fun = fun(ConnPid) ->
%%    QRes = epgsql:equery(ConnPid, <<"SELECT id FROM public.\"User\" WHERE login = $1 AND passwd = $2">>, [Login, Password]),
%%    lager:debug("QRES ~p", [QRes]),
%%    case QRes of
%%      {ok, _, [{Id}|_]} -> UserRec#user{id = Id};
%%      {ok, _, []} -> #user{login = Login};
%%      _ -> #user{login = <<"Db Error">>}
%%    end
%%        end,
%%  sherlock:transaction(database, Fun).


get_user(Struct) ->
  Login = eschat_xpath:get_val(<<"login">>, Struct),
  Password = eschat_xpath:get_val(<<"password">>, Struct),
  case from_cache(Login, Password) of
    {true, #user{id = Id}} ->
      {hit, Id};
    _ ->
      UserRec = #user{login = Login, password = Password},
      Fun = fun(ConnPid) ->
        QRes = epgsql:equery(ConnPid, <<"SELECT id FROM users WHERE login = $1 AND pass = $2">>, [Login, Password]),
        lager:debug("QRES ~p", [QRes]),
        case QRes of
          {ok, _, [{Id}|_]} -> UserRec#user{id = Id};
          {ok, _, []} -> {error, undefined};
          _ -> {error, db_error}
        end
            end,
      case sherlock:transaction(database, Fun) of
        #user{id = Id} = Rec ->
          to_cache(Rec),
          {miss, Id};
        {error, _Type} = Err  ->
          Err
      end
  end.

register_user(Json) -> 
  Password = eschat_xpath:get_val(<<"password">>, Json),
  Login = eschat_xpath:get_val(<<"login">>, Json),
  lager:debug("Login ~p Password ~p", [Login, Password]),
  UserRec = #user{login = Login, password = Password},
  lager:debug("UserRec ~p", [UserRec]), 
  Fun = fun(ConnPid) ->
    QRes = epgsql:equery(ConnPid, <<"INSERT INTO users (login, pass) VALUES ($1, $2) RETURNING id">>, [Login, Password]),
    lager:debug("QRES ~p", [QRes]),
    case QRes of
      {ok, _, _, [{Id}|_]} -> UserRec#user{id = Id};
      {ok, _, _, []} -> #user{login = Login};
      _ -> lager:debug("QRES ~p", [QRes]), {error, db_error} 
    end
  end,

 Res =  sherlock:transaction(database, Fun),
 lager:debug("RES ~p", [Res]),  
 case Res of 
    #user{id = Id} = Rec ->
      to_cache(Rec),
      {ok, Id};
    {error, _Type} = Err  ->
      Err
  end 
.

name() -> ?MODULE.
new() ->
  Name = name(),
  ets:new(Name, [named_table, set, public, {keypos, #user.login}]).

to_cache(Rec) ->
  ets:insert(name(), Rec).

from_cache(Login, Password) ->
  Now = ?NOW_SEC,
  case ets:lookup(name(), Login) of
    [#user{password = Password, ttl = StoredNow} = Rec] when StoredNow >= Now -> {true, Rec};
    _ -> {false, undefined}
  end.

now() -> ?NOW_SEC.

traverse_fun() ->
  fun (Key, Now) ->
    case ets:lookup(name(), Key) of
      [#user{ttl = StoredNow}] when StoredNow >= Now -> ok;
      [#user{}] -> ets:delete(name(), Key), ok;
      [] -> ok
    end
  end.