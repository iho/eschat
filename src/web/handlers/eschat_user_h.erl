%%%-------------------------------------------------------------------
%%% @author student
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2024 11:58 am
%%%-------------------------------------------------------------------
-module(eschat_user_h).

-author("student").

-behavior(cowboy_handler).

-include("eschat_user_h.hrl").

-export([init/2]).

%% "/api/:vsn/user/:action[/:id]"

init(Req0, State) ->
  Vsn = cowboy_req:binding(vsn, Req0),
  Action = cowboy_req:binding(action, Req0),
  Action = cowboy_req:binding(action, Req0),
  Method = cowboy_req:method(Req0),
  handle_request(Method, Vsn, Action, Req0, State).

handle_request(<<"POST">>, <<"v1">>, <<"register">>, Req0, State) ->
  {ok, Body, Req1} = cowboy_req:read_body(Req0),
  Data = eschat_json:decode(Body),
  lager:debug("Data: ~p~n", [Data]),
  case Data of
    #{<<"login">> := Login, <<"password">> := Password} ->
      case eschat_db_users:create_user(Login, Password) of
        {ok, UserId} ->
          {ok, SessionId} = eschat_db_sessions:create_session(UserId, 3600),
          Response =
            #{status => <<"success">>,
              user_id => UserId,
              session_id => SessionId},
          reply(200, Response, Req1, State);
        {error, user_exists} ->
          reply(409, #{status => <<"error">>, message => <<"User already exists">>}, Req1, State);
        {error, _} ->
          reply(500, #{status => <<"error">>, message => <<"Registration failed">>}, Req1, State)
      end;
    _ ->
      reply(400, #{status => <<"error">>, message => <<"Invalid request format">>}, Req1, State)
  end;
handle_request(<<"POST">>, <<"v1">>, <<"login">>, Req0, State) ->
  {ok, Body, Req1} = cowboy_req:read_body(Req0),
  case eschat_json:decode(Body) of
    #{<<"login">> := Login, <<"password">> := Password} ->
      case eschat_db_users:get_user_by_cred(Login, Password) of
        {ok, UserId} ->
          {ok, SessionId} = eschat_db_sessions:create_session(UserId, 3600),
          Response =
            #{status => <<"success">>,
              user_id => UserId,
              session_id => SessionId},
          reply(200, Response, Req1, State);
        {error, invalid_credentials} ->
          reply(401, #{status => <<"error">>, message => <<"Invalid credentials">>}, Req1, State);
        {error, _} ->
          reply(500, #{status => <<"error">>, message => <<"Login failed">>}, Req1, State)
      end;
    _ ->
      reply(400, #{status => <<"error">>, message => <<"Invalid request format">>}, Req1, State)
  end;
handle_request(<<"GET">>, <<"v1">>, <<"sessions">>, Req0, State) ->
  case cowboy_req:header(<<"authorization">>, Req0) of
    undefined ->
      reply(401, #{status => <<"error">>, message => <<"No session provided">>}, Req0, State);
    SessionId ->
      lager:debug("SessionId: ~p~n", [SessionId]),
      case eschat_db_sessions:get_session_by_id(SessionId) of
        {ok, UserId} ->
          {ok, Sessions} = eschat_db_sessions:get_sessions(UserId),
          Response = #{status => <<"success">>, sessions => Sessions},
          reply(200, Response, Req0, State);
        {error, _} ->
          reply(401, #{status => <<"error">>, message => <<"Invalid session">>}, Req0, State)
      end
  end;
handle_request(_, _, _, Req0, State) ->
  reply(404, #{status => <<"error">>, message => <<"Not found">>}, Req0, State).

reply(Status, Response, Req, State) ->
  Req1 =
    cowboy_req:reply(Status,
                     #{<<"content-type">> => <<"application/json">>},
                     eschat_json:encode(Response),
                     Req),
  {ok, Req1, State}.