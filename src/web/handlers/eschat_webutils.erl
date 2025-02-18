-module(eschat_webutils).

-export([reply/4, error_response/3, success_response/3, read_body/1, validate_session/1,
         with_json_body/3, with_session/3]).

-define(ERROR_RESPONSES,
        #{invalid_request => {400, <<"Invalid request format">>},
          no_session => {401, <<"No session provided">>},
          invalid_session => {401, <<"Invalid session">>},
          invalid_format => {400, <<"Invalid request format">>},
          not_owner => {403, <<"Not owner">>},
          chat_not_found => {404, <<"Chat not found">>},
          member_not_found => {404, <<"Member not found">>},
          already_member => {409, <<"Already a member">>},
          no_last_message => {404, <<"No last message">>},
          no_last_read => {404, <<"No last read">>},
          operation_failed => {500, <<"Operation failed">>},
          invalid_credentials => {401, <<"Invalid credentials">>},
          user_exists => {409, <<"User already exists">>},
          not_found => {404, <<"Not found">>},
          default => {500, <<"Internal server error">>}}).

error_response(Error, Req, State) ->
    {Status, Message} =
        maps:get(Error, ?ERROR_RESPONSES, maps:get(default, ?ERROR_RESPONSES)),
    reply(Status, #{status => <<"error">>, message => Message}, Req, State).

reply(Status, Response, Req, State) ->
    Req1 =
        cowboy_req:reply(Status,
                         #{<<"content-type">> => <<"application/json">>},
                         eschat_json:encode(Response),
                         Req),
    {ok, Req1, State}.

success_response(Data, Req, State) ->
    reply(200, Data#{status => <<"success">>}, Req, State).

read_body(Req) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, Req1} ->
            case eschat_json:decode(Data) of
                Map when is_map(Map) ->
                    {ok, Map, Req1};
                _ ->
                    {error, invalid_format}
            end;
        Error ->
            Error
    end.

validate_session(SessionId) ->
    eschat_db_sessions:get_session_by_id(SessionId).

with_json_body(Next, Req0, State) ->
    case read_body(Req0) of
        {ok, Data, Req1} ->
            Next(Req1#{json_data => Data}, State);
        {error, Reason} ->
            eschat_webutils:error_response(Reason, Req0, State)
    end.

with_session(Next, Req0, State) ->
    case cowboy_req:header(<<"authorization">>, Req0) of
        undefined ->
            eschat_webutils:error_response(no_session, Req0, State);
        SessionId ->
            case validate_session(SessionId) of
                {ok, UserId} ->
                    Next(Req0#{user_id => UserId, session_id => SessionId}, State);
                {error, Reason} ->
                    eschat_webutils:error_response(Reason, Req0, State)
            end
    end.
