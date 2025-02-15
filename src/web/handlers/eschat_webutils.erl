-module(eschat_webutils).
-export([
    reply/4,
    error_response/3,
    success_response/3
]).

-define(ERROR_RESPONSES, #{
    invalid_request => {400, <<"Invalid request format">>},
    no_session => {401, <<"No session provided">>},
    invalid_session => {401, <<"Invalid session">>},
    invalid_credentials => {401, <<"Invalid credentials">>},
    user_exists => {409, <<"User already exists">>},
    not_found => {404, <<"Not found">>},
    default => {500, <<"Internal server error">>}
}).

error_response(Error, Req, State) ->
    {Status, Message} = maps:get(Error, ?ERROR_RESPONSES, maps:get(default, ?ERROR_RESPONSES)),
    reply(Status, #{status => <<"error">>, message => Message}, Req, State).

reply(Status, Response, Req, State) ->
    Req1 = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        eschat_json:encode(Response),
        Req),
    {ok, Req1, State}.

success_response(Data, Req, State) ->
    reply(200, Data#{status => <<"success">>}, Req, State).
