-module(fcm_api_v1).

-include("logger.hrl").

-export([push/3]).
-export([reload_access_token/1]).

-define(SCOPE, <<"https://www.googleapis.com/auth/firebase.messaging">>).
-define(JSX_OPTS, [return_maps, {labels, atom}]).
-define(HTTP_OPTS, [{timeout, 5000}]).
-define(REQ_OPTS, [{full_result, false}, {body_format, binary}]).

-spec push(list(binary()), map(), string(), string()) -> list(tuple()).
push(RegIds, Message, State) ->
    push(RegIds, Message, State, []).

%% ----------------------------------------------------------------------
%% internal
%% ----------------------------------------------------------------------
push([], _, State, Acc) ->
    {ok, Acc, State};
push([RegId| RegIds] = AllRegIds, Body,
        #{
            auth_bearer         := AuthKey,
            push_url            := PushUrl
         } = State0, Acc0) ->
    case do_push(RegId, Body, AuthKey, PushUrl) of
        {ok, MsgId} ->
            Acc = [{RegId, MsgId} | Acc0],
            push(RegIds, Body, State0, Acc);
        {error, refresh_token} ->
            State = reload_access_token(State0),
            push(AllRegIds, Body, State, Acc0);
        Error ->
            Acc = [{RegId, Error} | Acc0],
            push(RegIds, Body, State0, Acc)
    end.

append_token(#{message := Message} = M, Token) ->
    M#{message => Message#{token => Token}};
append_token(#{<<"message">> := Message} = M, Token) ->
    M#{message => Message#{token => Token}};
append_token(Message, Token) ->
    #{message => Message#{token => Token}}.

do_push(RegId, Message0, AuthKey, PushUrl) ->
    MapBody = append_token(Message0, RegId),
    Body = jsx:encode(MapBody),
    Request = {PushUrl, [{"Authorization", AuthKey}], "application/json; UTF-8", Body},
    case httpc:request(post, Request, ?HTTP_OPTS, ?REQ_OPTS) of
        {ok, {200, Result}} ->
            #{name := Name} = jsx:decode(Result, ?JSX_OPTS),
            MsgId = lists:last(binary:split(Name, <<"/">>, [global, trim_all])),
            {ok, MsgId};
        {ok, {401, _}} ->
            {error, refresh_token};
        {ok, {StatusCode, Result} = Err} ->
            ?ERROR_MSG("error sending notification result: ~p~n ~s~n", [StatusCode, Result]),
            {error, Err};
        Error -> Error
    end.

reload_access_token(#{service_file := ServiceFile} = State) ->
    cancel_timer(State),
    {ok, Bin} = file:read_file(ServiceFile),
    #{project_id := ProjectId} = jsx:decode(Bin, ?JSX_OPTS),
    {ok, #{access_token := AccessToken}} = google_oauth:get_access_token(ServiceFile, ?SCOPE),
    AuthorizationBearer = <<"Bearer ", AccessToken/binary>>,
    PushUrl = iolist_to_binary(["https://fcm.googleapis.com/v1/projects/", ProjectId ,"/messages:send"]),
    State#{
        push_url            => erlang:binary_to_list(PushUrl),
        auth_bearer         => erlang:binary_to_list(AuthorizationBearer),
        token_tref          => erlang:send_after(timer:seconds(3540), self(), refresh_token)
    }.

cancel_timer(#{token_tref := TimerRef}) ->
    erlang:cancel_timer(TimerRef);
cancel_timer(_) ->
    ok.
