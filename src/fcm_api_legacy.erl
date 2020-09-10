-module(fcm_api_legacy).

-include("logger.hrl").

-export([push/4]).

-define(HTTP_OPTIONS, [{body_format, binary}]).
-define(BASEURL, "https://fcm.googleapis.com/fcm/send").
-define(JSX_OPTS, [return_maps, {labels, atom}]).
-define(HEADERS(ApiKey), [{"Authorization", ApiKey}]).
-define(CONTENT_TYPE, "application/json").

-define(HTTP_REQUEST(ApiKey, ReqBody),
        {?BASEURL, ?HEADERS(ApiKey), ?CONTENT_TYPE, ReqBody}).

-spec push(list(binary()), map(), string(), integer()) -> list(tuple()) | {error, term()}.
push(RegIds, Message, Key, Retry) ->
    ?INFO_MSG("Sending message: ~p to reg ids: ~p retries: ~p.~n", [Message, RegIds, Retry]),
    case do_push(RegIds, Message, Key) of
        {ok, GCMResult} ->
            handle_result(GCMResult, RegIds);
        {error, {retry, RetryAfter}} when (Retry > 0)->
            ?INFO_MSG("Received retry-after. Will retry: ~p times~n", [Retry]),
            timer:sleep(RetryAfter * 1000),
            push(RegIds, Message, Key, Retry - 1);
        {error, Reason} ->
            {error, Reason}
    end.

%% ----------------------------------------------------------------------
%% internal
%% ----------------------------------------------------------------------
handle_result(GCMResult, RegId) when is_binary(RegId) ->
    handle_result(GCMResult, [RegId]);
handle_result(GCMResult, RegIds) ->
    Json = jsx:decode(GCMResult, ?JSX_OPTS),
    ?INFO_MSG("fcm_api_legacy: result: ~p~n", [Json]),
    Results = maps:get(results, Json, []),
    lists:map(fun({Result, RegId}) -> {RegId, parse(Result)} end, lists:zip(Results, RegIds)).

parse(#{error := Error}) -> Error;
parse(#{registration_id := NewRegId}) ->
    {<<"NewRegistrationId">>, NewRegId};
parse(_) -> ok.

%% ------------------------------------------------------------
%% internal api
%% ------------------------------------------------------------
append_token(Message, [RegId]) ->
    maps:put(<<"to">>, RegId, Message);
append_token(Message, RegIds) ->
    maps:put(<<"registration_ids">>, RegIds, Message).

do_push(RegIds, MapBody0, ApiKey) ->
    MapBody = append_token(MapBody0, RegIds),
    ReqBody = jsx:encode(MapBody),
    try httpc:request(post, ?HTTP_REQUEST(ApiKey, ReqBody), [], ?HTTP_OPTIONS) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_, 400, _}, _, Body}} ->
            ?ERROR_MSG("Error in request. Reason was: Bad Request - ~p~n", [Body]),
            {error, Body};
        {ok, {{_, 401, _}, _, _}} ->
            ?ERROR_MSG("Error in request. Reason was: authorization error~n", []),
            {error, auth_error};
        {ok, {{_, Code, _}, Headers, _}} when Code >= 500 andalso Code =< 599 ->
            RetryTime = retry_after_from(Headers),
            ?ERROR_MSG("Error in request. Reason was: retry. Will retry in: ~p~n", [RetryTime]),
            {error, {retry, RetryTime}};
        {ok, {{_StatusLine, _, _}, _, _Body}} ->
            ?ERROR_MSG("Error in request. Reason was: timeout~n", []),
            {error, timeout};
        {error, Reason} ->
            ?ERROR_MSG("Error in request. Reason was: ~p~n", [Reason]),
            {error, Reason};
        OtherError ->
            ?ERROR_MSG("Error in request. Reason was: ~p~n", [OtherError]),
            {noreply, unknown}
    catch
        Exception ->
            ?ERROR_MSG("Error in request. Exception ~p while calling URL: ~p~n", [Exception, ?BASEURL]),
            {error, Exception}
    end.

retry_after_from(Headers) ->
    case proplists:get_value("retry-after", Headers) of
        undefined ->
            no_retry;
        RetryTime ->
            case string:to_integer(RetryTime) of
                {Time, _} when is_integer(Time) ->
                    Time;
                {error, no_integer} ->
                   Date = httpd_util:convert_request_date(RetryTime),
                   Now = calendar:universal_time(),
                   calendar:datetime_to_gregorian_seconds(Date) - calendar:datetime_to_gregorian_seconds(Now)
            end
    end.
