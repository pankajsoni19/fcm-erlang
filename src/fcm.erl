-module(fcm).

-behaviour(gen_server).

-include("logger.hrl").

-export([start_pool_with_api_key/2,
         start_pool_with_json_service_file/2,
         stop/1]).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([push/4]).

-define(SCOPE, <<"https://www.googleapis.com/auth/firebase.messaging">>).
-define(JSX_OPTS, [return_maps, {labels, atom}]).
-define(HTTP_OPTS, [{timeout, 5000}]).
-define(REQ_OPTS, [{full_result, false}, {body_format, binary}]).

start_pool_with_api_key(Name, ApiKey) ->
    fcm_sup:start_child(Name, #{fcm_key => ApiKey}).

start_pool_with_json_service_file(Name, FilePath) ->
    fcm_sup:start_child(Name, #{service_file => FilePath}).

stop(Name) ->
    gen_server:call(Name, stop).

push(Name, RegIds, Message, Retry) ->
    gen_server:cast(Name, {send, RegIds, Message, Retry}).

%% OTP
start_link(Name, Key) ->
    gen_server:start_link({local, Name}, ?MODULE, [Key], []).

init(Opts) when is_list(Opts) ->
    process_flag(trap_exit, true),
    MOpts = maps:from_list(Opts),
    init(MOpts);
init(#{fcm_key := ApiKey}) ->
    GoogleKey = string:concat("key=", ApiKey),
    {ok, #{auth_server_key => GoogleKey}};
init(MapOpts) ->
    application:ensure_all_started(google_oauth),
    {ok, reload_token(MapOpts)}.

terminate(_Reason, _State) ->
    ok.

handle_call({send, RegIds, Message, Retry}, _From, #{auth_server_key := Key} = State) ->
    Reply = do_push_legacy(RegIds, Message, Key, Retry),
    {reply, Reply, State};

handle_call({send, RegIds0, Message, _Retry}, _From,
             #{
                auth_bearer         := AuthKey,
                push_url            := PushUrl
            } = State) ->
    RegIds = clean_reg_ids(RegIds0),
    Reply = do_push_bearer(RegIds, Message, AuthKey, PushUrl, []),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh_token, State) ->
    {noreply, reload_token(State)};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

clean_reg_ids(RegId) when is_binary(RegId) -> [RegId];
clean_reg_ids(RegIds) -> RegIds.

do_push_bearer([], _, _, _, Acc) -> Acc;
do_push_bearer([RegId| RegIds], Body, AuthKey, PushUrl, Acc0) ->
    Res = do_push_bearer1(RegId, Body, AuthKey, PushUrl),
    Acc = [{RegId, Res} | Acc0],
    do_push_bearer(RegIds, Body, AuthKey, PushUrl, Acc).

do_push_bearer1(RegId, MapBody0, AuthKey, PushUrl) ->
    MapBody = MapBody0#{ token   => RegId },

    Body = jsx:encode(MapBody),

    Request = {PushUrl, [{"Authorization", AuthKey}], "application/json; UTF-8", Body},
    case httpc:request(post, Request, ?HTTP_OPTS, ?REQ_OPTS) of
        {ok, {200, Result}} ->
            #{name := Name} = jsx:decode(Result, ?JSX_OPTS),
            MsgId = lists:last(binary:split(Name, <<"/">>, [global, trim_all])),
            {ok, MsgId};
        {ok, {400, Result}} ->
            {error, {bad_token, Result}};
        Error ->
            {error, Error}
    end.

reload_token(#{service_file := ServiceFile} = State) ->
    {ok, Bin} = file:read_file(ServiceFile),
    #{project_id := ProjectId} = jsx:decode(Bin, ?JSX_OPTS),
    {ok, #{access_token := AccessToken}} = google_oauth:get_access_token(ServiceFile, ?SCOPE),
    erlang:send_after(timer:seconds(3540), self(), refresh_token),
    AuthorizationBearer = <<"Bearer ", AccessToken/binary>>,
    PushUrl = iolist_to_binary(["https://fcm.googleapis.com/v1/projects/", ProjectId ,"/messages:send"]),
    State#{
        auth_bearer         => erlang:binary_to_list(AuthorizationBearer),
        push_url            => erlang:binary_to_list(PushUrl)
    }.

do_push_legacy(RegIds, Message, Key, Retry) ->
    ?INFO_MSG("Sending message: ~p to reg ids: ~p retries: ~p.~n", [Message, RegIds, Retry]),
    case fcm_api_legacy:push(RegIds, Message, Key) of
        {ok, GCMResult} ->
            handle_result(GCMResult, RegIds);
        {error, {retry, RetryAfter}} when (Retry > 0)->
            ?INFO_MSG("Received retry-after. Will retry: ~p times~n", [Retry]),
            timer:sleep(RetryAfter * 1000),
            do_push_legacy(RegIds, Message, Key, Retry - 1);
        {error, Reason} ->
            {error, Reason}
    end.

handle_result(GCMResult, RegId) when is_binary(RegId) ->
    handle_result(GCMResult, [RegId]);
handle_result(GCMResult, RegIds) ->
    {_MulticastId, _SuccessesNumber, _FailuresNumber, _CanonicalIdsNumber, Results} = GCMResult,
    lists:map(fun({Result, RegId}) -> {RegId, parse(Result)} end, lists:zip(Results, RegIds)).

parse(Result) ->
    case {
      proplists:get_value(<<"error">>, Result),
      proplists:get_value(<<"message_id">>, Result),
      proplists:get_value(<<"registration_id">>, Result)
     } of
        {Error, undefined, undefined} ->
            Error;
        {undefined, _MessageId, undefined}  ->
            ok;
        {undefined, _MessageId, NewRegId} ->
            {<<"NewRegistrationId">>, NewRegId}
    end.
