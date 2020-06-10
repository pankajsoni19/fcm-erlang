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

-spec start_pool_with_api_key(atom(), string()) -> {ok, pid()}.
start_pool_with_api_key(Name, ApiKey) ->
    fcm_sup:start_child(Name, #{fcm_key => ApiKey}).

-spec start_pool_with_json_service_file(atom(), string()) -> {ok, pid()}.
start_pool_with_json_service_file(Name, FilePath) ->
    fcm_sup:start_child(Name, #{service_file => FilePath}).

-spec stop(atom()) -> ok.
stop(Name) ->
    gen_server:stop(Name, normal, 5000).

-spec push(
            atom(),
            binary() | list(binary()),
            list(tuple()) | map(),
            integer()) -> list(tuple()).
push(Name, RegIds, Message, Retry) when is_list(Message) ->
    push(Name, RegIds, maps:from_list(Message), Retry);
push(Name, RegId, Message, Retry) when is_binary(RegId) ->
    push(Name, [RegId], Message, Retry);
push(Name, RegIds, Message, Retry) ->
    gen_server:call(Name, {send, RegIds, Message, Retry}).

%% ========================================================================
%% internal functions
%% ========================================================================
start_link(Name, Key) ->
    gen_server:start_link({local, Name}, ?MODULE, [Key], []).

%% ------------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------------
init([#{fcm_key := ApiKey}]) ->
    process_flag(trap_exit, true),
    GoogleKey = string:concat("key=", ApiKey),
    {ok, #{auth_server_key => GoogleKey}};
init([MapOpts]) ->
    process_flag(trap_exit, true),
    application:ensure_all_started(google_oauth),
    {ok, fcm_api_v1:reload_access_token(MapOpts)}.

terminate(_Reason, _State) ->
    ok.

handle_call({send, RegIds, Message, Retry}, _From, #{auth_server_key := Key} = State) ->
    Reply = fcm_api_legacy:push(RegIds, Message, Key, Retry),
    {reply, Reply, State};

handle_call({send, RegIds, Message, _Retry}, _From, State0) ->
    {ok, Reply, State} = fcm_api_v1:push(RegIds, Message, State0),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh_token, State) ->
    {noreply, fcm_api_v1:reload_access_token(State)};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
