-module(fcm_sup).

-author('pankajsoni19@live.com').

-behaviour(supervisor).

%% supervisor init
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% function exports
-export([start_child/2]).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
-spec init([]) -> {ok, {{supervisor:strategy(), 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(fcm, worker)]}}.

-spec start_child(atom(), map()) ->
       {'error',_} | {'ok','undefined' | pid()} | {'ok','undefined' | pid(),_}.
start_child(Name, Opts) ->
    supervisor:start_child(?MODULE, [Name, Opts]).
