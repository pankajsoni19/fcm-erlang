-module(fcm_app).

-author('pankajsoni19@live.com').

-behaviour(application).

-export([start/2, stop/1]).

-spec start(_, _) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    fcm_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
    ok.
