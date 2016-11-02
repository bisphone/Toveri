-module(toveri_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_StartType, _StartArgs) ->
    toveri_sup:start_link().

stop(_State) ->
    ok.
