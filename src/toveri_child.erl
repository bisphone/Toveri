-module(toveri_child).

-export([start/2]).

%% -----------------------------------------------------------------------------
%% apply MFA, and add the pid to the given ring buffer
%% -----------------------------------------------------------------------------
start(Name, {M, F, A}) ->
    Res = erlang:apply(M, F, A),
    case Res of
        {ok, Pid} ->
            toveri_buffer:insert(Name, Pid);
        _ ->
            ok
    end,
    Res.
