-module(toveri_SUITE).

-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([all/0]).

-export([empty_buffer/1]).
-export([full_buffer/1]).
-export([half_empty_buffer/1]).
-export([insert_n_delete/1]).
-export([supervised_buffer/1]).

-include("src/toveri.hrl").

%% -----------------------------------------------------------------------------
%% common-test callbacks
%% -----------------------------------------------------------------------------
init_per_suite(Config) ->
    ok = application:start(toveri),
    Config.

end_per_suite(_Config) ->
    ok.

all() ->
    [
     empty_buffer,
     full_buffer,
     half_empty_buffer,
     insert_n_delete,
     supervised_buffer
    ].

%% -----------------------------------------------------------------------------
%% test-cases
%% -----------------------------------------------------------------------------
empty_buffer(_Config) ->
    %% make a ring buffer of size `Len
    Ring = empty_buffer,
    Len = 9,
    {ok, _} = toveri_buffer:start_link(Ring, Len),

    [{error, {empty, N}} = toveri_buffer:read_next(Ring) ||
        N <- lists:seq(1, Len)],
    ok.

full_buffer(_Config) ->
    Ring = full_buffer,
    Len = 10,
    {ok, _} = toveri_buffer:start_link(Ring, Len),

    %% spawn echo-servers to be inserted into the buffer
    F = fun() -> {ok, P} = echo_server:start_link(), P end,
    Pids = [F() || _ <- lists:seq(1, Len)],

    %% insert echo-server processes into the buffer
    [ok = toveri_buffer:insert(Ring, Pid) || Pid <- Pids],

    %% read the ring buffer one by one
    [{ok, Pid} = toveri_buffer:read_next(Ring) || Pid <- Pids],
    ok.

half_empty_buffer(_Config) ->
    Ring = half_empty_buffer,
    Len = 10,
    {ok, _} = toveri_buffer:start_link(Ring, Len),

    %% spawn echo-servers to be inserted into the buffer
    F = fun() -> {ok, P} = echo_server:start_link(), P end,
    Pids = [F() || _ <- lists:seq(1, Len div 2)],

    %% insert echo-server processes into the buffer
    [ok = toveri_buffer:insert(Ring, Pid) || Pid <- Pids],

    %% read the half full of the buffer
    [{ok, Pid} = toveri_buffer:read_next(Ring) || Pid <- Pids],

    %% read the half empty of the buffer
    [{error, {empty, _}} = toveri_buffer:read_next(Ring) ||
        _ <- lists:seq(1, Len div 2)],

    %% fill the half empty of the buffer, and read it
    [ok = toveri_buffer:insert(Ring, Pid) || Pid <- Pids],
    [{ok, Pid} = toveri_buffer:read_next(Ring) || Pid <- Pids].

insert_n_delete(_Config) ->
    Ring = insert_n_delete,
    Len = 13,
    {ok, _} = toveri_buffer:start_link(Ring, Len),

    %% insert some objects
    Pid = self(),
    [ok = toveri_buffer:insert(Ring, Pid) || _ <- lists:seq(1, 7)],

    %% delete a few objects
    ok = toveri_buffer:delete_pos(Ring, 1),
    ok = toveri_buffer:delete_pos(Ring, 3),
    ok = toveri_buffer:delete_pos(Ring, 7),

    %% read a few objects
    {ok, Pid} = toveri_buffer:read_pos(Ring, 2),
    {ok, Pid} = toveri_buffer:read_pos(Ring, 4),
    {ok, Pid} = toveri_buffer:read_pos(Ring, 6),
    {error, {empty, 7}} = toveri_buffer:read_pos(Ring, 7),
    ok.

supervised_buffer(_Config) ->
    Ring = supervised_buffer,
    MFA = {echo_server, start_link, []},
    Count = 15,
    {ok, _} = toveri:new(Ring, Count),
    ok = toveri:add_child(Ring, MFA, Count),
    {ok, P1} = toveri:get_pid(Ring),
    abc = echo_server:echo(P1, abc),

    {ok, P2} = toveri:get_pid(Ring),
    abc = echo_server:echo(P2, abc),

    %% make sure two processes are not the same
    true = (P1 =/= P2),

    %% make sure it doesn't return {error, _}
    [{ok, _} = toveri:get_pid(Ring) || _ <- lists:seq(1, Count * 3)],

    %% kill two processes,
    %% and make sure two other processes replace the killed ones
    exit(P1, kill),
    exit(P2, kill),
    Elements = ets:lookup(?ETS_TAB, Ring),
    Count = length(Elements),
    [{ok, _} = toveri:get_pid(Ring) || _ <- lists:seq(1, Count * 3)],

    %% kill the childrend, and delete the ring buffer
    ok = toveri:delete(Ring).
