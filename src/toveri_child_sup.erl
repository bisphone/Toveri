-module(toveri_child_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([start_child/2]).
-export([start_children/3]).
-export([init/1]).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
start_link(BufName) ->
    SupName = sup_name(BufName),
    supervisor:start_link({local, SupName}, ?MODULE, [BufName]).

start_child(BufName, MFA) ->
    SupName = sup_name(BufName),
    supervisor:start_child(SupName, [MFA]).

start_children(Name, MFA, Count) ->
    [{ok, _} = start_child(Name, MFA) || _ <- lists:seq(1, Count)],
    ok.

%% -----------------------------------------------------------------------------
%% supervisor callback
%% -----------------------------------------------------------------------------
init([BufName]) ->
    {ok, {{simple_one_for_one, 5, 10},
          [
           {toveri_child, {toveri_child, start, [BufName]},
            transient, 1000, worker, [toveri_child]}
          ]}}.

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
sup_name(Name) ->
    list_to_atom("toveri_buffer_" ++ atom_to_list(Name) ++ "_child_sup").
