-module(toveri_buffer_sup).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
start_link(SupName, BufName, Len) ->
    supervisor:start_link({local, SupName}, ?MODULE, [BufName, Len]).

%% -----------------------------------------------------------------------------
%% supervisor callback
%% -----------------------------------------------------------------------------
init([BufName, Len]) ->
    {ok, {{one_for_all, 5, 10},
          [
           {toveri_buffer,
            {toveri_buffer, start_link, [BufName, Len]},
            transient, 1000, worker, [toveri_buffer]},
           {toveri_child_sup,
            {toveri_child_sup, start_link, [BufName]},
            transient, 1000, supervisor, [toveri_child_sup]}
          ]}}.
