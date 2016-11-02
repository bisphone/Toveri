-module(toveri_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([new_buf/2]).
-export([delete_buf/1]).
-export([init/1]).

-include("toveri.hrl").

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

new_buf(BufName, Len) ->
    SupName = sup_name(BufName),
    ChildSpec = {SupName,
                 {toveri_buffer_sup, start_link, [SupName, BufName, Len]},
                 transient, 100, supervisor, [toveri_buffer_sup]},
    supervisor:start_child(?MODULE, ChildSpec).

delete_buf(BufName) ->
    SupName = sup_name(BufName),
    supervisor:terminate_child(?MODULE, SupName).

%% -----------------------------------------------------------------------------
%% supervisor callback
%% -----------------------------------------------------------------------------
init([]) ->
    ets:new(?ETS_TAB, [bag, named_table, public,
                       {keypos, #ringbuf.name},
                       {write_concurrency, true}]),

    {ok, {{one_for_one, 1, 1}, []}}.

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
sup_name(Name) ->
    list_to_atom("toveri_buffer_" ++ atom_to_list(Name) ++ "_sup").
