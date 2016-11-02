-module(toveri).

-export([new/3]).
-export([get/1]).
-export([delete/1]).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-spec new(atom(), mfa(), non_neg_integer()) -> ok.
new(Name, MFA, Count) ->
    toveri_sup:new_buf(Name, Count),
    toveri_child_sup:start_children(Name, MFA, Count).

-spec get(atom()) -> {ok, pid()} | {error, {empty, non_neg_integer()}}.
get(Name) ->
    toveri_buffer:read_next(Name).

-spec delete(atom()) -> ok.
delete(Name) ->
    toveri_sup:delete_buf(Name).
