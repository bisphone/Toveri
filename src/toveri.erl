-module(toveri).

-export([new/2]).
-export([add_child/2]).
-export([add_child/3]).
-export([get/1]).
-export([delete/1]).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-spec new(atom(), non_neg_integer()) -> ok.
new(Name, Size) ->
    toveri_sup:new_buf(Name, Size).

-spec add_child(atom(), mfa()) -> ok.
add_child(Name, MFA) ->
    add_child(Name, MFA, 1).

-spec add_child(atom(), mfa(), non_neg_integer()) -> ok.
add_child(Name, MFA, Count) ->
    toveri_child_sup:start_children(Name, MFA, Count).

-spec get(atom()) -> {ok, pid()} | {error, {empty, non_neg_integer()}}.
get(Name) ->
    toveri_buffer:read_next(Name).

-spec delete(atom()) -> ok.
delete(Name) ->
    toveri_sup:delete_buf(Name).
