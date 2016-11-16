-module(toveri).

-export([new/2]).
-export([delete/1]).
-export([add_child/2]).
-export([add_child/3]).
-export([get_size/1]).
-export([get_pid/1]).
-export([get_pid/2]).

-type size() :: non_neg_integer().
-type pos() :: non_neg_integer().

-type err_reason() :: no_such_ring
                    | {empty, pos()}.

-export_type([size/0]).
-export_type([pos/0]).
-export_type([err_reason/0]).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-spec new(atom(), size()) -> {ok, pid()} | {error, {already_started, pid()}}.
new(Name, Size) ->
    toveri_sup:new_buf(Name, Size).

-spec delete(atom()) -> ok.
delete(Name) ->
    toveri_sup:delete_buf(Name).

-spec add_child(atom(), mfa()) -> ok.
add_child(Name, MFA) ->
    add_child(Name, MFA, 1).

-spec add_child(atom(), mfa(), non_neg_integer()) -> ok.
add_child(Name, MFA, Count) ->
    toveri_child_sup:start_children(Name, MFA, Count).

-spec get_size(atom()) -> {ok, size()} | {error, err_reason()}.
get_size(Name) ->
    return(catch toveri_buffer:get_size(Name)).

-spec get_pid(atom()) -> {ok, pid()} | {error, err_reason()}.
get_pid(Name) ->
    return(catch toveri_buffer:read_next(Name)).

-spec get_pid(atom(), pos()) -> {ok, pid()} | {error, err_reason()}.
get_pid(Name, FromPos) ->
    return(catch toveri_buffer:read_pos(Name, FromPos)).

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
return({'EXIT', {noproc, _}}) ->
    {error, no_such_ring};
return(Else) ->
    Else.
