-module(toveri_buffer).
-behaviour(gen_server).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([start_link/2]).
-export([stop/1]).
-export([insert/2]).
-export([read_pos/2]).
-export([read_next/1]).
-export([delete_pos/2]).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("toveri.hrl").

-type pos() :: non_neg_integer().

-record(state, {name :: atom(),
                len = 0 :: non_neg_integer(),
                w_pos = 0 :: pos(),
                r_pos = 0 :: pos()}).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-spec start_link(atom(), non_neg_integer()) ->
                        {ok, pid()} | ignore | {error, term()}.
start_link(Name, Len) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Len], []).

-spec stop(atom()) -> ok.
stop(Name) ->
    gen_server:cast(Name, stop).

-spec insert(atom(), pid()) -> ok.
insert(Name, Pid) ->
    gen_server:call(Name, {insert, Pid}).

-spec read_pos(atom(), pos()) -> {ok, pid()} | {error, {empty, pos()}}.
read_pos(Name, Pos) ->
    gen_server:call(Name, {read_pos, Pos}).

-spec read_next(atom()) -> {ok, pid()} | {error, {empty, pos()}}.
read_next(Name) ->
    gen_server:call(Name, read_next).

-spec delete_pos(atom(), pos()) -> ok.
delete_pos(Name, Pos) ->
    gen_server:call(Name, {delete_pos, Pos}).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------
init([Name, Len]) ->
    process_flag(trap_exit, true),
    {ok, #state{name = Name, len = Len}}.

handle_call({insert, Pid}, _From, #state{name=Name}=State) ->
    do_insert(Name, Pid, State),
    NewState = State#state{w_pos = new_w_pos(State)},
    {reply, ok, NewState};
handle_call({read_pos, Pos}, _From, #state{name=Name}=State) ->
    Reply = do_read_pos(Name, Pos),
    {reply, Reply, State};
handle_call(read_next, _From, #state{name=Name}=State) ->
    NewRPos = new_r_pos(State),
    Reply = do_read_pos(Name, NewRPos),
    NewState = State#state{r_pos = NewRPos},
    {reply, Reply, NewState};
handle_call({delete_pos, Pos}, _From, #state{name=Name}=State) ->
    do_delete_pos(Name, Pos),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, shutdown, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _, process, Pid, _}, State) ->
    {ok, Rbuf} = match_pid(State#state.name, Pid),
    true = delete_object(Rbuf),
    {noreply, State#state{w_pos = Rbuf#ringbuf.pos - 1}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    true = ets:delete(?ETS_TAB, State#state.name),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
new_pos(Pos, Len) ->
    (Pos rem Len) + 1.

new_w_pos(#state{w_pos=Pos, len=Len}) ->
    new_pos(Pos, Len).

new_r_pos(#state{r_pos=Pos, len=Len}) ->
    new_pos(Pos, Len).

do_insert(Name, Pid, State) ->
    true = insert(#ringbuf{name = Name, pos = new_w_pos(State), pid = Pid}),
    erlang:monitor(process, Pid).

do_read_pos(Name, Pos) ->
    case match_pos(Name, Pos) of
        {ok, Rb} ->
            {ok, Rb#ringbuf.pid};
        _Else ->
            {error, {empty, Pos}}
    end.

do_delete_pos(Name, Pos) ->
    case match_pos(Name, Pos) of
        {ok, Rbuf} ->
            true = delete_object(Rbuf);
        _ ->
            ok
    end.

insert(Obj) ->
    ets:insert(?ETS_TAB, Obj).

delete_object(Obj) ->
    ets:delete_object(?ETS_TAB, Obj).

match_pos(Name, Pos) ->
    Pattern = #ringbuf{name = Name, pos = Pos, _ = '_'},
    match(Pattern).

match_pid(Name, Pid) ->
    Pattern = #ringbuf{name = Name, pid = Pid, _ = '_'},
    match(Pattern).

match(Pattern) ->
    case ets:match_object(?ETS_TAB, Pattern) of
        [RingBuf] ->
            {ok, RingBuf};
        [] ->
            {error, not_found}
    end.
