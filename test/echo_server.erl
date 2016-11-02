-module(echo_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([echo/2]).
-export([stop/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

echo(Pid, X) ->
    gen_server:call(Pid, {echo, X}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

init([]) ->
    {ok, undefined}.

handle_call({echo, X}, _From, State) ->
    {reply, X, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, shutdown, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
