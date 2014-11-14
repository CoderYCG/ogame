%%-------------------------------------
%% @Module	:	worker
%% @Description	:	工作进程
%%-------------------------------------
-module(worker).
-behaviour(pool_worker).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------
%% callback function
%%--------------------------------------------------------

start_link(Args) ->
	gen_server:start_link(?MODULE, [Args], []).

init([_Args]) ->
	io:format("worker ok~n"),
	{ok, []}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.