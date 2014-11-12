%%---------------------------------------------
%% @Module	:	log_event
%% @Description	:	日志事件处理器
%%---------------------------------------------
-module(log_event).
-behaviour(gen_event).
-export([
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).


%%--------------------------------------------------------
%% callback function
%%--------------------------------------------------------
init([]) ->
	{ok, none}.

handle_event(Event, State) ->
	log_logger:notify(Event),
	{ok, State}.

handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info({'EXIT', _, _}, _State) ->
	remove_handler;
handle_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.