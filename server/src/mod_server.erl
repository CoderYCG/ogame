%%---------------------------------------------
%% @Module	:	mod_server
%% @Description	:	玩家进程
%%---------------------------------------------
-module(mod_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, stop/1]).

-record(player, {
		socket = 0
}).

start() ->
	gen_server:start(?MODULE, [], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

init([]) ->
	process_flag(priority, max),
	{ok, #player{}}.

handle_call({'SOCKET_EVENT', Cmd, Data}, _From, State) ->
	case routing(Cmd, State, Data) of
		{ok, NewState} ->
			{reply, {ok, ok}, NewState};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



routing(Cmd, _State, Data) ->
	[_H1, _H2, _H3, _, _] = integer_to_list(Cmd),
	io:format("Data: ~p~n", [Data]).
