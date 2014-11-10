%%---------------------------------------------
%% @Module	:	gs_tcp_listener
%% @Description	:	监听进程
%%---------------------------------------------
-module(gs_tcp_listener).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-define(SOCKET_OPTIONS, [binary, {backlog, 20}, {packet, 4}, {active, false}, {keepalive, true},{reuseaddr, true}, {send_timeout, 5000}, {nodelay, false}, {delay_send, true}, {exit_on_close, true}]).
-define(MAX_COUNT, 30).

start_link(Port) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
	process_flag(trap_exit, true),
	case gen_tcp:listen(Port, ?SOCKET_OPTIONS) of
		{ok, LSocket} ->
			lists:foreach(fun(_) ->
					supervisor:start_child(gs_tcp_acceptor_sup, [LSocket])
				end, lists:duplicate(?MAX_COUNT, accept)
			),
			{ok, LSocket};
		{error, Reason} ->
			{stop, {cannot_listen, Reason}}
	end.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, LSocket) ->
	gen_tcp:close(LSocket),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.