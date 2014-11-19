%%---------------------------------------------
%% @Module	:	gs_tcp_client
%% @Description	:	玩家套接字消息接收进程
%%---------------------------------------------
-module(gs_tcp_client).
-export([start/1]).

-record(client, {
		pid = none,
		socket = 0
}).

-define(HEART_TIMEOUT, 6000).

start(Socket) ->
	spawn(
			fun() ->
				{ok, Pid} = mod_server:start(),
				loop(#client{pid = Pid, socket = Socket}),
				mod_server:stop(Pid)
			end
		).
%	gen_server:start(?MODULE, [], []).

loop(#client{socket = Socket, pid = Pid} = State) ->
	{ok, Ref} = async_recv(State#client.socket, 0, -1),
	receive
		{inet_async, Socket, Ref, {ok, <<Cmd:16, Bin/binary>>}} ->
			case routing(Cmd, Bin) of
				{ok, Data} ->
					case gen_server:call(Pid, {'SOCKET_EVENT', Cmd, Data}) of
						{ok, _} ->
							loop(State);
						Error ->
							{stop, Error, State}
					end;
				Other ->
					{stop, Other, State}
			end;
		{inet_async, Socket, Ref, {error, timeout}} ->
			{stop, timeout, State};
		{inet_async, _Socket, _Ref, {error, closed}} ->
			{stop, closed, State};
		Any ->
			{stop, Any, State}
	end.


%init([]) ->
%	io:format("start~n"),
%	process_flag(trap_exit, true),
%	{ok, #client{}}.

%handle_call(_Request, _From, State) ->
%	{reply, ok, State}.

%handle_cast({socket, Socket, LSock}, State) ->
%	io:format("socket:~p~n", [Socket]),
%	case set_sockopt(LSock, Socket) of
%		ok -> ok;
%		{error, Reason} -> exit({set_sockopt, Reason})
%	end,
%	{ok, Ref} = async_recv(Socket, 0, ?HEART_TIMEOUT),
%	{ok, Pid} = mod_server:start(),
%	{noreply, State#client{pid = Pid, socket = Socket, ref = Ref}};

%handle_cast(_Msg, State) ->
%	{noreply, State}.

%handle_info({inet_async, Socket, Ref, {ok, <<Cmd:16, Bin/binary>>}}, #client{socket = Socket, ref = Ref} = State) ->
%	case routing(Cmd, Bin) of
%		{ok, Data} ->
%			case gen_server:call(State#client.pid, {'SOCKET_EVENT', Cmd, Data}) of
%				{ok, _} ->
%					{ok, Ref} = async_recv(Socket, 4, ?HEART_TIMEOUT),
%					{noreply, State};
%				_Error ->
%					{stop, normal, State}
%			end;
%		_Other ->
%			{stop, normal, State}
%	end;

%% 超时
%handle_info({inet_async, _Socket, _Ref, {error, timeout}}, State) ->
%	io:format("timeout~n"),
%	{stop, normal, State};

%% 套接字关闭
%handle_info({inet_async, _Socket, _Ref, {error, closed}}, State) ->
%	io:format("close~n"),
%	{stop, normal, State};

%handle_info({'EXIT', _Socket, _Reason}, State) ->
%	io:format("t68~n"),
%	{stop, normal, State};

%handle_info(_Info, State) ->
%	io:format("~p~n", [_Info]),
%	{stop, normal, State}.

%terminate(_Reason, #client{pid = Pid, socket = Socket} = _State) ->
%	gen_tcp:close(Socket),
%	mod_server:stop(Pid),
%	ok.

%code_change(_OldVsn, State, _Extra) ->
%	{ok, State}.

async_recv(S, Length, Timeout) ->
	case prim_inet:async_recv(S, Length, Timeout) of
		{ok, Ref} ->
			{ok, Ref};
		Error ->
			Error
	end.

%% 消息路由
routing(Cmd, Binary) ->
	[H1, H2, H3, _, _] = integer_to_list(Cmd),
	Module = list_to_atom("pt_"++[H1, H2, H3]),
	Module:read(Cmd, Binary).


set_sockopt(LSock, Socket) ->
	true = inet_db:register_socket(Socket, inet_tcp),
	case prim_inet:getopts(LSock, [active, packet, nodelay, keepalive, delay_send, priority, tos]) of
		{ok, Opts} ->
			case prim_inet:setopts(Socket, Opts) of
				ok ->
					ok;
				Error ->
					gen_tcp:close(Socket),
					Error
			end;
		Error ->
			gen_tcp:close(Socket),
			Error
	end.