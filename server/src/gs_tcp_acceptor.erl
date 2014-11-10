%%---------------------------------------------
%% @Module	:	gs_tcp_acceptor
%% @Author	:	ycg
%% @Email	:	1050676515@qq.com
%% @Created	:	2014.10.30
%% @Description	:	连接进程
%%---------------------------------------------
-module(gs_tcp_acceptor).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-record(state, {
		socket = 0,
		ref = 0
}).

start_link(LSocket) ->
	gen_server:start_link(?MODULE, [LSocket], []).


init([LSock]) ->
	process_flag(trap_exit, true),
	gen_server:cast(self(), 'accept'),
	{ok, #state{socket = LSock}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(accept, State) ->
	private_accept(State);

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({inet_async, LSock, Ref, {ok, Socket}}, #state{socket = LSock, ref = Ref} = State) ->
	private_start_client(Socket, LSock),
	private_accept(State);

handle_info({inet_async, LSock, Ref, {error, closed}}, #state{socket = LSock, ref = Ref} = State) ->
	{stop, normal, State};

handle_info(Info, State) ->
	{stop, Info, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

private_accept(#state{socket = LSock} = State) ->
	{ok, Ref} = prim_inet:async_accept(LSock, -1),
	{noreply, State#state{ref = Ref}}.

private_start_client(Socket, LSock) ->
	Child = gs_tcp_client:start(Socket),
	ok = gen_tcp:controlling_process(Socket, Child).
%	gen_server:cast(Child, {socket, Socket, LSock}).
