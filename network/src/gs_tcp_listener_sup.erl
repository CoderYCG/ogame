%%---------------------------------------------
%% @Module	:	gs_tcp_listener_sup
%% @Description	:	监听进程监控树
%%---------------------------------------------
-module(gs_tcp_listener_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(Port) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
	RestartStrateg = {one_for_all, 10, 10},
	ChildSpec =
	[
		{
			gs_tcp_acceptor_sup,
			{gs_tcp_acceptor_sup, start_link, []},
			transient,
			infinity,
			supervisor,
			[gs_tcp_acceptor_sup]
		},
		{
			gs_tcp_listener,
			{gs_tcp_listener, start_link, [Port]},
			transient,
			100,
			worker,
			[gs_tcp_listener]
		}
	],
	{ok, {RestartStrateg, ChildSpec}}.