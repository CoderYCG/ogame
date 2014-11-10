%%---------------------------------------------
%% @Module	:	gs_tcp_client_sup
%% @Description	:	客户端通信进程监控树
%%---------------------------------------------
-module(gs_tcp_client_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	RestartStrateg = {simple_one_for_one, 10, 10},
	ChildSpec = 
	[
		{
			gs_tcp_client,
			{gs_tcp_client, start_link, []},
			transient,
			100,
			worker,
			[gs_tcp_client]
		}
	],
	{ok, {RestartStrateg, ChildSpec}}.