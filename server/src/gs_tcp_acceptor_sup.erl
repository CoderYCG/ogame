%%---------------------------------------------
%% @Module	:	gs_tcp_acceptor_sup
%% @Email	:	1050676515@qq.com
%% @Created	:	2014.10.30
%% @Description	:	连接进程监控树
%%---------------------------------------------
-module(gs_tcp_acceptor_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	RestartStrateg = {simple_one_for_one, 10, 10},
	ChildSpec =
	[
		{
			gs_tcp_acceptor,
			{gs_tcp_acceptor, start_link, []},
			transient,
			100,
			worker,
			[gs_tcp_acceptor]
		}
	],
	{ok, {RestartStrateg, ChildSpec}}.