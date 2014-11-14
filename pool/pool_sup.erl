%%-----------------------------------------------------
%% @Module	:	pool_sup
%% @Description	:	monitor the process where in the pool
%%-----------------------------------------------------
-module(pool_sup).
-behaviour(supervisor).
-export([start_link/2, init/1]).

start_link(Mod, Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Mod, Args]).


init([Mod, Args]) ->
	RestartStrateg = {simple_one_for_one, 0, 1},
	ChildSpec =
	[
		{
			Mod,
			{Mod, start_link, [Args]},
			temporary,
			5000,
			worker,
			[Mod]
		}
	],
	{ok, {RestartStrateg, ChildSpec}}.