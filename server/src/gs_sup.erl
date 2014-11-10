%%---------------------------------------------
%% @Module	:	gs_sup
%% @Description	:	根监控树
%%---------------------------------------------
-module(gs_sup).
-behaviour(supervisor).
-export([start_link/1, init/1, start_child/1, start_child/2]).

start_link([]) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Mod) ->
	start_child(Mod, []).

start_child(Mod, Args) ->
	supervisor:start_child(?MODULE, {Mod,
									{Mod, start_link, Args},
									transient,
									100,
									worker,
									[Mod]}).

init([]) ->
	{ok, {{one_for_one, 3, 10}, []}}.