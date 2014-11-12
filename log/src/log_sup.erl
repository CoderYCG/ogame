%%------------------------------------------------
%% @Module	:	log_sup
%% @Author	:	ycg
%% @Email	:	1050676515@qq.com
%% @Created	:	2014.11.10
%% @Description	:	日志进程监控树
%%------------------------------------------------
-module(log_sup).
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

%%---------------------------------------
%% callback function
%%---------------------------------------
init([]) ->
	{ok, {{one_for_one, 3, 10}, []}}.