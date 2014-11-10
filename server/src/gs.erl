%%----------------------------------------------
%% @Module	:	gs
%% @Description	:	游戏启动
%%----------------------------------------------
-module(gs).
-export([start/0]).

start() ->
	application:start(gs).

