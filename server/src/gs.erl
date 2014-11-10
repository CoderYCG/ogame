%%----------------------------------------------
%% @Module	:	gs
%% @Author	:	ycg
%% @Email	:	1050676515@qq.com
%% @Created	:	2014-10-30
%% @Description	:	游戏启动
%%----------------------------------------------
-module(gs).
-export([start/0]).

start() ->
	application:start(gs).

