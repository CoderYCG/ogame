%%----------------------------------------------
%% @Module	:	gs_app
%% @Author	:	ycg
%% @Email	:	1050676515@qq.com
%% @Created	:	2014.10.31
%% @Description	:	打包程序
%%----------------------------------------------
-module(gs_app).
-behaviour(application).
-export([start/2, stop/1]).


start(normal, []) ->
	{ok, SupPid} = gs_sup:start_link([]),
	gs_server:start(),
	{ok, SupPid}.

stop(_State) ->
	void.