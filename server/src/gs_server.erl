%%----------------------------------------------
%% @Module	:	gs_server
%% @Author	:	ycg
%% @Email	:	1050676515@qq.com
%% @Created	:	2014-10-30
%% @Description	:	游戏启动
%%----------------------------------------------
-module(gs_server).
-export([start/0]).

start() ->
	[_Ip, Port, _Sid] = init:get_plain_arguments(),
	{ok, _} = start_network(list_to_integer(Port)),
	{ok, _} = start_client().


start_network(Port) ->
	{ok, _} = gs_sup:start_child(gs_tcp_listener_sup, [Port]).

start_client() ->
	{ok, _} = gs_sup:start_child(gs_tcp_client_sup, []).