%%---------------------------------------------
%% @Module	:	log_app
%% @Description	:	打包日志模块
%%---------------------------------------------
-module(log_app).
-behaviour(application).
-export([start/2, stop/1]).


start(normal, []) ->
	{ok, SupPid} = log_sup:start_link([]),
	{ok, _} = log_sup:start_child(log_logger, []),
	gen_event:add_handler(error_logger, log_event, []),
	{ok, SupPid}.

stop(_State) ->
	void.


