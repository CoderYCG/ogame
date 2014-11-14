%%------------------------------------------------
%% @Module	:	pool_worker
%% @Description	:	规定工作进程行为
%%------------------------------------------------
-module(pool_worker).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
	[{start_link, 1}];
behaviour_info(_) ->
	undefined.
