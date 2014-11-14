%%-------------------------------------------------
%% @Module	:	example
%% @Description	:	进程池使用例子
%%-------------------------------------------------
-module(example).
-export([start/0, checkout/0, checkin/1]).


%% 启动进程池
start() ->
	{ok, _Pid} = pool:start([{name, {local, pool}}, {size, 2}, {max_overflow, 10}, {worker_module, worker}], []).


%% 申请分配进程
checkout() ->
	case pool:checkout(pool, true) of
		full ->
			io:format("The pool is full~n"),
			full;
		Pid ->
			Pid
	end.

%% 释放进程
checkin(Pid) when is_pid(Pid) ->
	pool:checkin(pool, Pid).