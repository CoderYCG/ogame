%%------------------------------------------------
%% @Module	:	pool
%% @Description	:	参照poolboy实现的进程池
%%------------------------------------------------
-module(pool).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
	start/2,				%% 启动进程池
	start_link/2,			%% 启动进程池
	checkin/2,				%% 释放进程
	checkout/2,				%% 申请分配进程
	checkout/3				%% 申请分配进程
]).

-record(state, {
	supervisor = none,				%% 进程池里进程的监控树
	workers = [],					%% 进程池里空闲进程列表 [pid()]
	waiting = none,					%% 等待分配进程的队列 pid_queue(From) 
	monitors = none,				%% 记录分配了进程的监控Ref  ets:tid()
	size = 5,						%% 进程池默认进程数量
	overflow = 0,					%% 记录进程池进程溢出的数量
	max_overflow = 10				%% 进程池最大可以溢出的进程数量
}).

-define(TIMEOUT, 5000).

%% 启动进程池
%% @param PoolArgs : proplists:proplist()
%%				{name, Name}					指定进程池名字
%%				{worker_module, Mod}			指定工作进程模块
%%				{size, Sizw}					指定进程池进程数量
%%				{max_overflow}					指定进程池可自动扩展的进程数量
%% @param WorkerArgs : 传递给工作进程回调函数init(WorkerArgs)的参数
start(PoolArgs, WorkerArgs) ->
	start_pool(start, PoolArgs, WorkerArgs).

start_link(PoolArgs, WorkerArgs) ->
	start_pool(start_link, PoolArgs, WorkerArgs).

start_pool(StartFun, PoolArgs, WorkerArgs) ->
	case proplists:get_value(name, PoolArgs) of
		undefined ->
			gen_server:StartFun(?MODULE, [PoolArgs, WorkerArgs], []);
		Name ->
			gen_server:StartFun(Name, ?MODULE, [PoolArgs, WorkerArgs], [])
	end.

%% 释放进程
checkin(Pool, Worker) when is_pid(Worker) ->
	gen_server:cast(Pool, {checkin, Worker}).

%% 申请进程
%% @param Block : 是否排队  true是|false否
%% @retrun Pid|full
checkout(Pool, Block) ->
	checkout(Pool, Block, ?TIMEOUT).

checkout(Pool, Block, Timeout) ->
	try
		gen_server:call(Pool, {checkout, Block}, Timeout)
	catch
		_:_ ->
			gen_server:cast(Pool, {cancel_waiting, self()}),
			full
	end.

%%-------------------------------------------------------------
%% callback function
%%-------------------------------------------------------------
init([PoolArgs, WorkerArgs]) ->
	process_flag(trap_exit, true),
	Waiting = queue:new(),
	Monitors = ets:new(monitors, [private]),
	init(PoolArgs, WorkerArgs, #state{waiting = Waiting, monitors = Monitors}).

init([{worker_module, Mod} | Rest], WorkerArgs, State) when is_atom(Mod) ->
	{ok, Sup} = pool_sup:start_link(Mod, WorkerArgs),
	init(Rest, WorkerArgs, State#state{supervisor = Sup});
init([{size, Size} | Rest], WorkerArgs, State) when is_integer(Size) ->
	init(Rest, WorkerArgs, State#state{size = Size});
init([{max_overflow, MaxOverflow} | Rest], WorkerArgs, State) when is_integer(MaxOverflow) ->
	init(Rest, WorkerArgs, State#state{max_overflow = MaxOverflow});
init([_ | Rest], WorkerArgs, State) ->
	init(Rest, WorkerArgs, State);
init([], _WorkerArgs, #state{size = Size, supervisor = Sup} = State) ->
	Workers = prepopulate(Size, Sup),
	{ok, State#state{workers = Workers}}.

%% 从进程池获取一个进程
%% @param Block : 是否排队  true是|false否
handle_call({checkout, Block}, {FromPid, _Tag} = From, State) ->
	#state{
		supervisor = Sup,
		workers = Workers,
		waiting = Waiting,
		monitors = Monitors,
		overflow = Overflow,
		max_overflow = MaxOverflow
	} = State,
	case Workers of
		[Pid | Rest] ->			%% 有空余进程
			Ref = erlang:monitor(process, FromPid),
			true = ets:insert(Monitors, {Pid, Ref}),
			{reply, Pid, State#state{workers = Rest}};
		[] when MaxOverflow > 0, Overflow < MaxOverflow ->			%% 无空余进程, 但有进去扩展空间
			{Pid, Ref} = new_worker(Sup, FromPid),
			true = ets:insert(Monitors, {Pid, Ref}),
			{reply, Pid, State#state{overflow = Overflow + 1}};
		[] when Block =:= false ->			%% 无扩展空间且不排队
			{reply, full, State};
		[] ->		%% 排队
			Ref = erlang:monitor(process, FromPid),
			NewWaiting = queue:in({From, Ref}, Waiting),
			{noreply, State#state{waiting = NewWaiting}}
	end;

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%% 释放进程
handle_cast({checkin, Pid}, #state{monitors = Monitors} = State) ->
	case ets:lookup(Monitors, Pid) of
		[{Pid, Ref}] ->
			true = erlang:demonitor(Ref),
			true = ets:delete(Monitors, Pid),
			NewState = handle_checkin(Pid, State),
			{noreply, NewState};
		[] ->
			{noreply, State}
	end;

%% 取消排队
handle_cast({cancel_waiting, Pid}, State) ->
	Waiting = queue:filter(fun({P, _}, Ref) -> P =/= Pid orelse not(erlang:demonitor(Ref)) end, State#state.waiting),
	{noreply, State#state{waiting = Waiting}};

handle_cast(_Msg, State) ->
	{noreply, State}.

%% 申请进程退出
handle_info({'DOWN', Ref, _, _, _}, State) ->
	case ets:match(State#state.monitors, {'$1', Ref}) of
		[[Pid]] ->
			true = ets:delete(State#state.monitors, Pid),
			NewState = handle_checkin(Pid, State),
			{noreply, NewState};
		[] ->
			Waiting = queue:filter(fun({_, R}) -> R =/= Ref end, State#state.waiting),
			{noreply, State#state{waiting = Waiting}}
	end;

%% 进程池里的进程退出
handle_info({'EXIT', Pid, _Reason}, State) ->
	#state{
		supervisor = Sup,
		workers = Workers,
		monitors = Monitors
	} = State,
	case ets:lookup(Monitors, Pid) of
		[{Pid, Ref}] ->			%% 已分配的进程
			true = erlang:demonitor(Ref),
			true = ets:delete(Monitors, Pid),
			NewState = handle_worker_exit(Pid, State),
			{noreply, NewState};
		[] ->			%% 未分配的空闲进程
			case lists:member(Pid, Workers) of
				true ->
					NewWorkers = [new_worker(Sup) | lists:filter(fun(P) -> P =/= Pid end, Workers)],
					{noreply, State#state{workers = NewWorkers}};
				false ->
					{noreply, State}
			end
	end;

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	true = exit(State#state.supervisor, shutdown),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------
%% private function
%%----------------------------------------------------------
prepopulate(N, _Sup) when N =< 0 ->
	[];
prepopulate(N, Sup) ->
	prepopulate(N, Sup, []).

prepopulate(0, _Sup, Workers) ->
	Workers;
prepopulate(N, Sup, Workers) ->
	prepopulate(N-1, Sup, [new_worker(Sup) | Workers]).

new_worker(Sup) ->
	{ok, Pid} = supervisor:start_child(Sup, []),
	true = link(Pid),
	Pid.

new_worker(Sup, FromPid) ->
	{ok, Pid} = supervisor:start_child(Sup, []),
	Ref = erlang:monitor(process, FromPid),
	{Pid, Ref}.


handle_checkin(Pid, State) ->
	#state{
		supervisor = Sup,
		workers = Workers,
		waiting = Waiting,
		monitors = Monitors,
		overflow = Overflow
	} = State,
	case queue:out(Waiting) of
		{{value, {From, Ref}}, NewWaiting} ->
			true = ets:insert(Monitors, {Pid, Ref}),
			gen_server:reply(From, Pid),
			State#state{waiting = NewWaiting};
		{empty, Empty} when Overflow > 0 ->
			ok = terminate_worker(Sup, Pid),
			State#state{waiting = Empty, overflow = Overflow - 1};
		{empty, Empty} ->
			NewWorkers = [Pid | Workers],
			State#state{waiting = Empty, workers = NewWorkers, overflow = 0}
	end.


terminate_worker(Sup, Pid) ->
	true = unlink(Pid),
	supervisor:terminate_child(Sup, Pid).

handle_worker_exit(Pid, State) ->
	#state{
		supervisor = Sup,
		waiting = Waiting,
		workers = Workers,
		monitors = Monitors,
		overflow = Overflow
	} = State,
	case queue:out(Waiting) of
		{{value, {From, Ref}}, NewWaiting} ->
			NewWorker = new_worker(Sup),
			true = ets:insert(Monitors, {NewWorker, Ref}),
			gen_server:reply(From, NewWorker),
			State#state{waiting = NewWaiting};
		{empty, Empty} when overflow > 0 ->
			State#state{overflow = Overflow - 1, waiting = Empty};
		{empy, Empty} ->
			NewWorkers = [new_worker(Sup) | lists:filter(fun(P) -> P =/= Pid end, Workers)],
			State#state{workers = NewWorkers, waiting = Empty}
	end.