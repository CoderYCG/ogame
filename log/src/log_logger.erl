%%----------------------------------------------
%% @Module	:	log_logger
%% @Description	:	日志进程模块，用于记录服务器运行时的日志
%%----------------------------------------------
-module(log_logger).
-behaviour(gen_server).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-export([
	start_link/0,
	notify/1
]).

-record(state, {
		log_dir = [],				%% 日志文件目录
		base_name = [],				%% 日志文件基础文件名
		is_mf = 0,					%% 文件名是否每天更新  1是|0否
		fd = []						%% 日志文件名
	}).

%% 消息队列长度
-define(MAX_PENDING_MSG, 100000).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

notify(Event) ->
	gen_server:cast(?MODULE, {event, Event}).

%%-----------------------------------------------
%% callback function
%%-----------------------------------------------
init([]) ->
	{LogDir, BaseFileName, IsMf} = private_get_env(),
	File = make_log_file(LogDir, BaseFileName, IsMf),
	{ok, #state{log_dir = LogDir, base_name = BaseFileName, is_mf = IsMf, fd = File}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({event, Event}, State) ->
	{_, Len} = erlang:process_info(erlang:self(), message_queue_len),
	case Len > ?MAX_PENDING_MSG of
		true ->
			write_event(State#state.fd, {erlang:localtime(), {error, group_leader(), {self(), "mservice log queue too large :~p~n", [Len]}}}),
			do_flush_message(Len);
		false ->
			write_event(State#state.fd, {erlang:localtime(), Event})
	end,
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(auto_update_filename, #state{log_dir = LogDir, base_name = BaseName, is_mf = IsMf} = State) ->
	File = make_log_file(LogDir, BaseName, IsMf),
	{ok, State#state{fd = File}};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------
%% private function
%%--------------------------------------------------
do_write(Fd, Time, Type, Format, Args) ->
	{{Year, Month, Day}, {Hour, Minute, Seconds}} = Time,
	[Type1] = io_lib:format("~s", [Type]),
	Type2 = erlang:iolist_to_binary(Type1),
	Type3 = unicode:characters_to_list(Type2),
	Time2 = io_lib:format("==== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ====\n", [Year, Month, Day, Hour, Minute, Seconds]),
	L1 = lists:concat([Type3, Time2]),
	B = unicode:characters_to_binary(L1),
	file:write_file(Fd, B, [append, delayed_write]),
	try
		M = io_lib:format(Format, Args),
		file:write_file(Fd, M, [append, delayed_write])
	catch _:Error ->
		io:format("log error ~p ~p ~p", [Error, Format, Args])
	end.

write_event(Fd, {Time, {error, _Gleader, {_Pid, Format, Args}}}) ->
	do_write(Fd, Time, "ERROR", Format, Args);


write_event(Fd, {Time, {emulator, _GL, Chars}}) ->
	T = write_time(Time, "ERROR"),
	case catch io_lib:format(Chars, []) of
		S when is_list(S) ->
			file:write_file(Fd, io_lib:format(T ++ S, []), [append, delayed_write]);
		_ ->
			file:write_file(Fd, io_lib:format(T ++ "ERROR: ~p~n", [Chars]), [append, delayed_write])
	end;

write_event(Fd, {Time, {info, _GL, {Pid, Info, _}}}) ->
	T = write_time(Time, "INFO"),
	file:write_file(Fd, io_lib:format(T ++ add_node("~p~n", Pid), [Info]), [append, delayed_write]);

write_event(Fd, {Time, {error_report, _GL, {Pid, std_error, Rep}}}) ->
	T = write_time(Time, "ERROR"),
	S = format_report(Rep),
	file:write_fild(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []), [append, delayed_write]);

write_event(Fd, {Time, {info_report, _GL, {Pid, std_info, Rep}}}) ->
	T = write_time(Time, "INFO REPORT"),
	S = format_report(Rep),
	file:write_file(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []), [append, delayed_write]);

%write_event(Fd, {Time, {info_report, _GL, {Pid, progress, Detail}}}) ->
%	T = write_time(Time, "OTP PROGRESS"),
%	S = format_report(Detail),
%	file:write_file(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []), [append, delayed_write]);

write_event(Fd, {Time, {info_msg, _GL, {_Pid, Format, Args}}}) ->
	do_write(Fd, Time, "INFO MSG", Format, Args);

write_event(Fd, {Time, {warning_report, _GL, {Pid, std_warning, Rep}}}) ->
	T = write_time(Time, "WARNING REPORT"),
	S = format_report(Rep),
	file:write_file(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []), [append, delayed_write]);

write_event(Fd, {Time, {warning_msg, _GL, {_Pid, Format, Args}}}) ->
	do_write(Fd, Time, "WARNING_MSG", Format, Args);

write_event(_, _Info) ->
%	io:format("~s ~p", ["ERROR UNKNOW MESSAGE", Info]),
	ok.


write_time(Time, Type) ->
	{{Year, Month, Day}, {Hour, Minute, Seconds}} = Time,
	io_lib:format("~n~s==== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ====\n", [Type, Year, Month, Day, Hour, Minute, Seconds]).

add_node(X, Pid) when is_atom(X) ->
    add_node(atom_to_list(X), Pid);
add_node(X, Pid) when node(Pid) =/= node() ->
    lists:concat([X,"** at node ",node(Pid)," **~n"]);
add_node(X, _) ->
    X.

format_report(Rep) when is_list(Rep) ->
	case string_p(Rep) of
		true ->
			io_lib:format("~s~n", [Rep]);
		false ->
			format_rep(Rep)
	end;
format_report(Rep) ->
	io_lib:format("~p~n", [Rep]).

format_rep([{Tag, Data}|Rep]) ->
	io_lib:format("    ~p: ~p~n", [Tag, Data]) ++ format_rep(Rep);

format_rep([Other|Rep]) ->
	io_lib:format("    ~p~n", [Other]) ++ format_rep(Rep);

format_rep(_) ->
	[].

string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
    case string_p1(H) of
	true -> string_p1(T);
	_    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.


%% 生成日志文件名
make_log_file(LogDir, BaseFileName, IsMf) ->
	ok = filelib:ensure_dir(LogDir),
	case IsMf of
		true ->
			{Year, Month, Day} = erlang:date(),
			auto_update_filename(),
			io_lib:format("~s/~s_~p_~p_~p.log", [LogDir, BaseFileName, Year, Month, Day]);
		false ->
			io_lib:format("~s/~s.log", [LogDir, BaseFileName])
	end.

%% 更新文件名
auto_update_filename() ->
	TimeOut = (86400 - util:get_seconds_from_midnight()) * 1000,
	erlang:send_after(TimeOut, self(), auto_update_filename).

%% flush进程消息队列
do_flush_message(0) ->
	ok;
do_flush_message(N) ->
	receive
		_R ->
			ok
	after 0 ->
			ok
	end,
	do_flush_message(N-1).

private_get_env() ->
	{ok, LogDir} = case application:get_env(log, logger_mf_dir) of
		{ok, _LogDir} ->
			{ok, _LogDir};
		undefined ->
			{ok, "."}
	end,
	{ok, BaseFileName} = case application:get_env(log, logger_file_base_name) of
		{ok, _BaseFileName} ->
			{ok, _BaseFileName};
		undefined ->
			{ok, "error_logger"}
	end,
	{ok, IsMf} = case application:get_env(log, logger_is_mf) of
		{ok, _IsMf} ->
			{ok, _IsMf};
		undefined ->
			{ok, false}
	end,
	{LogDir, BaseFileName, IsMf}.