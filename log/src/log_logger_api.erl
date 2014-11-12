%%-----------------------------------------
%% @Module	:	log_logger_api
%% @Description	:	日志模块对外接口
%%-----------------------------------------
-module(log_logger_api).
-export([
	dev_msg/4,
	debug_msg/4,
	info_msg/4,
	warning_msg/4,
	error_msg/4,
	critical_msg/4
]).

-define(LOG_LEVEL, [{0, no_log}, {1, critical}, {2, error}, {3, warning}, {4, info}, {5, debug}, {6, dev}]).

dev_msg(File, Line, Format, Args) ->
	Level = private_get_level(),
	if
		Level >= 6 ->
			private_notify(info_msg, "DEV(~p:~p) : " ++ Format ++ "\n", [File, Line] ++ Args);
		true ->
			ok
	end.

debug_msg(File, Line, Format, Args) ->
	Level = private_get_level(),
	if
		Level >= 5 ->
			private_notify(info_msg, "DEBUG(~p:~p) : " ++ Format ++ "\n", [File, Line] ++ Args);
		true ->
			ok
	end.

info_msg(File, Line, Format, Args) ->
	Level = private_get_level(),
	if
		Level >= 4 ->
			private_notify(info_msg, "INFO(~p:~p) : " ++ Format ++ "\n", [File, Line] ++ Args);
		true ->
			ok
	end.

warning_msg(File, Line, Format, Args) ->
	Level = private_get_level(),
	if
		Level >= 3 ->
			private_notify(info_msg, "WARNING(~p:~p) : " ++ Format ++ "\n", [File, Line] ++ Args);
		true ->
			ok
	end.

error_msg(File, Line, Format, Args) ->
	Level = private_get_level(),
	if
		Level >= 2 ->
			private_notify(error, "ERROR(~p:~p) : " ++ Format ++ "\n", [File, Line] ++ Args);
		true ->
			ok
	end.

critical_msg(File, Line, Format, Args) ->
	Level = private_get_level(),
	if
		Level >= 1 ->
			private_notify(error, "CRITICAL(~p:~p) : " ++ Format ++ "\n", [File, Line] ++ Args);
		true ->
			ok
	end.


%%----------------------------------------------------------
%% private function
%%----------------------------------------------------------
%% 将日志信息发送到error logger
private_notify(Type, Format, Args) ->
	Msg = {Type, group_leader(), {self(), Format, Args}},
	log_logger:notify(Msg).

%% 获取配置的日志等级, 默认等级为error
private_get_level() ->
	case application:get_env(log, error_type) of
		{ok, Type} ->
			case lists:keyfind(Type, 2, ?LOG_LEVEL) of
				{Level, Type} ->
					Level;
				false ->
					2
			end;
		undefined ->
			2
	end.