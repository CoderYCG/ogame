%%------------------------------------------------
%% @Module	:	util
%% @Description	:	工具模块
%%------------------------------------------------
-module(util).
-compile(export_all).
-define(DIFF_SECONDS_0000_1900, 62167219200).

%% 获取当前unix时间戳
unixtime() ->
	private_get_unixtime(erlang:universaltime()).

%% 获取指定时间的unix时间戳
unixtime(LocalTime) ->
	[UniversalTime] = calendar:local_time_to_universal_time_dst(LocalTime),
	private_get_unixtime(UniversalTime).

%% 获取当天零点时间戳
unixdate() ->
	{_Date, Time} = erlang:localtime(),
	Ds = calendar:time_to_seconds(Time),
	unixtime() - Ds.

%% 获取指定日期的零点时间戳
unixdate(UnixTime) ->
%	Date = calendar:gregorian_seconds_to_datetime(UnixTime + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})),
%	{_, Time} = calendar:universal_time_to_local_time(Date),
%	Ds = calendar:time_to_seconds(Time),
%	UnixTime - Ds.
	M = UnixTime div 1000000,
	S = UnixTime rem 1000000,
	{_, Time} = calendar:now_to_local_time({M, S, 0}),
	Ds = calendar:time_to_seconds(Time),
	M * 1000000 + S - Ds.

%% 获取当天0点到现在的秒数
get_seconds_from_midnight() ->
	{_, Time} = erlang:localtime(),
	calendar:time_to_seconds(Time).

%% 获取
private_get_unixtime(UniversalTime) ->
	S1 = calendar:datetime_to_gregorian_seconds(UniversalTime),
	S2 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
	S1 - S2.