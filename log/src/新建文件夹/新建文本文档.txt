%%---------------------------------------------------
%% @Module	:	test
%% @Description	:	测试
%%---------------------------------------------------
-module(test).
-compile(export_all).
-include("log.hrl").

test_dev() ->
	?DEV_MSG("test dev", []).

test_debug() ->
	?DEBUG_MSG("test debug", []).

test_info() ->
	?INFO_MSG("test info", []).

test_warning() ->
	?WARNING_MSG("test warning", []).

test_error() ->
	?ERROR_MSG("test error", []).

test_critical() ->
	?CRITICAL_MSG("test critical", []).