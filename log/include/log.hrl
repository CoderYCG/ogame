%%----------------------------------------------------
%% @Module	:	log.hrl
%% @Description	:	日志系统头文件
%%----------------------------------------------------

-define(DEV_MSG(Format, Args), log_logger_api:dev_msg(?FILE, ?LINE, Format, Args)).
-define(DEBUG_MSG(Format, Args), log_logger_api:debug_msg(?FILE, ?LINE, Format, Args)).
-define(INFO_MSG(Format, Args), log_logger_api:info_msg(?FILE, ?LINE, Format, Args)).
-define(WARNING_MSG(Format, Args), log_logger_api:warning_msg(?FILE, ?LINE, Format, Args)).
-define(ERROR_MSG(Format, Args), log_logger_api:error_msg(?FILE, ?LINE, Format, Args)).
-define(CRITICAL_MSG(Format, Args), log_logger_api:critical_msg(?FILE, ?LINE, Format, Args)).