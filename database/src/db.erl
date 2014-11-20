%%-------------------------------------------------------
%% @Module	:	db
%% @Description	:	封装emysql的操作接口
%%-------------------------------------------------------
-module(db).
-export([
	start/0,					%% 启动数据库模块
	get_one/1,					%% 获取首个数据
	get_one/2,
	get_row/1,					%% 获取首行数据
	get_row/2,
	get_all/1,					%% 获取所有数据
	get_all/2,
	execute/1,					%% 执行SQL语句
	execute/2,
	prepare/2,					%% mysql的prepare语法
	transaction/1,				%% 事务操作
	add_pool/2,					%% 增加连接池
	get_one_by_pool/2,			%% 获取首个数据(指定连接池ID)
	get_one_by_pool/3,
	get_row_by_pool/2,			%% 获取首行数据(指定连接池ID)
	get_row_by_pool/3,
	get_all_by_pool/2,			%% 获取所有数据(指定连接池ID)
	get_all_by_pool/3,
	execute_by_pool/2,			%% 执行SQL语句(指定连接池ID)
	execute_by_pool/3,
	transaction_by_pool/2		%% 事务操作(指定连接池ID)
]).
-include("emysql.hrl").
%% 数据库配置请参考emysql.config, 当修改了连接池ID后应同步修改此处
-define(DEFAULT_DB, db_pool).


start() ->
	crypto:start(),
	application:start(emysql).
	%% 可以在程序中添加池，也可通过配置文件初始化连接池
%%	emysql:add_pool(db_pool, [
%%					{size, 5},
%%					{user, "root"},
%%					{password, "123456"},
%%					{database, "gs"},
%%					{encoding, utf8},
%%					{host, "localhost"},
%%					{port, 3306}
%%	]).


%% 增加连接池
%% @param PoolId 连接池ID
%% @param Props 连接池的初始化参数 proplists
%% 				size : 连接池大小
%% 				user : 用户名
%% 				password : 密码
%% 				host : 数据库主机
%% 				port : 端口
%% 				database : 数据库名
%% 				encoding : 建议使用utf8
%% @return {ok, PoolId} | {error, pool_already_exists} | {Error, Reason}
add_pool(PoolId, Props) when is_integer(PoolId), is_list(Props) ->
	Pool = #pool{
		pool_id = PoolId,
		size = proplists:get_value(size, Props, 1),
		user = proplists:get_value(user, Props),
		password = proplists:get_value(password, Props),
		host = proplists:get_value(host, Props),
		port = proplists:get_value(port, Props),
		database = proplists:get_value(database, Props),
		encoding = proplists:get_value(encoding, Props, utf8)
	},
	emysql_conn_mgr:add_pool(Pool).

get_one_by_pool(PoolId, Sql) ->
	case emysql:execute(PoolId, Sql) of
		{result_packet, _, _, [], _} -> null;
		{result_packet, _, _, [[R|_]|_], _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([Sql, Reason]);
		unavailable -> mysql_halt([connect_error, unavailable]);
		{error, pool_not_found} -> mysql_halt([error, pool_not_found])
	end.

get_one_by_pool(PoolId, Sql, Args) when is_list(Args) ->
	case emysql:execute(PoolId, io_lib:format(Sql, Args)) of
		{result_packet, _, _, [], _} -> null;
		{result_packet, _, _, [[R|_]|_], _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([io_lib:format(Sql, Args), Reason]);
		unavailable -> mysql_halt([connect_error, unavailable]);
		{error, pool_not_found} -> mysql_halt([error, pool_not_found])
	end.

%% 获取查询的第一行数据
get_row_by_pool(PoolId, Sql) ->
	case emysql:execute(PoolId, Sql) of
		{result_packet, _, _, [], _} -> [];
		{result_packet, _, _, [R|_], _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([Sql, Reason]);
		unavailable -> mysql_halt([connect_error, unavailable]);
		{error, pool_not_found} -> mysql_halt([error, pool_not_found])
	end.
get_row_by_pool(PoolId, Sql, Args) when is_list(Args) ->
	case emysql:execute(PoolId, io_lib:format(Sql, Args)) of
		{result_packet, _, _, [], _} -> [];
		{result_packet, _, _, [R|_], _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([io_lib:format(Sql, Args), Reason]);
		unavailable -> mysql_halt([connect_error, unavailable]);
		{error, pool_not_found} -> mysql_halt([error, pool_not_found])
	end.

%% 获取查询的所有数据
get_all_by_pool(PoolId, Sql) ->
	case emysql:execute(PoolId, Sql) of
		{result_packet, _, _, R, _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([Sql, Reason]);
		unavailable -> mysql_halt([connect_error, unavailable]);
		{error, pool_not_found} -> mysql_halt([error, pool_not_found])
	end.
get_all_by_pool(PoolId, Sql, Args) when is_list(Args) ->
	case emysql:execute(PoolId, io_lib:format(Sql, Args)) of
		{result_packet, _, _, R, _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([io_lib:format(Sql, Args), Reason]);
		unavailable -> mysql_halt([connect_error, unavailable]);
		{error, pool_not_found} -> mysql_halt([error, pool_not_found])
	end.

%% 执行一条Sql语句, 返回影响的行数
execute_by_pool(PoolId, Sql) ->
	case emysql:execute(PoolId, Sql) of
		{ok_packet, _, Rows, _, _, _, _} -> Rows;
		{result_packet, _, _, R, _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([Sql, Reason]);
		unavailable -> mysql_halt([connect_error, unavailable]);
		{error, pool_not_found} -> mysql_halt([error, pool_not_found])
	end.
execute_by_pool(PoolId, StmtName, Args) when is_atom(StmtName), is_list(Args) ->
	case emsql:execute(PoolId, StmtName, Args) of
		{ok_packet, _, Rows, _, _, _, _} -> Rows;
		{result_packet, _, _, R, _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([StmtName, Reason]);
		unavailable -> mysql_halt([connect_error, unavailable]);
		{error, pool_not_found} -> mysql_halt([error, pool_not_found])
	end;
execute_by_pool(PoolId, Sql, Args) when is_list(Args) ->
	case emysql:execute(PoolId, io_lib:format(Sql, Args)) of
		{ok_packet, _, Rows, _, _, _, _} -> Rows;
		{result_packet, _, _, R, _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([io_lib:format(Sql, Args), Reason]);
		unavailable -> mysql_halt([connect_error, unavailable]);
		{error, pool_not_found} -> mysql_halt([error, pool_not_found])
	end.

%% mysql事务操作
transaction_by_pool(PoolId, F) ->
	case emysql:transaction(PoolId, F) of
		{ok, Val} -> {ok, Val};
		{aborted, {begin_error, ErrorPacket}} -> mysql_halt([begin_error, ErrorPacket]);
		{aborted, {commit_error, ErrorPacket}} -> mysql_halt([commit_error, ErrorPacket]);
		{aborted, Reason} -> Reason;					%% 自定义的错误类型
		Error -> mysql_halt(Error)
	end.


%% 获取查询的第一个数据
get_one(Sql) ->
	case emysql:execute(?DEFAULT_DB, Sql) of
		{result_packet, _, _, [], _} -> null;
		{result_packet, _, _, [[R|_]|_], _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([Sql, Reason]);
		unavailable -> mysql_halt([connect_error, unavailable])
	end.
get_one(Sql, Args) when is_list(Args) ->
	case emysql:execute(?DEFAULT_DB, io_lib:format(Sql, Args)) of
		{result_packet, _, _, [], _} -> null;
		{result_packet, _, _, [[R|_]|_], _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([io_lib:format(Sql, Args), Reason]);
		unavailable -> mysql_halt([connect_error, unavailable])
	end.

%% 获取查询的第一行数据
get_row(Sql) ->
	case emysql:execute(?DEFAULT_DB, Sql) of
		{result_packet, _, _, [], _} -> [];
		{result_packet, _, _, [R|_], _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([Sql, Reason]);
		unavailable -> mysql_halt([connect_error, unavailable])
	end.
get_row(Sql, Args) when is_list(Args) ->
	case emysql:execute(?DEFAULT_DB, io_lib:format(Sql, Args)) of
		{result_packet, _, _, [], _} -> [];
		{result_packet, _, _, [R|_], _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([io_lib:format(Sql, Args), Reason]);
		unavailable -> mysql_halt([connect_error, unavailable])
	end.

%% 获取查询的所有数据
get_all(Sql) ->
	case emysql:execute(?DEFAULT_DB, Sql) of
		{result_packet, _, _, R, _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([Sql, Reason]);
		unavailable -> mysql_halt([connect_error, unavailable])
	end.
get_all(Sql, Args) when is_list(Args) ->
	case emysql:execute(?DEFAULT_DB, io_lib:format(Sql, Args)) of
		{result_packet, _, _, R, _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([io_lib:format(Sql, Args), Reason]);
		unavailable -> mysql_halt([connect_error, unavailable])
	end.

%% 执行一条Sql语句, 返回影响的行数
execute(Sql) ->
	case emysql:execute(?DEFAULT_DB, Sql) of
		{ok_packet, _, Rows, _, _, _, _} -> Rows;
		{result_packet, _, _, R, _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([Sql, Reason]);
		unavailable -> mysql_halt([connect_error, unavailable])
	end.
execute(StmtName, Args) when is_atom(StmtName), is_list(Args) ->
	case emsql:execute(?DEFAULT_DB, StmtName, Args) of
		{ok_packet, _, Rows, _, _, _, _} -> Rows;
		{result_packet, _, _, R, _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([StmtName, Reason]);
		unavailable -> mysql_halt([connect_error, unavailable])
	end;
execute(Sql, Args) when is_list(Args) ->
	case emysql:execute(?DEFAULT_DB, io_lib:format(Sql, Args)) of
		{ok_packet, _, Rows, _, _, _, _} -> Rows;
		{result_packet, _, _, R, _} -> R;
		{error_packet, _, _, _, Reason} -> mysql_halt([io_lib:format(Sql, Args), Reason]);
		unavailable -> mysql_halt([connect_error, unavailable])
	end.

%% mysql的prepare语法
prepare(StmtName, Statement) when is_atom(StmtName) andalso (is_list(Statement) orelse is_binary(Statement)) ->
	ok = emsql:prepare(StmtName, Statement).


%% mysql事务操作
transaction(F) ->
	case emysql:transaction(?DEFAULT_DB, F) of
		{ok, Val} -> {ok, Val};
		{aborted, {begin_error, ErrorPacket}} -> mysql_halt([begin_error, ErrorPacket]);
		{aborted, {commit_error, ErrorPacket}} -> mysql_halt([commit_error, ErrorPacket]);
		{aborted, Reason} -> Reason;					%% 自定义的错误类型
		Error -> mysql_halt(Error)
	end.


%%------------------------------------------------------
%% private function
%%------------------------------------------------------
%% @doc 显示人可以看得懂的错误信息
%% 用catch捕捉得到的错误类型是
%% error:{db_error, [Sql, Reason]}
mysql_halt([Sql, Reason]) ->
    erlang:error({db_error, [Sql, Reason]}).


