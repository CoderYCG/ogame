%%-------------------------------------------------
%% @Module	:	test
%% @Description	:	测试数据库模块
%% 数据库配置请参考emysql.config, 当修改了连接池ID后应在db模块同步修改
%%	CREATE TABLE `user`(
%%		`id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '自增长ID',
%%		`account` char(32) NOT NULL default '[]' COMMENT '用户名',
%%		`passwd` char(32) NOT NULL default '[]' COMMENT '密码',
%%		PRIMARY KEY(`id`),
%%		index account_passwd(`account`, `passwd`)
%%	) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='用户表';
%%-------------------------------------------------
-module(test).
-export([
	test_get_one/0,
	test_get_row/0,
	test_get_all/0,
	test_execute/0,
	test_transaction_ok/0,
	test_transaction_fail/0
]).

test_get_one() ->
	db:get_one(<<"SELECT * FROM user WHERE id = ~p">>, [1]).

test_get_row() ->
	db:get_row(<<"SELECT * FROM user">>).

test_get_all() ->
	db:get_all(<<"SELECT * FROM user">>).

test_execute() ->
	db:execute(<<"INSERT INTO user(account, passwd) VALUES('test', '123456')">>).

test_transaction_ok() ->
	F1 = fun() ->
			db:execute(<<"INSERT INTO user(account, passwd) VALUES('test11', '123456')">>),
			db:execute(<<"INSERT INTO user(account, passwd) VALUES('test12', '123456')">>)
	end,
	db:transaction(F1).

test_transaction_fail() ->
	F2 = fun() ->
			db:execute(<<"INSERT INTO user(account, passwd) VALUES('test13', '123456')">>),
			db:execute(<<"INSERT INTO user(account, passwd) VALUES('test14', '123456')">>),
			throw(failed)
	end,
	db:transaction(F2).