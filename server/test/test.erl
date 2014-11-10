-module(test).
-compile(export_all).


start() ->
	{ok,File}=file:open("test.txt",[write]),
	lists:foreach(fun(Num) ->
			spawn(fun() -> test(File, Num, 2345) end),
			receive
				_ -> skip
			after 10 ->
				skip
			end
	end, lists:seq(1, 50000)).

test(File, Num, Port) ->
	case gen_tcp:connect("127.0.0.1", Port, [binary, {packet, 4}, {reuseaddr, true}]) of
		{ok, _} ->
			io:format(File, "~p~n", [Num]),
			receive
				_Any ->
					io:format("Any: ~p~n", [_Any])
			end;
		_ ->
			io:format("error~n")
	end.
