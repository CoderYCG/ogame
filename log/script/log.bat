cd ../config
erl -name node@127.0.0.1 -boot start_sasl -config log -pa ../ebin -s log start
pause