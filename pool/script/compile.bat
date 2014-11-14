cd ../
del /Q ebin\*
erl -noshell -pa ebin\ -s make all -s init stop
pause