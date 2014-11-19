cd ../
del /Q ebin\*
erl -noshell -s make all -s init stop
pause