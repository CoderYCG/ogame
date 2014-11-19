#!/bin/bash

cd ../
rm -f ebin/*
erl -noshell -s make all -s init stop
