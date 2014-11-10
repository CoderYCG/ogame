#!/bin/bash

cd ../config
erl +P 1024000 +K true -smp disable -name node1@127.0.0.1 -setcookie gs -boot start_sasl -config gs -pa ../ebin -s gs start -extra 127.0.0.1 2345 1
