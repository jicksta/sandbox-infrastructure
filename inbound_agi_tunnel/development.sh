#!/bin/bash
cd `dirname $0`
# tail -f tmp/development.log &
erl -noshell -pa ebin -s inbound_agi_tunnel start -s init stop
