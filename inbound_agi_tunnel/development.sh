#!/bin/bash
cd `dirname $0`
erl -noshell -pa ebin -s inbound_agi_tunnel start -s init stop
