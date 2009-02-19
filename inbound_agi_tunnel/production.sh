#!/bin/bash
RUN_AS_USER=sandbox
cd `dirname $0`
su - $RUN_AS_USER -c "erl $1 -pa /opt/sandbox-infrastructure/inbound_agi_tunnel/ebin -noshell -s inbound_agi_tunnel start /opt/sandbox-infrastructure/inbound_agi_tunnel/envs/production/config.production.erl -s init stop"
