% This module should both log to the console and a log file for debugging purposes.

-module(reporter).
-export([adhearsion_connection_timed_out/1, bad_md5/1, new_connection_denied/0, connection_requested_for_nonexistent_hash/0,
         incoming_sip_call_without_adhearsion_leg/1, starting_tunnel_loop_with_headers/1, asterisk_agi_initialization_error/1]).

adhearsion_connection_timed_out(Username) ->
    puts("adhearsion_connection_timed_out ~s", Username).

new_connection_denied() ->
    puts("new_connection_denied").

bad_md5(MD5) ->
    puts("bad_md5 ~s", MD5).

incoming_sip_call_without_adhearsion_leg(Username) ->
    puts("incoming_sip_call_without_adhearsion_leg ~s", Username).

connection_requested_for_nonexistent_hash() ->
    puts("connection_requested_for_nonexistent_hash").
    
asterisk_agi_initialization_error(Error) ->
    puts("asterisk_agi_initialization_error ~w", Error).
    
starting_tunnel_loop_with_headers(Headers) ->
    puts("starting_tunnel_loop_with_headers ~w", [Headers]).

puts(String) -> io:format(String ++ "~n").
puts(String, Variables) -> io:format(String ++ "~n", Variables).