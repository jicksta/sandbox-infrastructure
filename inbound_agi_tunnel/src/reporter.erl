% This module should both log to the console and a log file for debugging purposes.

-module(reporter).
-export([adhearsion_connection_timed_out/1, bad_md5/1]).

adhearsion_connection_timed_out(Username) -> ok.

bad_md5(MD5) -> todo.
