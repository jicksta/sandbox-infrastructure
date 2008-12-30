% This module handles any access to the non-Erlang world

-module(services).
-export([username_for_md5/1]).

username_for_md5(MD5) ->
    SearchResult = os:cmd("username_from_md5.rb " ++ MD5),
    case(SearchResult) of
        "Not found!" -> not_found;
        Username -> {found, Username}
    end.