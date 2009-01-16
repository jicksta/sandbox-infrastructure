-module(util).
-compile(export_all).

chomp(String) ->
    case(lists:last(String) =:= 10) of
        true -> lists:sublist(String, length(String) - 1);
        false -> String
    end.
