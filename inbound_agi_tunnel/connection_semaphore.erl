-module(connection_semaphore).
-export([start/0]).

start() -> loop(dict:new()).

loop(Dictionary) ->
    receive
        {tunnel_waiting, AdhearsionPid, Username} ->
            % TODO: catch the exception if this is not found. We're checking to see if it exists in the dictionary already.
            % If it does, we should raise an error which eventually makes it back to the Adhearsion socket.
            case(dict:is_key(Username, Dictionary)) of
                true  -> AdhearsionPid ! too_many_waiting;
                false -> loop(dict:store(Username, AdhearsionPid, Dictionary))
            end;
        {tunnel_completed, Username} ->
            loop(dict:erase(Username, Dictionary));
        {tunnel_connection_request, AsteriskPid, Username} ->
            % Find the Adhearsion Pid which should be waiting to be connected. If found, return Pid. If not, return an error.
            case(dict:find(Username, Dictionary)) of
                {ok, AdhearsionPid} ->
                    AsteriskPid ! {found, AdhearsionPid};
                error ->
                    AsteriskPid ! no_socket_waiting
            end,
            loop(Dictionary)
    end.
