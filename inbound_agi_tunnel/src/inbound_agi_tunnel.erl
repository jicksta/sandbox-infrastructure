-module(inbound_agi_tunnel).
-export([start/0]).

start() ->
	io:format("Starting server~n"),
	{ok, ServerSocket} = gen_tcp:listen(20000, [list, {packet, 0}, {active, true}]),
	spawn_link(fun() -> receive_connection_loop(ServerSocket) end, []),
	ProcessDictionary = spawn(fun() -> process_dictionary() end),
	register(process_dictionary, ProcessDictionary).

process_dictionary() -> process_dictionary(dict:new()).

process_dictionary(Dictionary) ->
    receive
        {tunnel_waiting, Pid, Username} ->
            % TODO: catch the exception if this is not found. We're checking to see if it exists in the dictionary already.
            % If it does, we should raise an error which eventually makes it back to the Adhearsion socket.
            dict:fetch(Username, Dictionary);
            % Next, we should Store the Username and the Pid together...
            % TODO
        {tunnel_closed, Pid, Username} ->
            dict:erase(Username, Dictionary);
        {tunnel_connection_request, Username} ->
            % Check if the Username is in the dictionary. if so, return the Pid. if not, send an error back.
            todo
    end,
    process_dictionary(Dictionary).

receive_connection_loop(ServerSocket) ->
	{ok, Socket} = gen_tcp:accept(ServerSocket),
	handle_connection(self(), Socket),
	receive_connection_loop(ServerSocket).

handle_connection(Parent, FromAdhearsion) ->
    receive
        {tcp, _Socket, InitialData} ->
            case(check_authentication(InitialData)) of
        	    not_allowed -> Parent ! {not_allowed, FromAdhearsion};
        	    not_found   -> Parent ! {not_found,   FromAdhearsion};
        		Username    ->
        		    case(wait_for_agi_leg_or_timeout(Username)) of
        		        timeout ->
        		            reporter:adhearsion_connection_timed_out(Username),
                            gen_tcp:close(FromAdhearsion);
        		        {bridge_legs, FromAsterisk} ->
        		            bridge(FromAdhearsion, FromAsterisk)
        	        end
        	end,
        	handle_connection(Parent, FromAdhearsion);
        Error -> Parent ! {error, todo}
    end.

% Yay! A connection has been made. Let's now start forwarding packets back and forth.
bridge(FromAdhearsion, FromAsterisk) ->
    todo.

wait_for_agi_leg_or_timeout(Username) ->
    % TODO: Register with a global dictionary that 
    ProcessDictionaryPid = whereis(process_dictionary),
    ProcessDictionaryPid ! {tunnel_waiting, self(), Username},
    receive
        {bridge_request, FromAsterisk} -> {bridge_legs, FromAsterisk}
        after 1800 ->
            % Timeout after 30 minutes
            ProcessDictionaryPid ! {tunnel_closed, Username},
            timeout
    end.



% The authentication must be exact 33 bytes: 32 for the MD5 and 1 for the "\n" line delimiter.
check_authentication(TextualData) when length(TextualData) =:= 33; tl(TextualData) =:= "\n" ->
    MD5 = lists:sublist(TextualData, 32),
	% Get the username based on the MD5 Hash
	case(remote_services:username_for_md5(MD5)) of
	    {found, Username} -> {ok, Username};
	    not_found -> not_found
	end.

% Data must be 33 bytes. If it's not, it can't be valid.
check_authentication(BadData) -> not_allowed.