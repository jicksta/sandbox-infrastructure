-module(inbound_agi_tunnel).
-export([start/0]).
-compile(export_all).

start() ->
	io:format("Starting server~n"),
	register(process_dictionary, spawn_link(fun process_dictionary/0)),
	{ok, AdhearsionServerSocket} = gen_tcp:listen(20000, [list, {packet, line}, {active, false}]),
	{ok, AsteriskServerSocket}   = gen_tcp:listen(4574,  [list, {packet, line}, {active, false}]),
	spawn_link(fun() -> receive_adhearsion_connection_loop(AdhearsionServerSocket) end, []),
    spawn_link(fun() -> receive_asterisk_connection_loop(AsteriskServerSocket) end, []),
    receive
        stop ->
            gen_tcp:close(AdhearsionServerSocket),
            gen_tcp:close(AsteriskServerSocket)
    end.

receive_asterisk_connection_loop(ServerSocket) ->
    {ok, FromAsterisk} = gen_tcp:accept(ServerSocket),
    io:format("Received a connection from Asterisk: ~w~n", FromAsterisk),
    % TODO: Change spawn_link to spawn
    ConnectionHandler = spawn_link(fun() -> handle_asterisk_connection(FromAsterisk) end),
    gen_tcp:controlling_process(FromAsterisk, ConnectionHandler),
    ConnectionHandler ! start,
    receive_asterisk_connection_loop(ServerSocket).

receive_adhearsion_connection_loop(ServerSocket) ->
	{ok, FromAdhearsion} = gen_tcp:accept(ServerSocket),
	io:format("Received a connection from Adhearsion: ~w~n", FromAdhearsion),
	% TODO: Change spawn_link to spawn
	ConnectionHandler = spawn_link(fun() -> handle_adhearsion_connection(FromAdhearsion) end),
	gen_tcp:controlling_process(FromAdhearsion, ConnectionHandler),
	ConnectionHandler ! start,
	receive_adhearsion_connection_loop(ServerSocket).

handle_asterisk_connection(FromAsterisk) ->
    receive start -> ok end,
    case(extract_username_and_headers_via_agi(FromAsterisk)) of
        {username, Username, headers, Headers} ->
            ProcessDictionary = whereis(process_dictionary),
            ProcessDictionary ! {tunnel_connection_request, self(), Username},
            receive
                no_socket_waiting ->
                    reporter:incoming_sip_call_without_adhearsion_leg(Username);
                {found, AdhearsionPid} ->
                    gen_tcp:controlling_process(FromAsterisk, AdhearsionPid),
                    AdhearsionPid ! {bridge_request, FromAsterisk, Headers},
                    io:format("Handing control of Asterisk socket for ~s to ~w ~n", [Username, AdhearsionPid])
            end;
        Error ->
            reporter:asterisk_agi_initialization_error(Error)
    end.

handle_adhearsion_connection(FromAdhearsion) ->
    receive
        start ->
            % Receive just the authentication string
            inet:setopts(FromAdhearsion, {active, once}),
            handle_adhearsion_connection(FromAdhearsion);
        {tcp, FromAdhearsion, InitialData} ->
            case(check_authentication(InitialData)) of
        	    not_allowed -> reporter:new_connection_denied();
        	    not_found   -> reporter:connection_requested_for_nonexistent_hash();
        		Username    ->
        		    case(wait_for_agi_leg_or_timeout(Username)) of
        		        timeout ->
        		            reporter:adhearsion_connection_timed_out(Username),
                            gen_tcp:close(FromAdhearsion);
        		        {bridge_legs, FromAsterisk, Headers} ->
        		            start_tunnel_session(Username, FromAdhearsion, FromAsterisk, Headers)
        	        end
        	end;
        Error -> {error, Error}
    end.

% Yay! A connection has been made. Let's now start forwarding packets back and forth.
start_tunnel_session(Username, FromAdhearsion, FromAsterisk, Headers) ->
    inet:setopts(FromAdhearsion, {active, true}),
    inet:setopts(FromAsterisk, {active, true}),
    
    % Because the Asterisk PID had to read the headers to properly service itself, we'll write them back (in reverse order)
    lists:foreach(fun(Header) ->
        gen_tcp:send(FromAdhearsion, Header)
    end, Headers),
    gen_tcp:send(FromAdhearsion, "\n"),

    reporter:starting_tunnel_loop_with_headers(Headers),
    
    tunnel_loop(Username, FromAdhearsion, FromAsterisk).
    
tunnel_loop(Username, FromAdhearsion, FromAsterisk) ->
    receive
        {tcp, FromAdhearsion, Line} ->
            gen_tcp:send(FromAsterisk, Line),
            tunnel_loop(Username, FromAdhearsion, FromAsterisk);
        {tcp, FromAsterisk, Line} ->
            gen_tcp:send(FromAdhearsion, Line),
            tunnel_loop(Username, FromAdhearsion, FromAsterisk);
        _Error ->
            exit("There was an error on one of the legs:")
    end.
        

% % Yay! A connection has been made. Let's now start forwarding packets back and forth.
% start_tunnel_session(Username, FromAdhearsion, FromAsterisk, Headers) ->
%     AsteriskReader = spawn_link(fun() -> line_reading_loop(self(), FromAsterisk) end),
%     gen_tcp:controlling_process(FromAsterisk, AsteriskReader),
%     AsteriskReader ! start,
%     
%     AdhearsionReader = spawn_link(fun() -> line_reading_loop(self(), FromAdhearsion) end),
%     gen_tcp:controlling_process(FromAdhearsion, AdhearsionReader),
%     AdhearsionReader ! start,
%     
%     % TODO: These will be created by something else...
%     AsteriskWriter   = spawn_link(fun() -> line_writing_loop(FromAsterisk)   end),
%     AdhearsionWriter = spawn_link(fun() -> line_writing_loop(FromAdhearsion) end),
%     
%     % Because the Asterisk PID had to read the headers to properly service itself, we'll write them back (in reverse order)
%     % to the Adhearsion writer.
%     lists:foreach(fun(Header) ->
%         AdhearsionWriter ! {data, Header}
%     end, Headers),
%     AdhearsionWriter ! {data, "\n"}
%     
%     tunnel_loop(
%         Username,
%         {adhearsion, AdhearsionReader, AdhearsionWriter},
%         {asterisk, AsteriskReader, AsteriskWriter}
%     ).

% tunnel_loop(Username, Adhearsion, Asterisk) ->
%     {adhearsion, AdhearsionReader, AdhearsionWriter} = Adhearsion,
%     {asterisk,   AsteriskReader,   AsteriskWriter}   = Asterisk,
%     receive
%         {reader, ReaderPid, data, Line} ->
%             WriterPid = case(ReaderPid) of
%                 AdhearsionReader -> AsteriskWriter;
%                 AsteriskReader   -> AdhearsionWriter
%             end,
%             WriterPid ! {data, Line},
%             tunnel_loop(Username, Adhearsion, Asterisk);
%         Error ->
%             AdhearsionReader ! {stop, self()},
%             AdhearsionWriter ! {stop, self()},
%             AsteriskReader   ! {stop, self()},
%             AsteriskWriter   ! {stop, self()},
%             times(4, fun() -> receive stopped end end),
%             whereis(process_dictionary) ! {tunnel_closed, Username},
%     end.

% line_writing_loop(Parent, Socket) ->
%     receive
%         {data, Line} -> ok = gen_tcp:send(Socket, Line);
%         stop -> Parent ! stopped
%     end.
% 
% line_reading_loop(Parent, Socket) ->
%     receive
%         stop  -> Parent ! stopped;
%         start ->
%             inet:setopts(Socket, {active, true}),
%             line_reading_loop(Parent, Socket);
%         {tcp, Socket, Line} ->
%             Parent ! {Socket, data, Line},
%             line_reading_loop(Parent, Socket);
%         Error ->
%             Parent ! {reader, self(), error, Error};
%     end.

wait_for_agi_leg_or_timeout(Username) ->
    ProcessDictionaryPid = whereis(process_dictionary),
    ProcessDictionaryPid ! {tunnel_waiting, self(), Username},
    receive
        {bridge_request, FromAsterisk, Buffer} ->
            {bridge_legs, FromAsterisk, Buffer}
        after 300 ->
            % Timeout after 5 minutes
            ProcessDictionaryPid ! {tunnel_closed, Username},
            timeout
    end.

% The authentication request text must be exact 33 bytes: 32 for the MD5 and 1 for the "\n" line delimiter.
check_authentication(TextualData)  ->
    if 
        length(TextualData) =:= 33; tl(TextualData) =:= "\n" ->
            MD5 = chomp(TextualData),
        	% Get the username based on the MD5 Hash
        	case(username_for_md5(MD5)) of
        	    {found, Username} -> {ok, Username};
        	    not_found -> not_found
        	end;
        true -> not_allowed
    end.

extract_username_and_headers_via_agi(FromAsterisk) -> extract_username_and_headers_via_agi(FromAsterisk, []).
extract_username_and_headers_via_agi(FromAsterisk, Headers) ->
    inet:setopts(FromAsterisk, {active, once}),
    receive
        {tcp, FromAsterisk, "\n"} ->
            % This is the blank line which ends the headers' section of the AGI protocol.
            gen_tcp:send(FromAsterisk, "GET VARIABLE SANDBOX_USERNAME"),
            receive
                {tcp, FromAsterisk, Username} ->
                    {username, chomp(Username), headers, Headers};
                _Error -> error
            end;
        {tcp, FromAsterisk, HeaderLine} ->
            extract_username_and_headers_via_agi(FromAsterisk, [HeaderLine|Headers]);
        _Error -> error
    end.


% The process dictionary helps the Asterisk socket find the Adhearsion socket.

process_dictionary() -> process_dictionary(dict:new()).

process_dictionary(Dictionary) ->
    receive
        {tunnel_waiting, AdhearsionPid, Username} ->
            % TODO: catch the exception if this is not found. We're checking to see if it exists in the dictionary already.
            % If it does, we should raise an error which eventually makes it back to the Adhearsion socket.
            case(dict:is_key(Username, Dictionary)) of
                true  -> AdhearsionPid ! too_many_waiting;
                false -> dict:store(Username, AdhearsionPid, Dictionary)
            end;
        {tunnel_closed, Username} ->
            dict:erase(Username, Dictionary);
        {tunnel_connection_request, AsteriskPid, Username} ->
            % Find the Adhearsion Pid which should be waiting to be connected. If found, return Pid. If not, return an error.
            case(dict:find(Username)) of
                {ok, AdhearsionPid} ->
                    AsteriskPid ! {found, AdhearsionPid};
                error ->
                    AsteriskPid ! no_socket_waiting
            end
    end,
    process_dictionary(Dictionary).

username_for_md5(MD5) ->
    SearchResult = os:cmd("./username_from_md5 " ++ MD5),
    case(SearchResult) of
        "Not found!" -> not_found;
        Username     -> {found, Username}
    end.

chomp(String) ->
    case(lists:tl(String) =:= "\n") of
        true -> lists:sublist(String, length(String) - 1);
        false -> String
    end.

% times(0, Fun) -> done.
% times(Number, Fun) ->
%     Fun(),
%     times(Number - 1).