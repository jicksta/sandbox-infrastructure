-module(inbound_agi_tunnel).
-export([start/0, start/2, report/1, report/2]).
-record(config, {adhearsion_listen_on="0.0.0.0",
                 adhearsion_port=20000,
                 asterisk_listen_on="127.0.0.1",
                 asterisk_port=4574,
                 default_adhearsion_wait_time=10,
                 log_file="inbound_agi_tunnel.log"}).

start() ->
    start(config, #config{}).

start(config_file, ConfigFile) ->
    start(config, record_from_config_file(file:consult(ConfigFile)));

start(config, Config) ->
    ReporterPid = start_reporter(Config#config.log_file),
    link(ReporterPid),
    register(reporter, ReporterPid),
    
    report("Starting server"),
	register(process_dictionary, spawn_link(fun process_dictionary/0)),
	
	{ok, AdhearsionServerSocket} = gen_tcp:listen(Config#config.adhearsion_port,
	    [list, {packet, line}, {active, false}, {ip, Config#config.adhearsion_listen_on}]),
	{ok, AsteriskServerSocket}   = gen_tcp:listen(Config#config.asterisk_port,
	    [list, {packet, line}, {active, false},{ip, Config#config.asterisk_listen_on}]),
	
	report("Listening on ports~n"),
	
	Adhearsion = spawn_link(fun() -> receive_adhearsion_connection_loop(AdhearsionServerSocket) end),
    Asterisk   = spawn_link(fun() -> receive_asterisk_connection_loop(AsteriskServerSocket) end),
    
    receive
        stop ->
            report("Received stop request~n"),
            ReporterPid ! stop,
            gen_tcp:close(AdhearsionServerSocket),
            gen_tcp:close(AsteriskServerSocket);
        {'EXIT', Adhearsion, Why} ->
            report("Adhearsion connection loop error! ~s~n", [Why]);
        {'EXIT', Asterisk, Why} ->
            report("Asterisk connection loop error! ~s~n", [Why]);
        {'EXIT', Other, Why} ->
            report("PROCESS CRASH: [~p] ~s~n", [Other, Why])
    end,
    gen_tcp:close(AdhearsionServerSocket),
    gen_tcp:close(AsteriskServerSocket).

record_from_config_file(Tuples) ->
    lists:foldl(fun(Record, ConfigParameter) ->
        case(ConfigParameter) of
            {adhearsion_listen_on, IPAddress} ->
                Record#config{adhearsion_listen_on=IPAddress};
            {adhearsion_port, Port} ->
                Record#config{adhearsion_port=Port};
            {asterisk_listen_on, IPAddress} ->
                Record#config{asterisk_listen_on=IPAddress};
            {asterisk_port, Port} ->
                Record#config{asterisk_port=Port};
            {log_file, LogFile} ->
                Record#config{log_file=LogFile};
            {default_adhearsion_wait_time, Seconds} ->
                Record#config{default_adhearsion_wait_time=Seconds};
            Other ->
                report("Ignoring unrecognized configuration option: ~p~n", [Other]),
                Record
        end
    end, #config{}, Tuples).

receive_asterisk_connection_loop(ServerSocket) ->
    {ok, FromAsterisk} = gen_tcp:accept(ServerSocket),
    report("Received a connection from Asterisk: ~p~n", [FromAsterisk]),
    % TODO: Change spawn_link to spawn
    ConnectionHandler = spawn_link(fun() -> handle_asterisk_connection(FromAsterisk) end),
    gen_tcp:controlling_process(FromAsterisk, ConnectionHandler),
    ConnectionHandler ! start,
    receive_asterisk_connection_loop(ServerSocket).

receive_adhearsion_connection_loop(ServerSocket) ->
	{ok, FromAdhearsion} = gen_tcp:accept(ServerSocket),
	report("Received a connection from Adhearsion: ~p~n", [FromAdhearsion]),
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
                    report(incoming_sip_call_without_adhearsion_leg, Username),
                    gen_tcp:send(FromAsterisk, "SET VARIABLE BRIDGE_OUTCOME \"FAILED\""),
                    gen_tcp:close(FromAsterisk);
                {found, AdhearsionPid} ->
                    gen_tcp:controlling_process(FromAsterisk, AdhearsionPid),
                    AdhearsionPid ! {bridge_request, FromAsterisk, Headers},
                    report("Handing control of Asterisk socket for ~s to ~p ~n", [Username, AdhearsionPid])
            end;
        Error ->
            report(asterisk_agi_initialization_error, Error),
            gen_tcp:send(FromAsterisk, "SET VARIABLE BRIDGE_OUTCOME \"FAILED\""),
            gen_tcp:close(FromAsterisk)
    end.

handle_adhearsion_connection(FromAdhearsion) ->
    report("handle_adhearsion_connection~n"),
    receive
        start ->
            % Receive just the authentication string
            ok = inet:setopts(FromAdhearsion, [{active, once}]),
            report("Handling an Adhearsion connection. Expecting initial data next~n"),
            handle_adhearsion_connection(FromAdhearsion);
        {tcp, _Socket, InitialData} ->
            report("Got initial data: '~s'~n", [InitialData]),
            case(check_authentication(InitialData)) of
        	    not_allowed ->
        	        report(new_connection_denied),
        	        gen_tcp:send(FromAdhearsion, "authentication failed\n"),
                    gen_tcp:close(FromAdhearsion);
        	    not_found ->
        	        report(connection_requested_for_nonexistent_hash),
        	        gen_tcp:send(FromAdhearsion, "authentication failed\n"),
                    gen_tcp:close(FromAdhearsion);
        		{ok, Username} ->
        		    report("Adhearsion request from ~s~n", [Username]),
        		    case(wait_for_agi_leg(Username)) of
        		        timeout ->
        		            report(adhearsion_connection_timed_out, Username),
                            gen_tcp:close(FromAdhearsion);
                        too_many_waiting ->
                            report(duplicate_adhearsion_connection),
                            gen_tcp:send(FromAdhearsion, "wait 10\n"),
                            gen_tcp:close(FromAdhearsion);
        		        {bridge_legs, FromAsterisk, Headers} ->
        		            start_tunnel_session(Username, FromAdhearsion, FromAsterisk, Headers)
        	        end
        	end;
        Error ->
            report("Got a gen_tcp error! ~p~n", [Error]),
            {error, Error}
    end.

% Yay! A connection has been made. Let's now start forwarding packets back and forth.
start_tunnel_session(Username, FromAdhearsion, FromAsterisk, Headers) ->
    ok = inet:setopts(FromAdhearsion, [{active, true}, {nodelay, true}]),
    ok = inet:setopts(FromAsterisk, [{active, true}, {nodelay, true}]),
    
    gen_tcp:send(FromAdhearsion, "authentication accepted\n"),
    gen_tcp:send(FromAsterisk, "SET VARIABLE BRIDGE_OUTCOME \"SUCCESS\""),
    
    % Because the Asterisk PID had to read the headers to properly service itself, we'll write them back (in reverse order)
    lists:foreach(fun(Header) ->
        gen_tcp:send(FromAdhearsion, Header)
    end, Headers),
    gen_tcp:send(FromAdhearsion, "\n"),

    report(starting_tunnel_loop_with_headers, Headers),
    
    tunnel_loop(Username, FromAdhearsion, FromAsterisk).
    
tunnel_loop(Username, FromAdhearsion, FromAsterisk) ->
    receive
        {tcp, FromAdhearsion, Line} ->
            gen_tcp:send(FromAsterisk, Line),
            tunnel_loop(Username, FromAdhearsion, FromAsterisk);
        {tcp, FromAsterisk, Line} ->
            gen_tcp:send(FromAdhearsion, Line),
            tunnel_loop(Username, FromAdhearsion, FromAsterisk);
        {tcp_closed, FromAsterisk} ->
            report(session_gracefully_stopped, Username),
            gen_tcp:close(FromAdhearsion);
        {tcp_closed, FromAdhearsion} ->
            report(session_gracefully_stopped, Username),
            gen_tcp:close(FromAsterisk);
        Error ->
            report("There was an error on one of the legs: ~p~n", [Error]),
            gen_tcp:close(FromAdhearsion),
            gen_tcp:close(FromAsterisk)
    end.

wait_for_agi_leg(Username) ->
    ProcessDictionaryPid = whereis(process_dictionary),
    ProcessDictionaryPid ! {tunnel_waiting, self(), Username},
    receive
        {bridge_request, FromAsterisk, Buffer} ->
            {bridge_legs, FromAsterisk, Buffer};
        too_many_waiting ->
            too_many_waiting
        after 300 * 1000 ->
            % Timeout after 5 minutes
            ProcessDictionaryPid ! {tunnel_closed, Username},
            timeout
    end.

% The authentication request text must be exact 33 bytes: 32 for the MD5 and 1 for the "\n" line delimiter.
check_authentication(TextualData)  ->
    [LastCharacter|_] = lists:reverse(TextualData),
    if 
        length(TextualData) =:= 33, LastCharacter =:= 10 ->
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
    ok = inet:setopts(FromAsterisk, [{active, once}]),
    receive
        {tcp, FromAsterisk, "\n"} ->
            % This is the blank line which ends the headers' section of the AGI protocol.
            ok = inet:setopts(FromAsterisk, [{active, once}]),
            gen_tcp:send(FromAsterisk, "GET VARIABLE SANDBOX_USERNAME"),
            receive
                {tcp, FromAsterisk, UsernameWithNewline} ->
                    Username = chomp(UsernameWithNewline),
                    report("Got an incoming Asterisk AGI call for username '~s'~n", [Username]),
                    {username, Username, headers, Headers};
                Error ->
                    report("Received unrecognized data when requesting SANDBOX_USERNAME: ~p~n", [Error])
            end;
        {tcp, FromAsterisk, HeaderLine} ->
            report("Asterisk header: ~s~n", [HeaderLine]),
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
                false -> process_dictionary(dict:store(Username, AdhearsionPid, Dictionary))
            end;
        {tunnel_closed, Username} ->
            process_dictionary(dict:erase(Username, Dictionary));
        {tunnel_connection_request, AsteriskPid, Username} ->
            % Find the Adhearsion Pid which should be waiting to be connected. If found, return Pid. If not, return an error.
            case(dict:find(Username, Dictionary)) of
                {ok, AdhearsionPid} ->
                    AsteriskPid ! {found, AdhearsionPid};
                error ->
                    AsteriskPid ! no_socket_waiting
            end,
            process_dictionary(Dictionary)
    end.

username_for_md5(MD5) ->
    SearchResult = os:cmd("./username_from_md5 " ++ MD5),
    case(SearchResult) of
        "Not found!" -> not_found;
        Username     -> {found, Username}
    end.

chomp(String) ->
    [LastCharacter|_] = lists:reverse(String),
    case(LastCharacter =:= 10) of
        true -> lists:sublist(String, length(String) - 1);
        false -> String
    end.

report(String) when is_list(String) ->
    whereis(reporter) ! String.

report(String, FormatArgs) when is_list(String) and is_list(FormatArgs) ->
    whereis(reporter) ! io_lib:format(String, FormatArgs).

start_reporter(LogFilePath) ->
    case(file:open(LogFilePath, [append])) of
        {ok, LogFile} ->
            spawn(fun() -> reporter_loop(LogFile) end);
        Error ->
            io:format("Could not open the log file \"~s\". Got error ~p~n", [LogFilePath, Error]),
            erlang:error(Error)
    end.

reporter_loop(LogFile) ->
    receive
        stop ->
            Message = "Reporter PID stopping",
            io:format(LogFile, Message),
            io:format(Message),
            file:close(LogFile);
        String when is_list(String) ->
            io:format(LogFile, String),
            io:format(String),
            reporter_loop(LogFile)
    end.