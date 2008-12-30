-module(inbound_agi_tunnel).
-export([start/0]).

start() ->
	io:format("Starting server~n"),
	{ok, ServerSocket} = gen_tcp:listen(20000, [list, {packet, 0}, {active, true}]),
	spawn_link(fun -> receive_connection_loop(ServerSocket) end),
	loop(fun() ->
		receive
			{'EXIT', something}
		end
	end).

loop(Fun) -> loop(Fun, []).
loop(Fun, Args) ->
	apply(Fun, Args),
	loop(Fun, Args).

receive_connection_loop(ServerSocket) ->
	{ok, Socket} = gen_tcp:accept(ServerSocket),
	handle_connection(self(), Socket),
	receive_connection_loop(ServerSocket).

handle_connection(Parent, Socket) ->
	case(authenticate_socket(Socket)) of
		ok -> wait_for_agi_leg_or_timeout(Socket);
		Error ->
			Parent ! {unauthenticated, Socket}
	end,
	handle_connection(Parent).
	
authenticate_socket(Parent) ->
	receive
		{tcp, _Socket, RawAuthentication} ->
			[LastCharacter|ReversedAuthentication] = lists:reverse(RawAuthentication),
			case(LastCharacter) of
				"\n" -> ok
				_ -> exit({})
			end
	end,