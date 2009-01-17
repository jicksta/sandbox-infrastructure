-module(inbound_agi_tunnel).
-export([start/0, start/1, start/2]).
-define(LOGGING_SPEC, ).

start() ->
    start(["envs/development/config.development.erl"]).

start([ConfigFile]) ->
    start(config_file, ConfigFile).

start(config_file, ConfigFile) ->    
    
    {ok, ConfigDataStructure} = file:consult(ConfigFile),
    
    Config = dict:from_list(ConfigDataStructure),
    
    % Initialize log4erl
    application:start(log4erl),
    log4erl:add_logger(tunnel),
    log4erl:add_file_appender(tunnel, dict:fetch(log4erl_spec, Config)),
    
    ConnectionSemaphore = spawn_link(fun() -> connection_semaphore:start() end),
    
    TunnelManager = tunnel_manager:new(Config, ConnectionSemaphore),
    
    TunnelManager:start().
