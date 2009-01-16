-module(inbound_agi_tunnel).
-export([start/0, start/1, start/2]).

start() ->
    start(["testing/config.testing.erl"]).

start([ConfigFile]) ->
    start(config_file, ConfigFile).

start(config_file, ConfigFile) ->
    {ok, ConfigDataStructure} = file:consult(ConfigFile),
    Config = dict:from_list(ConfigDataStructure),
    
    LogFilePath = dict:fetch(log_file, Config),
    io:format("Log file path is ~s~n", [LogFilePath]),
    
    LogFile = case(file:open(LogFilePath, [append])) of
        {ok, OpenedFile} -> OpenedFile;
        Error ->
            io:format("Could not open the log file \"~s\". Got error ~p", [LogFilePath, Error]),
            erlang:error(Error)
    end,
    io:format("Opened log file~n"),
    ConnectionSemaphore = spawn_link(fun() -> connection_semaphore:start() end),
    
    TunnelManager = tunnel_manager:new(Config, LogFile, ConnectionSemaphore),
    
    spawn_link(fun() -> TunnelManager:start() end).
