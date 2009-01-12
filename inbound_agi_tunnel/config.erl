% This is the hostname to which the Adhearsion should bind.
{adhearsion_listen_on, "0.0.0.0"}.

% This is the port to which the Adhearsion sandbox component connects from the outside world
{adhearsion_port, 20000}.

% This is the hostname to which the Asterisk AGI answering server socket should bind
{asterisk_listen_on, "127.0.0.1"}.

% This is the port on which to listen for incoming AGI connections from Asterisk.
{asterisk_port, 4574}.

% When a connection comes in from the Adhearsion sandbox component, how long should we hold onto it before closing it? Time
% is in minutes.
{default_adhearsion_wait_time, 5}.

% Which log file should we log to?
{log_file, "/opt/sandbox-infrastructure/inbound_agi_tunnel/inbound_agi_tunnel.log"}.

% This can be the 'false' atom if you want to just use the current directory
{working_dir, "/"}.

% This is the path to the script which authenticates the incoming calls.
{working_dir, "/opt/sandbox-infrastructure/inbound_agi_tunnel"}.
>>>>>>> 8e249b73b5af5d034ef322ed71eae3b57de23b6b:inbound_agi_tunnel/config.erl
