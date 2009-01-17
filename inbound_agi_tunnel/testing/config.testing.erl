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

% This can be the 'false' atom if you want to just use the current directory
{working_dir, false}.

% When a connection comes in from the Adhearsion sandbox component, it sends an MD5 generated from the user's username and
% password. That MD5 is given as argument to the script and the script is expected to print to STDOUT one of two things:
% "Not found!" or the username from the database.
{authentication_script, "testing/not_found"}.

% This is the Spec format for log4erl

{log4erl_spec, { 
  "logs",         % Log directory
  "development",  % Filename
  {size, 512000}, % Size of file
  4,              % Rotations
  "log",          % File extension
  debug           % Logging level
}}.
