%% This is the application resource file (.app file) for the 'base'
%% application.
{application, log4erl,
[{description, "Simple logger for erlang"},
 {vsn, "0.8.2"},
 {modules, [log4erl]},
 {registered,[log4erl]},
 {applications, [kernel,stdlib]},
 {mod, {log4erl,[default_logger]}},
 {start_phases, []}
]}.
