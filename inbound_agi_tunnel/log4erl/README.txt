log4erl Manual:
===============

TOC:
====
1. Features
2. Installation
3. Usage
4. API
5. Future development
6. Lincense

1. FEATURES:
============
- Multiple logs
- Currently, only size-based log rotation of files for file appender
- Support default logger if no logger specified
- 5 predifined log levels (debug, info, warn, error, fatal)
- Support for user-specified log levels
- Support for simple log formatter (similar to Layouts in Log4J)

2. INSTALLATION:
================
To compile & install log4erl, download source from google code's website
(http://code.google.com/p/log4erl/) or from svn:
$> svn checkout http://log4erl.googlecode.com/svn/trunk/ log4erl
$> cd log4erl
$> make

or you can run the below from erlang shell:

$> cd src
$> erl
1> make:all([{outdir, "../ebin"}]).

3. USAGE:
=========
1- In order to use log4erl, you need to first include it in the path. There
are 2 ways to do this:
a) include the "log4erl" directory in erlang's "lib" directory in the target
machine (cp -Rf log4erl /where/erlang/is/lib).
$> cp -Rf log4erl /usr/local/lib/erlang/lib/

b) include the "log4erl" ebin directory in the path when running you program
$> erl -pz /path/to/log4erl ...

2- Once the log4erl directory is included, you can use its API as described in section "API". but before, 
you need to run:
> application:start(log4erl).

Precedance of log levels are:
all = debug < info < warn < error < fatal < none
User defined levels are always written except when none level is specified in the logger specification
(See below).

4. API:
=======
NOTE:
-----
Please be informed that the API below as of now is not stable and any of the functions/parameters below
may be changed without prior notice.

**> log4erl:add_logger(Name) -> ok | {error, E}
  Name :: atom() name of the logger 

  Example:
  log4erl:add_logger(chat_logger)
  This will create a new logger with name 'chat_logger"

**> log4erl:add_file_appender(Name, Spec) -> ok | {error, E}
  Name :: atom() name of the appender. This value will be used
  to uniquely references this file appender and can be used to 
  change log level and format of the appended file. This appender
  will be added to the default_logger

  Spec :: tuple() of the form
       {LogDir, LogFileName, {size, Size}, NumOfRotations, Suffix, LogLevel}
  This tuple defines the appender's attributes. 

  Example:
  log4erl:add_file_appender(chat_room1, {"../logs", "room1", {size, 100000}, 4, "txt", warn}).
  This will directs log4erl to create a file appender and add it to the default
  logger. all log messages towards default logger will be written to file
  "../logs/room1.txt". The file will have a size limit of 100000, after which
  the file will be rotated for 4 times. The log level will be warn, which means
  info & debug messages will not be written.

  After suffeciantly long time, the directory "../logs" will look like this:
  

**> log4erl:add_file_logger(Logger, Spec) -> ok
    log4erl:add_file_logger(Logger, SpecFile) -> ok
  Logger :: atom()
  This will create another logger and associate spec in Spec or in SpecFile to it. Spec and
  content of SpecFile are the same as in add_file_logger/1

  Example:
  log4erl:add_file_logger(chat_log, "../priv/chat_logs.conf").
  This will add another logger with the name "chat_log" and use configuration in
  the file "../priv/chat_logs.conf" for it. To write to this log, you need to specify
  the name of the logger.

**> log4erl:change_log_level(Level) -> ok
    log4erl:change_log_level(Logger, Level) -> ok
  Level::atom() = {all, debug, info, error, warn, fatal, none}
  Logger::atom()

  This will change log level for default logger or named logger to the level
  specified.

  Example:
  log4erl:change_log_level(info). %% This will change level of default logger to info
  log4erl:change_log_level(test_log, warn) %% This will change level of test_log logger to warn

**> log4erl:change_format(Appender, Format) -> ok
    log4erl:change_format(Logger, Appender, Format) -> ok
  Appender :: atom()
  Logger :: atom()
  Format :: string()

  @since version 0.8.2

  This will change the output format to the specified Format. Format is a pattern string similar to
  PatternLayout in Log4j. patterns is an arbitrary string with specifiers (proceeded by '%').
  Possible specifiers are below:
	   d - output date (1-2-2008)
	   j - output date (01-02-2008)
	   t - time (2:13:9)
	   T - time (02:28:01)
	   y - year in YY format (08)
	   Y - year in YYYY format (2008)
	   M - month
	   D - day
	   h - hour
	   m - minute
	   s - second
	   l - the actual log message
	   L - log level
	   n - new line
	   % - the percentage sign (%)

  Example: 
  log4erl:change_format(file1, "%j %T [%L] %l%n").
  Will result in the following output (on log4erl:warn(file1, "hello"))
  
  27-10-2008 15:28:59 [warn] hello

**> log4erl:log(Level, Log) -> ok
    Level :: atom()
    Log :: string()

  This will log the text Log to the default logger with level Level.

  Example:
  log4erl:log(warn, "Hello there").
  log4erl:log(test_level, "Hello there").

**> log4erl:log(Level, Log, Data) -> ok
    Level :: atom()
    Log :: string()
    Data :: list()

  This will log the text Log to the default logger with level Level and
  will use Data to format the log text.

  Example:
  log4erl:log(info, "received message ~p", [Msg]).

**> log4erl:log(Logger, Level, Log, Data) -> ok
    Logger :: atom()
    Level :: atom()
    Log :: string()
    Data :: list()

  This will log the (Log, Data) to Logger with level Level

  Example:
  log4erl:log(chat_log, debug, "user entered chat text: ~p", [Chat]).

**> log4erl:Level(Log) -> ok
    log4erl:Level(Log, Data) -> ok
    log4erl:Level(Logger, Data) -> ok
    log4erl:Level(Logger, Log, Data) -> ok

  Level :: warn | info | error | fatal | debug
  Logger :: atom()
  Log :: string()
  Data :: list()

  Exmaple:
  log4erl:info("This is an info msg").
  log4erl:warn("Received error ~p",[Msg]).
  log4erl:fatal(chat_log, "exception occured").
  log4erl:debug(chat_log, "message received is ~p", [Msg]).
  log4erl:error("Error").

5. FUTURE DEVELOPMENT:
======================
- Add support for extensive file-based configuration
- Add support for different log persistance methods (e.g DB logs, SNMP logs, syslog logs)
- Add support for time-based log rotation
- Add support for NDC & MDC ???

Please send your suggestion to ahmed.nawras <at @ at> gmail <dot . dot> com

6. LICENSE:
===========
This software is subject to "Mozilla Public License 1.1". You can find the license terms
in the file 'LICENSE.txt' shipping along with the source code. You may also get a copy
of the license term from the URL: "http://www.mozilla.org/MPL/MPL-1.1.html".
