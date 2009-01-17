Erlang AGI Tunnel
=================

This project is a way for Adhearsion applications, using the `sandbox` component, to accept AGI calls from a remote Asterisk box, even through NAT. Because this server is written in Erlang, it can support a very large number of open, idle connections and switch their packets very efficiently.

Compiling and Running
---------------------

Once you have Erlang installed, just run `make` to build the project.

    make

To run the tunnel using development mode, use the development.sh script

    ./development.sh

This will load the envs/development/config.development.erl file. This file configures the application to run on a development computer.

If you wish to run it as a daemon in production mode, you can do

    erl -detached -noshell -s inbound_agi_tunnel start envs/production/config.production.erl

The `-detached` flag makes it run in the background.

Custom Usernames
----------------

Because this tunnel system has an authentication step, you'll probably need to re-implement the `production/username_from_md5` file for your own domain. For the Adhearsion sandbox, it `require`s the ActiveRecord models used to power the Adhearsion.com Rails app and authenticates using its backend database and ORM methods.

The script can be implemented any way you like, even with a language other than Ruby, as long as it prints `"Not found!"` or the username from the database.

TODO
----

* The system can't exit gracefully yet

License
-------
Copyright (c) 2009 Jay Phillips

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.