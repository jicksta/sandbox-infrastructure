Erlang AGI Tunnel
=================

This project is a way for Adhearsion applications, using the `sandbox` component, to accept AGI calls from a remote Asterisk box, even through NAT. Because this server is written in Erlang, it can support a very large number of open, idle connections and switch their packets very efficiently.

Compiling and Running
---------------------

Once you have Erlang installed, simply do `erlc *.erl` in this directory.

To run the tunnel, type the following command in this directory.

    erl -noshell -s inbound_agi_tunnel start testing/config.testing.erl

If you wish to run it as a daemon in production mode, you can do

    erl -detached -noshell -s inbound_agi_tunnel start production/config.production.erl

The `-detached` flag makes it run in the background.

Custom Usernames
----------------

Because this tunnel system has an authentication step, you'll probably need to re-implement the `production/username_from_md5` file for your own domain. For the Adhearsion sandbox, it `require`s the ActiveRecord models used to power the Adhearsion.com Rails app and authenticates using its backend database and ORM methods.

The script can be implemented any way you like, even with a language other than Ruby, as long as it prints `"Not found!"` or the username from the database.