Erlang AGI Tunnel
=================

This project is a way for Adhearsion applications, using the `sandbox` component, to accept AGI calls from a remote Asterisk box, even through NAT. Because this server is written in Erlang, it can support a very large number of open, idle connections and switch their packets very efficiently.

Compiling and Running
---------------------

Once you have Erlang installed, simply do `erlc inbound_agi_tunnel.erl`. This outputs an `inbound_agi_tunnel.beam` file.

To run the file, `cd` into the directory with the compiled `.beam` file and type the following command:

    erl -noshell -s inbound_agi_tunnel start config.erl

Note, if you don't wish to start with the configuration file (and simply accept all defaults) you can do this:

    erl -noshell -s inbound_agi_tunnel start

The defaults are defined in the `config` record in `inbound_agi_tunnel.erl`.

Custom Usernames
----------------

Because this tunnel system has an authentication step, you'll probably need to re-implement the `username_from_md5` file for your own domain. For the Adhearsion sandbox, it `require`s the ActiveRecord models used to power the Adhearsion.com Rails app and authenticates using its backend database and ORM methods.

The script can be implemented any way you like, even with a language other than Ruby, as long as it prints `"Not found!"` or the username from the database, **without** a trailing newline.