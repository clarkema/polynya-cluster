polynya-cluster
===============

`polynya-cluster` is a basic DX cluster node written in Erlang.  Unlike
a normal node it provides no facilities for users to connect, but
simply dumps all incoming spots into a CouchDB database.  Sending spots
is not yet implemented, but will work by watching the database for
updates using CouchDB's change notifications.
