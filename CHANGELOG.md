Version 1.2.2.4
---------------

Fix compatibility with GHC 7.10

Version 1.2.2.3
---------------

Fix compatibility with recent conduit.

Version 1.2.2.2
---------------

Migrate from network-conduit (which is deprecated) to conduit-extra

Version 1.2.2.1
---------------

Fix build on GHC 7.4

Version 1.2.2
---------------

* `Show` and `Read` instances for `Term` now use Haskell, not Erlang syntax. To
  get the Erlang-syntax-formatted terms, `showTerm` and `parseTerm` are now
  exposed.

Version 1.2.1.2
---------------

* Fix Windows compatibility

Version 1.2.1.1
---------------

* Fix integer (de)serialization on 64-bit platforms

Version 1.2.1
-------------

* Fix the docs
* Export the `Error` data type

Version 1.2
-----------

* Drop the `bert` command-line tool
* Remove support for the (non-standard) bert:// URI
* Change the way transports are represented
* Instead of `fromURI` or `fromHostPort`, you should now use `tcpClient` and
  `tcpServer`
* Both the client and the server now support persistent connections
* The default TCP backlog is increased for the server
