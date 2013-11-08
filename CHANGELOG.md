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
