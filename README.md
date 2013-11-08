BERT[-RPC] for Haskell
======================

Originally written by marius a. eriksen (marius@monkey.org)

This is a [BERT](http://bert-rpc.org/) serializer/deserializer and
[BERT-RPC](http://bert-rpc.org) client and server for
[Haskell](http://www.haskell.org/). BERT-RPC currently supports
synchronous (`call`) requests.

The primitives provided are fairly elementary: for the client, `call`
provides the capability to perform the RPC call, while the server's
`serve` is provided with a dispatch function providing the dispatching
logic for the server. Thus, one can imagine building higher level
abstractions on top of these primitives.

Installation
------------

It's a cabal package, so

    $ cabal install bert

should do the trick.

BERT
----

    import qualified Data.ByteString.Lazy.Char8 as C
    import Data.BERT

Creating BERT terms is simple.

    TupleTerm [BytelistTerm (C.pack "hello"), IntTerm 123]

Or by using the `BERT` typeclass.

    showBERT $ ("hello", 123)

The `BERT` class can also read terms back.

    Right ("hello", 123) = readBERT . showBERT $ ("hello", 123)

BERT-RPC client
---------------

    import Data.BERT
    import Network.BERT.Client

Create a transport to the server endpoint, and issue a (synchronous)
call with it.

    t <- tcpClient "localhost" 8080
    r <- call t "calc" "add" ([123, 3000]::[Int])
    case r of
      Right res -> print (res :: Int)
      Left _    -> putStrLn "error"
    
BERT-RPC server
---------------

    import Data.BERT
    import Network.BERT.Server

Create a transport from which to accept connections, and provide a
dispatch function for incoming RPCs. The dispatch function is issued
in a new thread for each incoming request.

    main = do
      s <- tcpServer 8080
      serve t dispatch

    dispatch "calc" "add" [IntTerm a, IntTerm b] = 
      return $ Success $ IntTerm (a + b)
    dispatch "calc" _ _ =
      return NoSuchFunction
    dispatch _ _ _ = 
      return NoSuchModule
