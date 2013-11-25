-- | BERT-RPC server (<http://bert-rpc.org/>). This implements the
-- client RPC call/reply logic. Only synchronous requests are
-- supported at this time.

{-# LANGUAGE CPP #-}

module Network.BERT.Server
  (
    -- * Example
    -- $example
    -- * Documentation
    serve
  , DispatchResult(..)
  , tcpServer
  ) where

import Control.Concurrent
import Control.Monad.Trans
import Control.Exception
import Network.BERT.Transport
import Network.Socket
import Data.ByteString.Lazy.Char8 as C
import Data.BERT
import Text.Printf
#if !mingw32_HOST_OS
import qualified System.Posix.Signals as Sig
#endif

data DispatchResult
  = Success Term
  | NoSuchModule
  | NoSuchFunction
  | Undesignated String
    deriving (Eq, Show, Ord)

data TcpServer = TcpServer !Socket

-- | Serve from the given transport (forever), handling each request
-- with the given dispatch function in a new thread.
serve
  :: Server s
  => s
  -> (String -> String -> [Term] -> IO DispatchResult)
  -> IO ()
serve server dispatch = do
#if !mingw32_HOST_OS
  -- Ignore sigPIPE, which can be delivered upon writing to a closed
  -- socket.
  Sig.installHandler Sig.sigPIPE Sig.Ignore Nothing
#endif

  (runServer server $ \t ->
    (forkIO $ runSession t $ handleCall dispatch) >> return ())
    `finally`
    cleanup server

handleCall dispatch = recvtForever handle
  where
    handle (TupleTerm [AtomTerm "info", AtomTerm "stream", _]) =
      sendErr "server" 0 "BERTError" "streams are unsupported" []
    handle (TupleTerm [AtomTerm "info", AtomTerm "cache", _]) =
      return () -- Ignore caching requests.
    handle (TupleTerm [
             AtomTerm "call", AtomTerm mod,
             AtomTerm fun, ListTerm args]) = do
      res <- liftIO $ dispatch mod fun args
      case res of
        Success term ->
          sendt $ TupleTerm [AtomTerm "reply", term]
        NoSuchModule ->
          sendErr "server" 1 "BERTError"
                  (printf "no such module \"%s\"" mod :: String) []
        NoSuchFunction ->
          sendErr "server" 2 "BERTError"
                  (printf "no such function \"%s\"" fun :: String) []
        Undesignated detail ->
          sendErr "server" 0 "HandlerError" detail []

    sendErr etype ecode eclass detail backtrace =
      sendt $ TupleTerm [
        AtomTerm "error",
        TupleTerm [
          AtomTerm etype, IntTerm ecode, BinaryTerm . C.pack $ eclass,
          ListTerm $ Prelude.map (BinaryTerm . C.pack) backtrace]]

-- $example
--
-- To serve requests, create a server and call 'serve' with a
-- dispatch function.
--
-- > main = do
-- >   s <- tcpServer 8080
-- >   serve s dispatch
-- >
-- > dispatch "calc" "add" [IntTerm a, IntTerm b] =
-- >   return $ Success $ IntTerm (a + b)
-- > dispatch "calc" _ _ =
-- >   return NoSuchFunction
-- > dispatch _ _ _ =
-- >   return NoSuchModule
