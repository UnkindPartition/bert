{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Network.BERT.Transport
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- Transport for BERT-RPC client. Creates a transport from a URI,
-- leaving flexibility for several underlying transport
-- implementations. The current one is over sockets (TCP), with the
-- URI scheme bert, eg: <bert://localhost:9000>
-- 
-- The TCP transport will create a new connection for every request
-- (every block of 'withTransport'), which seems to be what the
-- current server implementations expect. It'd be great to have
-- persistent connections, however.

module Network.BERT.Transport
  ( Transport, fromURI, fromHostPort
  -- ** Transport monad
  , TransportM, withTransport
  , sendt, recvt, recvtForever
  -- ** Server side
  , servet
  ) where

import Control.Monad (forever)
import Control.Applicative
import Control.Monad.Reader
import Network.URI (URI(..), URIAuth(..), parseURI)
import Network.Socket (
  Socket(..), Family(..), SockAddr(..), SocketType(..), 
  SocketOption(..), AddrInfo(..), connect, socket, sClose, 
  setSocketOption, bindSocket, listen, accept, iNADDR_ANY, 
  getAddrInfo, defaultHints)
import Data.Maybe (fromJust)
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Network
import qualified Network.Socket.ByteString.Lazy as LS
import qualified System.Posix.Signals as Sig

import Data.BERT (Term(..), BERT(..), Packet(..))
import Data.BERT.Packet

-- | Defines a transport endpoint. Create with 'fromURI'.
data Transport
  = TcpTransport       SockAddr
  | TcpServerTransport Socket
    deriving (Show, Eq)

newtype TransportM a
  = TransportM { runTransportM :: Sink Packet (ReaderT Socket IO) a }
    deriving (Monad, MonadIO)

-- | Create a transport from the given URI.
fromURI :: String -> IO Transport
fromURI = fromURI_ . fromJust . parseURI

-- | Create a (TCP) transport from the given host and port
fromHostPort :: (Integral a) => String -> a -> IO Transport
fromHostPort "" port = 
  return $ TcpTransport 
         $ SockAddrInet (fromIntegral port) iNADDR_ANY
fromHostPort host port = do
  resolve host >>= return . TcpTransport 
                          . SockAddrInet (fromIntegral port)

fromURI_ uri@(URI { uriScheme = "bert:"
                  , uriAuthority = Just URIAuth 
                                   { uriRegName = host
                                   , uriPort = ':':port}}) =
  fromHostPort host (fromIntegral . read $ port)

servet :: Transport -> (Transport -> IO ()) -> IO ()
servet (TcpTransport sa) dispatch = do
  -- Ignore sigPIPE, which can be delivered upon writing to a closed
  -- socket.
  Sig.installHandler Sig.sigPIPE Sig.Ignore Nothing

  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock sa
  listen sock 1

  forever $ do
    (clientsock, _) <- accept sock
    setSocketOption clientsock NoDelay 1
    dispatch $ TcpServerTransport clientsock

resolve host = do
  r <- getAddrInfo (Just hints) (Just host) Nothing
  case r of
    (AddrInfo { addrAddress = (SockAddrInet _ addr) }:_) -> return addr
    _ -> fail $ "Failed to resolve " ++ host
  where
    hints = defaultHints { addrFamily = AF_INET }

-- | Execute the given transport monad action in the context of the
-- passed transport.
withTransport :: Transport -> TransportM a -> IO a
withTransport transport (TransportM m) = do
  sock <-
    case transport of
      TcpTransport sa -> do
        sock <- socket AF_INET Stream 0
        connect sock sa
        return sock
      TcpServerTransport sock -> return sock

  flip runReaderT sock $
    sourceSocket sock $= decodePackets $$ m

-- | Send a term (inside the transport monad)
sendt :: Term -> TransportM ()
sendt t = TransportM $ do
  sock <- ask
  liftIO $ LS.sendAll sock $ encode (Packet t)
  return ()

-- | Receive a term (inside the transport monad)
recvt :: TransportM (Maybe Term)
recvt = TransportM $ fmap fromPacket <$> await

recvtForever :: (Term -> TransportM a) -> TransportM ()
recvtForever f = TransportM $ awaitForever $ runTransportM . f . fromPacket
