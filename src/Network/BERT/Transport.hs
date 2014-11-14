{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts #-}
-- | Underlying transport abstraction
module Network.BERT.Transport
  (
  -- * Core definitions
    Transport(..)
  , Server(..)
  , TransportM(..)
  , SendPacketFn
  -- * Sending and receiving packets
  , sendt, recvt, recvtForever
  -- * TCP transport
  , TCP(..)
  , tcpClient
  , TCPServer(..)
  , tcpServer
  -- * Utilities
  , resolve
  ) where

import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Network.Socket as Net
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Serialization.Binary
import Data.Void

import Data.BERT

-- | A function to send packets to the peer
type SendPacketFn = Packet -> IO ()

-- | The transport monad allows receiving packets through the conduit,
-- and sending functions via the provided 'SendPacketFn'
type TransportM = ReaderT SendPacketFn (ConduitM Packet Void IO)

-- | The class for transports
class Transport t where
  runSession :: t -> TransportM a -> IO a
  closeConnection :: t -> IO ()

class Transport (ServerTransport s) => Server s where
  -- | The underlying transport used by the server
  type ServerTransport s

  -- | This method should listen for incoming requests, establish some
  -- sort of a connection (represented by the transport) and then invoke
  -- the handling function
  runServer :: s -> (ServerTransport s -> IO ()) -> IO ()

  -- | Free any resources that the server has acquired (such as the
  -- listening socket)
  cleanup :: s -> IO ()

-- | The TCP transport
data TCP = TCP {
    getTcpSocket :: !Socket
    -- ^ The socket used for communication.
    --
    -- The connection is assumed to be already established when this
    -- structure is passed in.
  }

tcpSendPacketFn :: TCP -> SendPacketFn
tcpSendPacketFn (TCP sock) packet =
  yield packet    $=
  conduitEncode   $$
  sinkSocket sock

instance Transport TCP where
  runSession tcp@(TCP sock) session =
    sourceSocket sock $=
    conduitDecode     $$
    (runReaderT session (tcpSendPacketFn tcp))
  closeConnection (TCP sock) = sClose sock

-- | Establish a connection to the TCP server and return the resulting
-- transport. It can be used to make multiple requests.
tcpClient :: HostName -> PortNumber -> IO TCP
tcpClient host port = do
  sock <- socket AF_INET Stream defaultProtocol
  sa <- SockAddrInet port <$> resolve host
  Net.connect sock sa
  return $ TCP sock

-- | The TCP server
data TCPServer = TCPServer {
    getTcpListenSocket :: !Socket
    -- ^ The listening socket. Assumed to be bound but not listening yet.
  }

instance Server TCPServer where
  type ServerTransport TCPServer = TCP

  runServer (TCPServer sock) handle = do
    listen sock sOMAXCONN

    forever $ do
      (clientsock, _) <- accept sock
      setSocketOption clientsock NoDelay 1
      handle $ TCP clientsock

  cleanup (TCPServer sock) = sClose sock

-- | A simple 'TCPServer' constructor, listens on all local interfaces.
--
-- If you want to bind only to some of the interfaces, create the socket
-- manually using the functions from "Network.Socket".
tcpServer :: PortNumber -> IO TCPServer
tcpServer port = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bindSocket sock $ SockAddrInet port iNADDR_ANY
  return $ TCPServer sock

-- | Send a term
sendt :: Term -> TransportM ()
sendt t = ask >>= \send -> liftIO . send . Packet $ t

-- | Receive a term
recvt :: TransportM (Maybe Term)
recvt = fmap fromPacket <$> lift await

-- | Execute an action for every incoming term, until the connection is
-- closed
recvtForever :: (Term -> TransportM a) -> TransportM ()
recvtForever f =
  ReaderT $ \send -> awaitForever $ flip runReaderT send . f . fromPacket

-- | A simple address resolver
resolve :: HostName -> IO HostAddress
resolve host = do
  r <- getAddrInfo (Just hints) (Just host) Nothing
  case r of
    (AddrInfo { addrAddress = (SockAddrInet _ addr) }:_) -> return addr
    _ -> fail $ "Failed to resolve " ++ host
  where
    hints = defaultHints { addrFamily = AF_INET }
