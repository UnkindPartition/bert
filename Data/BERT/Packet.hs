-- |
-- Module      : Data.BERT.Packet
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- BERP (BERT packets) support.
module Data.BERT.Packet 
  ( Packet(..)
  , fromPacket
  , decodePackets
  ) where

import Control.Monad (liftM)
import Data.ByteString.Lazy as L
import Data.ByteString as BS
import Data.Binary (Binary(..), Get(..), encode, decode)
import Data.Binary.Put (putWord32be, putLazyByteString)
import Data.Binary.Get (getWord32be, getLazyByteString, runGet, runGetState)
import Data.Conduit
import Data.Conduit.Serialization.Binary

import Data.BERT.Term
import Data.BERT.Types (Term(..))

-- | A single BERP. Little more than a wrapper for a term.
data Packet
  = Packet Term
    deriving (Show, Ord, Eq)

fromPacket (Packet t) = t

instance Binary Packet where
  put (Packet term) = 
    putWord32be (fromIntegral len) >> putLazyByteString encoded
    where encoded = encode term
          len     = L.length encoded

  get = getPacket

getPacket =
  liftM fromIntegral getWord32be >>= 
  getLazyByteString              >>= 
  return . Packet . decode

-- | Conduit which decodes a binary stream into BERT packets
decodePackets :: MonadThrow m => Conduit BS.ByteString m Packet
decodePackets = conduitGet getPacket
