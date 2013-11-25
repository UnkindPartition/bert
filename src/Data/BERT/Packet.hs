-- | BERP (BERT packets) support.
module Data.BERT.Packet
  ( Packet(..)
  , fromPacket
  ) where

import Control.Monad
import Data.ByteString.Lazy as L
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import Data.BERT.Term ()
import Data.BERT.Types

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
