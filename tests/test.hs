{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Control.Monad

import Data.Binary
import Data.Char (chr)
import Data.Map (Map)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series

import Data.BERT

-- NB A better Char instance would help here — something like
--
--  generate $ \d -> take d $ map chr [0..255]

instance (Serial m a, Ord a, Serial m b) => Serial m (Map a b) where
  series = liftM Map.fromList series

type T a = a -> Bool
-- value -> Term -> encoded -> Term -> value
t a = Right a == (readBERT . decode . encode . showBERT) a
-- value -> Term -> Packet -> encoded -> Packet -> Term -> value
p a = Right a == (readBERT . fromPacket . decode . encode . Packet . showBERT) a

main = defaultMain $ localOption (SmallCheckDepth 4) $ testGroup "Tests" [simpleTerms, simplePackets]

simpleTerms = testGroup "Simple terms"
  [ testProperty "Bool" (t :: T Bool)
  , testProperty "Integer" (t :: T Integer)
  , testProperty "String" (t :: T String)
  , testProperty "(String, String)" (t :: T (String, String))
  , testProperty "(String, [String])" (t :: T (String, [String]))
  , testProperty "[String]" (t :: T [String])
  , testProperty "(Map String String)" (t :: T (Map String String))
  , testProperty "(String, Int, Int, Int)" (t :: T (String, Int, Int, Int))
  , testProperty "(Int, Int, Int, Int)" (t :: T (Int, Int, Int, Int))
  ]

simplePackets = testGroup "Simple packets"
  [ testProperty "Bool" (p :: T Bool)
  , testProperty "Integer" (p :: T Integer)
  , testProperty "String" (p :: T String)
  , testProperty "(String, String)" (p :: T (String, String))
  , testProperty "(String, [String])" (p :: T (String, [String]))
  , testProperty "[String]" (p :: T [String])
  , testProperty "(Map String String)" (p :: T (Map String String))
  , testProperty "(String, Int, Int, Int)" (p :: T (String, Int, Int, Int))
  ]
