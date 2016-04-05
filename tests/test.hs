{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

import Control.Monad

import Data.Binary
import Data.Char (chr, ord)
import Data.List (genericLength)
import Data.Map (Map)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import Text.Printf

import Control.Concurrent
import Control.Concurrent.Async
import Network
import System.Timeout

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.Tasty.HUnit
import Test.SmallCheck.Series

import Data.BERT
import Network.BERT.Client
import Network.BERT.Server

-- NB A better Char instance would help here — something like
--
--  generate $ \d -> take d $ map chr [0..255]

instance (Serial m a, Ord a, Serial m b) => Serial m (Map a b) where
  series = liftM Map.fromList series

type T a = a -> Either String String

eqVerbose :: (Eq a, Show a) => a -> a -> Either String String
eqVerbose x y =
  let sx = show x
      sy = show y
  in
  if x == y
    then Right $ printf "%s == %s" sx sy
    else Left  $ printf "%s /= %s" sx sy

-- value -> Term -> encoded -> Term -> value
t :: (BERT a, Eq a, Show a) => T a
t a = Right a `eqVerbose` (readBERT . decode . encode . showBERT) a

-- value -> Term -> Packet -> encoded -> Packet -> Term -> value
p :: (BERT a, Eq a, Show a) => T a
p a = Right a `eqVerbose` (readBERT . fromPacket . decode . encode . Packet . showBERT) a

main :: IO ()
main = defaultMain $ localOption (SmallCheckDepth 4) $
  testGroup "Tests"
    [ testGroup "Serialization" [simpleTerms, simplePackets]
    , networkTests
    , testGroup "Specification compliance" specTests
    ]

simpleTerms :: TestTree
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

simplePackets :: TestTree
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

networkTests :: TestTree
networkTests = testGroup "Network"
  [ networkTest1
  , networkTest2
  , networkTest3
  , networkTest4
  ]

port :: PortNumber
port = 1911

delay :: IO ()
delay = threadDelay (10^5)

networkTest1 :: TestTree
networkTest1 = testCase "Simple call" $ do
  t <- tcpServer port
  let server = serve t $ \ "mod" "f" [IntTerm a] -> return $ Success $ IntTerm (a+1)
  withAsync server $ \_ -> do
    delay
    c <- tcpClient "localhost" port
    result <- call c "mod" "f" [IntTerm 3]
    result @?= Right (IntTerm 4)

networkTest2 :: TestTree
networkTest2 = testCase "5 calls per connection" $ do
  t <- tcpServer port
  let server = serve t $ \ "mod" "f" [IntTerm a, IntTerm b] -> return $ Success $ IntTerm (a+b)
  withAsync server $ \_ -> do
    delay
    c <- tcpClient "localhost" port
    forM_ [1..5] $ \x -> do
      result <- call c "mod" "f" [IntTerm 3, IntTerm x]
      result @?= Right (IntTerm (3+x))

networkTest3 :: TestTree
networkTest3 = testCase "5 sequential connections" $ do
  t <- tcpServer port
  let server = serve t $ \ "mod" "f" [IntTerm a, IntTerm b] -> return $ Success $ IntTerm (a+b)
  withAsync server $ \_ -> do
    delay
    forM_ [1..5] $ \x -> do
      c <- tcpClient "localhost" port
      result <- call c "mod" "f" [IntTerm 3, IntTerm x]
      result @?= Right (IntTerm (3+x))

networkTest4 :: TestTree
networkTest4 = testCase "100 simultaneous connections" $ do
  t <- tcpServer port
  let server = serve t $ \ "mod" "f" [IntTerm a, IntTerm b] ->
        do
          threadDelay (5*10^5) -- 0.5s delay
          return $ Success $ IntTerm (a+b)
  r <-
    withAsync server $ \_ -> do
      delay
      timeout (10^6) $ do
        flip mapConcurrently [1..100] $ \x -> do
          c <- tcpClient "localhost" port
          result <- call c "mod" "f" [IntTerm 3, IntTerm x]
          result @?= Right (IntTerm (3+x))
  maybe (assertFailure "Timed out!") (const $ return ()) r

ord' :: Char -> Word8
ord' = fromIntegral . ord

-- Test internal representation according to specification
-- http://erlang.org/doc/apps/erts/erl_ext_dist.html
specTests :: [TestTree]
specTests =
  [ thereAndBackAgainTest "INTEGER_EXT" (IntTerm 0x400) [131, 98, 0, 0, 4, 0]
  , thereAndBackAgainTest "ATOM_EXT"
      (AtomTerm "foobar")
      ([131, 100, 0, 6] ++ map ord' "foobar")
  , thereAndBackAgainTest "SMALL_TUPLE_EXT"
      (TupleTerm
         [ AtomTerm x
         | x <- ["a", "b", "c", "d"]
         ])
      ([131, 104, 4] ++
       concat [ [100, 0, genericLength x] ++ map ord' x
              | x <- ["a", "b", "c", "d"]
              ])
  , thereAndBackAgainTest "LARGE_TUPLE_EXT"
      (TupleTerm
         [ AtomTerm [x]
         | x <- take 512 $ cycle ['a'..'z']
         ])
      ([131, 105, 0, 0, 2, 0] ++
       concat [ [100, 0, 1, ord' x]
              | x <- take 512 $ cycle ['a'..'z']
              ])
  , thereAndBackAgainTest "NIL_EXT"
      (ListTerm [])
      [131, 106]
  , thereAndBackAgainTest "STRING_EXT"
      (BytelistTerm "abc\0")
      [131, 107, 0, 4, 97, 98, 99, 0]
  , thereAndBackAgainTest "LIST_EXT"
      (ListTerm [AtomTerm "abc", AtomTerm "xyz"])
      ([131, 108, 0, 0, 0, 2] ++
       [100, 0, 3, 97, 98, 99] ++
       [100, 0, 3, 120, 121, 122] ++
       [106])
  , thereAndBackAgainTest "LIST_EXT - nested"
      (ListTerm [ListTerm [AtomTerm "abc"], ListTerm [AtomTerm "xyz"]])
      ([131, 108, 0, 0, 0, 2] ++
       ([108, 0, 0, 0, 1] ++ [100, 0, 3, 97, 98, 99]    ++ [106]) ++
       ([108, 0, 0, 0, 1] ++ [100, 0, 3, 120, 121, 122] ++ [106]) ++
       [106])
  , thereAndBackAgainTest "BINARY_EXT"
      (BinaryTerm "x\0y\1z")
      ([131, 109, 0, 0, 0, 5, 120, 0, 121, 1, 122])
  , thereAndBackAgainTest " SMALL_BIG_EXT"
      (BigintTerm $ 4 + 3 * 256 + 2 * 256^2 + 1 * 256^3)
      ([131, 110, 4, 0, 4, 3, 2, 1])
  ]

thereAndBackAgainTest :: String -> Term -> [Word8] -> TestTree
thereAndBackAgainTest name term binaryRepr = testGroup name
  [ testCase "Term -> binary" $
      L.unpack (encode term) @?= binaryRepr
  , testCase "binary -> Term" $
      decode (L.pack binaryRepr) @?= term
  ]

