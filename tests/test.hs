{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Control.Monad

import Data.Binary
import Data.Char (chr)
import Data.Map (Map)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map

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

type T a = a -> Bool
-- value -> Term -> encoded -> Term -> value
t a = Right a == (readBERT . decode . encode . showBERT) a
-- value -> Term -> Packet -> encoded -> Packet -> Term -> value
p a = Right a == (readBERT . fromPacket . decode . encode . Packet . showBERT) a

main = defaultMain $ localOption (SmallCheckDepth 4) $
  testGroup "Tests"
    [ testGroup "Serialization" [simpleTerms, simplePackets]
    , networkTests
    ]

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

networkTest1 = testCase "Simple call" $ do
  t <- tcpServer port
  let server = serve t $ \ "mod" "f" [IntTerm a] -> return $ Success $ IntTerm (a+1)
  withAsync server $ \_ -> do
    delay
    c <- tcpClient "localhost" port
    result <- call c "mod" "f" [IntTerm 3]
    result @?= Right (IntTerm 4)

networkTest2 = testCase "5 calls per connection" $ do
  t <- tcpServer port
  let server = serve t $ \ "mod" "f" [IntTerm a, IntTerm b] -> return $ Success $ IntTerm (a+b)
  withAsync server $ \_ -> do
    delay
    c <- tcpClient "localhost" port
    forM_ [1..5] $ \x -> do
      result <- call c "mod" "f" [IntTerm 3, IntTerm x]
      result @?= Right (IntTerm (3+x))

networkTest3 = testCase "5 sequential connections" $ do
  t <- tcpServer port
  let server = serve t $ \ "mod" "f" [IntTerm a, IntTerm b] -> return $ Success $ IntTerm (a+b)
  withAsync server $ \_ -> do
    delay
    forM_ [1..5] $ \x -> do
      c <- tcpClient "localhost" port
      result <- call c "mod" "f" [IntTerm 3, IntTerm x]
      result @?= Right (IntTerm (3+x))

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
