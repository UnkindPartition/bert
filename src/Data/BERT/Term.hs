{-# LANGUAGE OverlappingInstances, TypeSynonymInstances, FlexibleInstances #-}
-- | Define BERT terms their binary encoding & decoding and a typeclass
-- for converting Haskell values to BERT terms and back.
--
-- We define a number of convenient instances for 'BERT'. Users will
-- probably want to define their own instances for composite types.
module Data.BERT.Term
  ( BERT(..)
  , showTerm
  , parseTerm
  ) where

import Control.Monad
import Control.Applicative
import Data.Bits
import Data.Char
import Data.Int
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.List
import Data.Time
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf
import Data.BERT.Types
import Data.BERT.Parser

-- The 0th-hour as per the BERT spec.
zeroHour = UTCTime (read "1970-01-01") 0

decomposeTime :: UTCTime -> (Int, Int, Int)
decomposeTime t = (mS, s, uS)
  where
    d       = diffUTCTime t zeroHour
    (mS, s) = (floor d) `divMod` 1000000
    uS      = floor $ 1000000 * (snd $ properFraction d)

composeTime :: (Int, Int, Int) -> UTCTime
composeTime (mS, s, uS) = addUTCTime seconds zeroHour
  where
    mS'     = fromIntegral mS
    s'      = fromIntegral s
    uS'     = fromIntegral uS
    seconds = ((mS' * 1000000) + s' + (uS' / 1000000))

-- Another design would be to split the Term type into
-- SimpleTerm|CompositeTerm, and then do everything in one go, but
-- that complicates syntax and semantics for end users. Let's do this
-- one ugly thing instead, eh?
ct b rest = TupleTerm $ [AtomTerm "bert", AtomTerm b] ++ rest
compose NilTerm = ListTerm []
compose (BoolTerm True) = ct "true" []
compose (BoolTerm False) = ct "false" []
compose (DictionaryTerm kvs) =
  ct "dict" [ListTerm $ map (\(k, v) -> TupleTerm [k, v]) kvs]
compose (TimeTerm t) =
  ct "time" [IntTerm mS, IntTerm s, IntTerm uS]
  where
    (mS, s, uS) = decomposeTime t
compose (RegexTerm s os) =
  ct "regex" [BytelistTerm (C.pack s),
              TupleTerm [ListTerm $ map AtomTerm os]]
compose _ = error "invalid composite term"

showTerm (IntTerm x) = show x
showTerm (FloatTerm x) = printf "%15.15e" x
showTerm (AtomTerm "") = ""
showTerm (AtomTerm a@(x:xs))
  | isAsciiLower x = a
  | otherwise      = "'" ++ a ++ "'"
showTerm (TupleTerm ts) =
  "{" ++ intercalate ", " (map showTerm ts) ++ "}"
showTerm (BytelistTerm bs) = show $ C.unpack bs
showTerm (ListTerm ts) =
  "[" ++ intercalate ", " (map showTerm ts) ++ "]"
showTerm (BinaryTerm b)
  | all (isAscii . chr . fromIntegral) (B.unpack b) =
      wrap $ "\"" ++ C.unpack b ++ "\""
  | otherwise =
      wrap $ intercalate ", " $ map show $ B.unpack b
  where
    wrap x = "<<" ++ x ++ ">>"
showTerm (BigintTerm x) = show x
showTerm (BigbigintTerm x) = show x
-- All other terms are composite:
showTerm t = showTerm . compose $ t

class BERT a where
  -- | Introduce a 'Term' from a Haskell value.
  showBERT :: a -> Term
  -- | Attempt to read a haskell value from a 'Term'.
  readBERT :: Term -> (Either String a)

-- Herein are some instances for common Haskell data types. To do
-- anything more complicated, you should make your own instance.

instance BERT Term where
  showBERT = id
  readBERT = return . id

instance BERT Int where
  showBERT = IntTerm
  readBERT (IntTerm value) = return value
  readBERT _ = fail "Invalid integer type"

instance BERT Bool where
  showBERT = BoolTerm
  readBERT (BoolTerm x) = return x
  readBERT _ = fail "Invalid bool type"

instance BERT Integer where
  showBERT = BigbigintTerm
  readBERT (BigintTerm x) = return x
  readBERT (BigbigintTerm x) = return x
  readBERT _ = fail "Invalid integer type"

instance BERT Float where
  showBERT = FloatTerm
  readBERT (FloatTerm value) = return value
  readBERT _ = fail "Invalid floating point type"

instance BERT String where
  showBERT = BytelistTerm . C.pack
  readBERT (BytelistTerm x) = return $ C.unpack x
  readBERT (BinaryTerm x) = return $ C.unpack x
  readBERT (AtomTerm x) = return x
  readBERT (ListTerm xs) = mapM readBERT xs >>= return . map chr
  readBERT _ = fail "Invalid string type"

instance BERT ByteString where
  showBERT = BytelistTerm
  readBERT (BytelistTerm value) = return value
  readBERT _ = fail "Invalid bytestring type"

instance (BERT a) => BERT [a] where
  showBERT xs = ListTerm $ map showBERT xs
  readBERT (ListTerm xs) = mapM readBERT xs
  readBERT _ = fail "Invalid list type"

instance (BERT a, BERT b) => BERT (a, b) where
  showBERT (a, b) = TupleTerm [showBERT a, showBERT b]
  readBERT (TupleTerm [a, b]) = liftM2 (,) (readBERT a) (readBERT b)
  readBERT _ = fail "Invalid tuple(2) type"

instance (BERT a, BERT b, BERT c) => BERT (a, b, c) where
  showBERT (a, b, c) = TupleTerm [showBERT a, showBERT b, showBERT c]
  readBERT (TupleTerm [a, b, c]) =
    liftM3 (,,) (readBERT a) (readBERT b) (readBERT c)
  readBERT _ = fail "Invalid tuple(3) type"

instance (BERT a, BERT b, BERT c, BERT d) => BERT (a, b, c, d) where
  showBERT (a, b, c, d) =
    TupleTerm [showBERT a, showBERT b, showBERT c, showBERT d]
  readBERT (TupleTerm [a, b, c, d]) =
    liftM4 (,,,) (readBERT a) (readBERT b) (readBERT c) (readBERT d)
  readBERT _ = fail "Invalid tuple(4) type"

instance (Ord k, BERT k, BERT v) => BERT (Map k v) where
  showBERT m = DictionaryTerm
             $ map (\(k, v) -> (showBERT k, showBERT v)) (Map.toList m)
  readBERT (DictionaryTerm kvs) =
    mapM (\(k, v) -> liftM2 (,) (readBERT k) (readBERT v)) kvs >>=
      return . Map.fromList
  readBERT _ = fail "Invalid map type"

-- Binary encoding & decoding.
instance Binary Term where
  put term = putWord8 131 >> putTerm term
  get      = getWord8 >>= \magic ->
               case magic of
                 131 -> getTerm
                 _   -> fail "bad magic"

-- | Binary encoding of a single term (without header)
putTerm :: Term -> PutM ()
putTerm (IntTerm value) = tag 98 >> put32s value
putTerm (FloatTerm value) =
  tag 99 >> (putL . C.pack . pad $ printf "%15.15e" value)
  where
    pad s = s ++ replicate (31 - (length s)) '\0'
putTerm (AtomTerm value)
  | len < 256 = tag 100 >> put16u len >> (putL $ C.pack value)
  | otherwise = fail "BERT atom too long (>= 256)"
  where
    len = length value
putTerm (TupleTerm value)
  | len < 256 = tag 104 >> put8u len  >> forM_ value putTerm
  | otherwise = tag 105 >> put32u len >> forM_ value putTerm
  where
    len = length value
putTerm (BytelistTerm value)
  | len < 65536 = tag 107 >> put16u len >> putL value
  | otherwise = do  -- too big: encode as a list.
      tag 108
      put32u len
      forM_ (B.unpack value) $ \v -> do
        tag 97
        putWord8 v
  where
    len = B.length value
putTerm (ListTerm value)
  | len == 0 = putNil  -- this is mentioned in the BERT spec.
  | otherwise= do
      tag 108
      put32u $ length value
      forM_ value putTerm
      putNil
  where
    len = length value
    putNil = putWord8 106
putTerm (BinaryTerm value) = tag 109 >> (put32u $ B.length value) >> putL value
putTerm (BigintTerm value) = tag 110 >> putBigint put8u value
putTerm (BigbigintTerm value) = tag 111 >> putBigint put32u value
-- All other terms are composite:
putTerm t = putTerm . compose $ t

-- | Binary decoding of a single term (without header)
getTerm :: Get Term
getTerm = do
  tag <- get8u
  case tag of
    97  -> IntTerm <$> get8u
    98  -> IntTerm <$> get32s
    99  -> getL 31 >>= return . FloatTerm . read . C.unpack
    100 -> get16u >>= getL >>= return . AtomTerm . C.unpack
    104 -> get8u >>= getN >>= tupleTerm
    105 -> get32u >>= getN >>= tupleTerm
    106 -> return $ ListTerm []
    107 -> get16u >>= getL >>= return . BytelistTerm
    108 -> get32u >>= getN >>= return . ListTerm
    109 -> get32u >>= getL >>= return . BinaryTerm
    110 -> getBigint get8u >>= return . BigintTerm . fromIntegral
    111 -> getBigint get32u >>= return . BigintTerm . fromIntegral
  where
    getN n = replicateM n getTerm
    -- First try & decode composite terms.
    tupleTerm [AtomTerm "bert", AtomTerm "true"]  = return $ BoolTerm True
    tupleTerm [AtomTerm "bert", AtomTerm "false"] = return $ BoolTerm False
    tupleTerm [AtomTerm "bert", AtomTerm "dict", ListTerm kvs] =
      mapM toTuple kvs >>= return . DictionaryTerm
      where
        toTuple (TupleTerm [k, v]) = return $ (k, v)
        toTuple _ = fail "invalid dictionary"
    tupleTerm [AtomTerm "bert", AtomTerm "time",
               IntTerm mS, IntTerm s, IntTerm uS] =
      return $ TimeTerm $ composeTime (mS, s, uS)
    tupleTerm [AtomTerm "bert", AtomTerm "regex",
               BytelistTerm s, ListTerm os] =
      options os >>= return . RegexTerm (C.unpack s)
      where
        -- TODO: type-check the options values as well
        options []                = return []
        options ((AtomTerm o):os) = options os >>= return . (o:)
        options _                 = fail "regex options must be atoms"
    -- All other tuples are just .. tuples
    tupleTerm xs = return $ TupleTerm xs

putBigint putter value = do
  putter len  -- TODO: verify size?
  if value < 0
    then put8u 1
    else put8u 0
  putL $ B.pack $ map (fromIntegral . digit) [0..len-1]
  where
    value'    = abs value
    len       = ceiling $ logBase 256 (fromIntegral $ value' + 1)
    digit pos = (value' `shiftR` (8 * pos)) .&. 0xFF

getBigint getter = do
  len   <- fromIntegral <$> getter
  sign  <- get8u
  bytes <- getL len
  multiplier <-
    case sign of
      0 -> return 1
      1 -> return (-1)
      _ -> fail "Invalid sign byte"
  return $ (*) multiplier
         $ foldl (\s (n, d) -> s + d*(256^n)) 0
         $ zip [0..len-1] (map fromIntegral $ B.unpack bytes)

-- Note about put32s/get32s:
--
-- When dealing with 32-bit signed ints, we first convert between Int and
-- Int32, and only then cast to Word32. This is to ensure put and get are
-- as close to inverse as possible. Coercing word types to and from
-- integer types using 'fromIntegral' is guaranteed to preserve
-- representation (see Notes in "Data.Int").
--
-- For an example of what can go wrong, see
-- https://github.com/feuerbach/bert/issues/6

put8u :: (Integral a) => a -> Put
put8u = putWord8 . fromIntegral
put16u :: (Integral a) => a -> Put
put16u = putWord16be . fromIntegral
put32u :: (Integral a) => a -> Put
put32u = putWord32be . fromIntegral
put32s :: (Integral a) => a -> Put
put32s = putWord32be . (fromIntegral :: Int32 -> Word32) . fromIntegral
putL = putLazyByteString

get8u  = fromIntegral <$> getWord8
get16u = fromIntegral <$> getWord16be
get32u = fromIntegral <$> getWord32be
get32s = fromIntegral . (fromIntegral :: Word32 -> Int32) <$> getWord32be
getL :: (Integral a) => a -> Get ByteString
getL = getLazyByteString . fromIntegral

tag :: Word8 -> Put
tag which = putWord8 which
