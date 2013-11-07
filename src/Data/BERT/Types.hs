-- | The Term type.
module Data.BERT.Types
  ( Term(..)
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Time (UTCTime)

-- | A single BERT term.
data Term
  -- Simple (erlang) terms:
  = IntTerm        Int
  | FloatTerm      Float
  | AtomTerm       String
  | TupleTerm      [Term]
  | BytelistTerm   ByteString
  | ListTerm       [Term]
  | BinaryTerm     ByteString
  | BigintTerm     Integer
  | BigbigintTerm  Integer
  -- Composite (BERT specific) terms:
  | NilTerm
  | BoolTerm       Bool
  | DictionaryTerm [(Term, Term)]
  | TimeTerm       UTCTime
  | RegexTerm      String [String]
    deriving (Eq, Ord)

