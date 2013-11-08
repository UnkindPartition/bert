-- | BERT-RPC client (<http://bert-rpc.org/>). This implements the client RPC call logic.

module Network.BERT.Client
  ( -- * Example
    -- $example
    -- * Documentation
    call, tcpClient,
    Call, Error(..)
  ) where

import Data.BERT
import Network.BERT.Transport

data Error
  = ClientError String
  | ServerError Term
    deriving (Show, Ord, Eq)

-- | Convenience type for @call@
type Call a = IO (Either Error a)

-- | Call the @{mod, func, args}@ synchronously on the endpoint
-- defined by @transport@, returning the results of the call or an
-- error.
call :: (BERT a, BERT b, Transport t)
     => t
     -> String
     -> String
     -> [a]
     -> Call b
call transport mod fun args =
  runSession transport $ do
    sendt $ TupleTerm [AtomTerm "call", AtomTerm mod, AtomTerm fun,
                       ListTerm $ map showBERT args]
    recvAndHandle
  where
    handle (TupleTerm [AtomTerm "reply", reply]) =
      return $ either (const . Left $ ClientError "decode failed") Right
             $ readBERT reply
    handle (TupleTerm (AtomTerm "info":_)) =
      recvAndHandle  -- We don't yet handle info directives.
    handle t@(TupleTerm (AtomTerm "error":_)) =
      return $ Left . ServerError $ t
    handle t = fail $ "unknown reply " ++ (show t)

    recvAndHandle =
      recvt >>= maybe (fail "No answer") handle

-- $example
--
-- > t <- tcpClient "localhost" 8080
-- > r <- call t "calc" "add" ([123, 3000]::[Int])
-- > case r of
-- >   Right res -> print (res :: Int)
-- >   Left _    -> putStrLn "error"
