module Bonlang.Lang.Bool
    ( or
    , and
    , negate
    ) where

import           Bonlang.Lang
import           Prelude      hiding (and, or, negate)

binOp :: (Bool -> Bool -> Bool) -> PrimFunc
binOp op x = case x of
  [BonlangBool y, BonlangBool z]
      -> return $ BonlangBool (y `op` z)
  _   -> Left $ InternalTypeMismatch "Invalid types supplied" x

negate :: PrimFunc
negate x = case x of
  [BonlangBool b] -> return $ BonlangBool (not b)
  _               -> Left $ InternalTypeMismatch "Invalid type supplied" []

or, and :: PrimFunc
or  = binOp (||)
and = binOp (&&)
