module Bonlang.Lang.Bool
    ( or
    , and
    , negate
    , orMaybe
    , andMaybe
    ) where

import           Bonlang.Lang
import           Prelude      hiding (and, or, negate)

binOp :: (Bool -> Bool -> Bool)
      -> BonlangValue
      -> BonlangValue
      -> ThrowsError BonlangValue
binOp op (BonlangBool x) (BonlangBool y)
  = return $ BonlangBool (x `op` y)
binOp _ x y
  = Left $ InternalTypeMismatch "Invalid types supplied" [x, y]

negate :: BonlangValue -> ThrowsError BonlangValue
negate (BonlangBool b)
  = return $ BonlangBool (not b)
negate _
  = Left $ InternalTypeMismatch "Invalid type supplied" []

or, and :: BonlangValue -> BonlangValue -> ThrowsError BonlangValue
or  = binOp (||)
and = binOp (&&)

orMaybe, andMaybe :: BonlangValue -> BonlangValue -> Maybe BonlangValue
orMaybe  x y = either (const Nothing) Just $ or x y
andMaybe x y = either (const Nothing) Just $ and x y
