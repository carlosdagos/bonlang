module Bonlang.Lang.Numeric
    ( add
    , subtract
    , multiply
    , divide
    , modulo
    , greaterThan
    , greaterThanOrEquals
    , lessThan
    , lessThanOrEquals
    ) where

import           Bonlang.Lang
import           Prelude      hiding (subtract)

binOp :: (a -> BonlangValue)              -- A Constructor
      -> (BonlangNum -> BonlangNum -> a)  -- A binary operation
      -> PrimFunc                         -- A primitive function
binOp c op xs = case xs of
  [BonlangNumber x, BonlangNumber y] -> return $ c (x `op` y)
  _ -> Left $ NumArgs (length xs) xs

binNumericOp :: (BonlangNum -> BonlangNum -> BonlangNum) -> PrimFunc
binNumericOp = binOp BonlangNumber

binBoolOp :: (BonlangNum -> BonlangNum -> Bool) -> PrimFunc
binBoolOp = binOp BonlangBool

add, subtract, multiply, divide, modulo                      :: PrimFunc
greaterThan, greaterThanOrEquals, lessThan, lessThanOrEquals :: PrimFunc

add                 = binNumericOp (+)
multiply            = binNumericOp (*)
subtract            = binNumericOp (-)
divide              = binNumericOp (/)
modulo              = binNumericOp mod
greaterThan         = binBoolOp (>)
greaterThanOrEquals = binBoolOp (>=)
lessThan            = binBoolOp (<)
lessThanOrEquals    = binBoolOp (<=)
