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

binOp
    :: (a -> BonlangValue)
    -> (BonlangNum -> BonlangNum -> a)
    -> BonlangValue
    -> BonlangValue
    -> Maybe BonlangValue
binOp c op (BonlangNumber x) (BonlangNumber y) = Just $ c (x `op` y)
binOp _ _ _ _ = Nothing

binNumericOp
    :: (BonlangNum -> BonlangNum -> BonlangNum)
    -> BonlangValue
    -> BonlangValue
    -> Maybe BonlangValue
binNumericOp = binOp BonlangNumber

binBoolOp
    :: (BonlangNum -> BonlangNum -> Bool)
    -> BonlangValue
    -> BonlangValue
    -> Maybe BonlangValue
binBoolOp = binOp BonlangBool

add, subtract, multiply, divide, modulo
    :: BonlangValue -> BonlangValue -> Maybe BonlangValue
greaterThan, greaterThanOrEquals, lessThan, lessThanOrEquals
    :: BonlangValue -> BonlangValue -> Maybe BonlangValue

add                 = binNumericOp (+)
multiply            = binNumericOp (*)
subtract            = binNumericOp (-)
divide              = binNumericOp (/)
modulo              = binNumericOp mod
greaterThan         = binBoolOp (>)
greaterThanOrEquals = binBoolOp (>=)
lessThan            = binBoolOp (<)
lessThanOrEquals    = binBoolOp (<=)

