module Bonlang.Runtime.Types
    ( isScalar'
    , isString'
    , isNumber'
    , isInteger'
    , isDouble'
    , isList'
    , isFunction'
    , isEqual'
    ) where

import           Bonlang.Lang
import           Bonlang.Lang.Types
import qualified Control.Monad.Except as Error

booleanCheck :: (BonlangValue -> Bool)
             -> [BonlangValue]
             -> ThrowsError BonlangValue
booleanCheck _ [] = Error.throwError $ NumArgs 0 []
booleanCheck f xs = Right $ BonlangBool (all f xs)

isScalar', isString', isNumber', isInteger', isDouble', isList', isFunction'
    :: [BonlangValue] -> ThrowsError BonlangValue
isScalar'   = booleanCheck isScalar
isString'   = booleanCheck isString
isNumber'   = booleanCheck isNumber
isInteger'  = booleanCheck isInteger
isDouble'   = booleanCheck isDouble
isList'     = booleanCheck isList
isFunction' = booleanCheck isFunction

isEqual' :: [BonlangValue] -> ThrowsError BonlangValue
isEqual' []     = Error.throwError $ NumArgs 0 []
isEqual' [_]    = Error.throwError $ NumArgs 1 []
isEqual' (x:xs) = Right $ BonlangBool (all (x ==) xs)
