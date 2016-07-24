module Bonlang.Lang.Types
    ( isScalar
    , isString
    , isBool
    , isNumber
    , isInteger
    , isDouble
    , isList
    , isFunction
    , isPrimFunction
    , isPrimFunction'
    , isPrimIOFunction
    ) where

import           Bonlang.Lang

isScalar, isString, isNumber, isInteger           :: BonlangValue -> Bool
isBool, isDouble, isList, isFunction              :: BonlangValue -> Bool
isPrimFunction, isPrimFunction', isPrimIOFunction :: BonlangValue -> Bool

isScalar x = or [isString x, isNumber x, isBool x]

isString (BonlangString _) = True
isString _                 = False

isBool (BonlangBool _) = True
isBool _               = False

isNumber (BonlangNumber _) = True
isNumber _                 = False

isInteger (BonlangNumber (Left _)) = True
isInteger _                        = False

isDouble (BonlangNumber (Right _)) = True
isDouble _                         = False

isList (BonlangList _) = True
isList _                    = False

isFunction (BonlangFunc _ _ _ _) = True
isFunction _                     = False

isPrimFunction (BonlangPrimFunc _)   = True
isPrimFunction (BonlangPrimIOFunc _) = True
isPrimFunction _                     = False

isPrimFunction' (BonlangPrimFunc _) = True
isPrimFunction' _                   = False

isPrimIOFunction (BonlangPrimIOFunc _) = True
isPrimIOFunction _                     = False