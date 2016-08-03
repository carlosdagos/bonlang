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

isString BonlangString {} = True
isString _                = False

isBool BonlangBool {} = True
isBool _              = False

isNumber BonlangNumber {} = True
isNumber _                = False

isInteger (BonlangNumber (Left _)) = True
isInteger _                        = False

isDouble (BonlangNumber (Right _)) = True
isDouble _                         = False

isList BonlangList {} = True
isList _              = False

isFunction BonlangClosure {} = True
isFunction _                 = False

isPrimFunction BonlangPrimFunc {}   = True
isPrimFunction BonlangPrimIOFunc {} = True
isPrimFunction _                    = False

isPrimFunction' BonlangPrimFunc {} = True
isPrimFunction' _                  = False

isPrimIOFunction BonlangPrimIOFunc {} = True
isPrimIOFunction _                    = False
