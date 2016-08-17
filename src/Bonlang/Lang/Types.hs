module Bonlang.Lang.Types
    ( isScalar
    , isScalarP
    , isString
    , isStringP
    , isBool
    , isBoolP
    , isNumber
    , isNumberP
    , isInteger
    , isIntegerP
    , isDouble
    , isDoubleP
    , isList
    , isListP
    , isFunction
    , isFunctionP
    , isPrimFunction
    , isPrimFunctionP
    , isPrimFunctionA
    , isPrimFunctionAP
    , isPrimIOFunction
    , isPrimIOFunctionP
    ) where

import           Bonlang.Lang

isScalar, isString, isNumber, isInteger           :: BonlangValue -> Bool
isBool, isDouble, isList, isFunction              :: BonlangValue -> Bool
isPrimFunction, isPrimFunctionA, isPrimIOFunction :: BonlangValue -> Bool

isScalarP, isStringP, isNumberP, isIntegerP          :: PrimFunc
isBoolP, isDoubleP, isListP, isFunctionP             :: PrimFunc
isPrimFunctionP, isPrimFunctionAP, isPrimIOFunctionP :: PrimFunc

unaryTransform :: (BonlangValue -> Bool) -> PrimFunc
unaryTransform f xs = case xs of
  [x] -> return $ BonlangBool (f x)
  _   -> Left $ NumArgs (length xs) xs

isScalarP         = unaryTransform isScalar
isStringP         = unaryTransform isString
isNumberP         = unaryTransform isNumber
isIntegerP        = unaryTransform isInteger
isBoolP           = unaryTransform isBool
isDoubleP         = unaryTransform isDouble
isListP           = unaryTransform isList
isFunctionP       = unaryTransform isFunction
isPrimFunctionP   = unaryTransform isPrimFunction
isPrimFunctionAP  = unaryTransform isPrimFunctionA
isPrimIOFunctionP = unaryTransform isPrimIOFunction

isScalar x = isString x || isNumber x || isBool x

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

isPrimFunctionA BonlangPrimFunc {} = True
isPrimFunctionA _                  = False

isPrimIOFunction BonlangPrimIOFunc {} = True
isPrimIOFunction _                    = False
