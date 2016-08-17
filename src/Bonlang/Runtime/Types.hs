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
import qualified Bonlang.Lang.Types as T
import qualified Data.Map as M

booleanUnaryCheck :: PrimFunc -> BonlangValue
booleanUnaryCheck f = BonlangClosure { cParams = ["b0"]
                                     , cEnv    = M.empty
                                     , cBody   = BonlangPrimFunc f
                                     }

isScalar', isString', isNumber', isInteger' :: BonlangValue
isDouble', isList', isFunction'             :: BonlangValue

isScalar'   = booleanUnaryCheck T.isScalarP
isString'   = booleanUnaryCheck T.isStringP
isNumber'   = booleanUnaryCheck T.isNumberP
isInteger'  = booleanUnaryCheck T.isIntegerP
isDouble'   = booleanUnaryCheck T.isDoubleP
isList'     = booleanUnaryCheck T.isListP
isFunction' = booleanUnaryCheck T.isFunctionP

isEqual' :: BonlangValue
isEqual' = BonlangClosure { cParams = ["b0", "b1"]
                          , cEnv    = M.empty
                          , cBody   = BonlangPrimFunc equals'
                          }
           where
             equals' :: PrimFunc
             equals' xs = case xs of
               [x, y] -> Right $ BonlangBool (x == y)
               _      -> Left $ NumArgs (length xs) xs
