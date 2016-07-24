module Bonlang.Runtime.Numeric
    ( add'
    , subtract'
    , multiply'
    , divide'
    , modulo'
    , greaterThan'
    , greaterThanOrEquals'
    , lessThan'
    , lessThanOrEquals'
    ) where

import           Bonlang.Lang
import           Bonlang.Lang.Numeric
import           Bonlang.Lang.Types
import           Control.Monad        (foldM)
import qualified Control.Monad.Except as Error
import           Data.Maybe           (fromJust)
import           Prelude              hiding (subtract)

numericOp :: (BonlangValue -> BonlangValue -> Maybe BonlangValue)
          -> [BonlangValue]
          -> ThrowsError BonlangValue
numericOp _ [] = Error.throwError $ NumArgs 0 []
numericOp op xs'@(x:xs)
  = if isNumericList xs'
       then case foldM op x xs of
              r@(Just _) -> Right $ fromJust r
              _          -> Error.throwError $
                InternalTypeMismatch "Internal error" []
       else Error.throwError $
                TypeMismatch "Can't operate on non-number" (findConflict xs')
    where
        isNumericList = all isNumber
        findConflict  = head . filter (not . isNumber)

binaryNumericOp :: (BonlangValue -> BonlangValue -> Maybe BonlangValue)
          -> [BonlangValue]
          -> ThrowsError BonlangValue
binaryNumericOp op xs
  | length xs /= 2 = Error.throwError $ NumArgs (length xs) xs
  | otherwise      = numericOp op xs

add', subtract', multiply', divide', modulo'
   :: [BonlangValue] -> ThrowsError BonlangValue
greaterThan', greaterThanOrEquals', lessThan', lessThanOrEquals'
   :: [BonlangValue] -> ThrowsError BonlangValue

add'                 = numericOp add
subtract'            = numericOp subtract
multiply'            = numericOp multiply
divide'              = binaryNumericOp divide
modulo'              = binaryNumericOp modulo
greaterThan'         = binaryNumericOp greaterThan
greaterThanOrEquals' = binaryNumericOp greaterThanOrEquals
lessThan'            = binaryNumericOp lessThan
lessThanOrEquals'    = binaryNumericOp lessThanOrEquals
