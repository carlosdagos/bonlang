module Bonlang.Runtime.Bool
    ( or'
    , and'
    , negate'
    ) where

import           Bonlang.Lang
import           Bonlang.Lang.Bool
import           Bonlang.Lang.Types
import           Control.Monad        (foldM)
import qualified Control.Monad.Except as Error
import           Data.Maybe           (fromJust)
import           Prelude              hiding (negate)

booleanOp :: (BonlangValue -> BonlangValue -> Maybe BonlangValue)
          -> [BonlangValue]
          -> ThrowsError BonlangValue
booleanOp _ [] = Error.throwError $ NumArgs 0 []
booleanOp op xs'@(x:xs)
  = if isBoolList xs'
       then case foldM op x xs of
              r@(Just _) -> Right $ fromJust r
              Nothing    -> Error.throwError $
                  InternalTypeMismatch "Something went wrong" xs'
       else Error.throwError $
                   TypeMismatch "Can't operate on non-number" (findConflict xs)
    where
        isBoolList   = all isBool
        findConflict = head . filter (not . isNumber)

or', and' :: [BonlangValue] -> ThrowsError BonlangValue
or'  = booleanOp orMaybe
and' = booleanOp andMaybe

negate' :: [BonlangValue] -> ThrowsError BonlangValue
negate' []  = Error.throwError $ NumArgs 0 []
negate' [x] = negate x
negate' xs  = Error.throwError $ NumArgs (length xs) xs
