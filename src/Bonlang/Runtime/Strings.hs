module Bonlang.Runtime.Strings
    ( concat'
    , toString'
    ) where

import           Bonlang.Lang
import qualified Bonlang.Lang.Strings as Strings
import qualified Bonlang.Lang.Types   as Types
import           Control.Monad        (foldM)
import qualified Control.Monad.Except as Error
import           Data.Maybe           (fromJust)

concat' :: [BonlangValue] -> ThrowsError BonlangValue
concat' [] = Error.throwError $ NumArgs 0 []
concat' xs'@(x:xs)
  = if isStringsList
       then Right $ fromJust $ foldM Strings.concatMaybe x xs
       else Error.throwError $
             InternalTypeMismatch "Can't concat non string" [findConflict xs']
    where
        isStringsList = all Types.isString xs'
        findConflict  = head . filter (not . Types.isString)

toString' :: [BonlangValue] -> ThrowsError BonlangValue
toString' []  = Error.throwError $ NumArgs 0 []
toString' [x]
  = case Strings.toString x of
      Just s  -> return $ BonlangString { unString = s }
      Nothing -> Error.throwError $ TypeMismatch "Can't convert to string" x
toString' xs  = Error.throwError $ NumArgs (length xs) xs
