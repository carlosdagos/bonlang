module Bonlang.Lang.Strings
    ( concat
    , toString
    ) where

import           Bonlang.Lang
import qualified Data.Text    as T
import Data.Monoid
import           Prelude      hiding (concat)

concat :: PrimFunc
concat xs = case xs of
  [BonlangString s, BonlangString s'] -> Right $ BonlangString $ s <> s'
  _ -> Left $ InternalTypeMismatch "Incompatible types" xs

toString :: PrimFunc
toString [s@BonlangString {}] = return s
toString [BonlangNumber x]    = return $ BonlangString (T.pack $ show x)
toString [BonlangBool b]      = return $ BonlangString (T.pack $ show b)
toString xs                   = Left $ NumArgs (length xs) xs
