module Bonlang.Lang.Strings
    ( concat
    , concatMaybe
    , toString
    ) where

import           Bonlang.Lang
import           Data.Maybe
import qualified Data.Text    as T
import           Prelude      hiding (concat)

concat :: BonlangValue -> BonlangValue -> ThrowsError BonlangValue
concat (BonlangString s) (BonlangString s')
  = Right $ BonlangString $ s `mappend` s'
concat x y
  = Left $ InternalTypeMismatch "Incompatible types" [x, y]

concatMaybe :: BonlangValue -> BonlangValue -> Maybe BonlangValue
concatMaybe x y = either (const Nothing) Just (concat x y)

toString :: BonlangValue -> Maybe T.Text
toString (BonlangString s) = Just s
toString (BonlangNumber x) = Just $ T.pack $ show x
toString (BonlangBool b)   = Just $ T.pack $ show b
toString _                 = Nothing
