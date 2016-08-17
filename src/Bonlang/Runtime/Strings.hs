module Bonlang.Runtime.Strings
    ( concat'
    , toString'
    ) where

import           Bonlang.Lang
import qualified Bonlang.Lang.Strings as Strings
import qualified Data.Map             as M

concat' :: BonlangValue
concat' = BonlangClosure { cParams = ["s0", "s1"]
                         , cEnv    = M.empty
                         , cBody   = BonlangPrimFunc Strings.concat
                         }

toString' :: BonlangValue
toString' = BonlangClosure { cParams = ["s0"]
                           , cEnv    = M.empty
                           , cBody   = BonlangPrimFunc Strings.toString
                           }
