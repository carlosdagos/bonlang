module Bonlang.Runtime.Bool
    ( or'
    , and'
    , negate'
    ) where

import           Bonlang.Lang
import qualified Bonlang.Lang.Bool    as B
import qualified Data.Map             as M
import           Prelude              hiding (negate)

booleanOp :: PrimFunc -> BonlangValue
booleanOp f = BonlangClosure { cParams = ["b0", "b1"]
                             , cEnv    = M.empty
                             , cBody   = BonlangPrimFunc f
                             }

or', and' :: BonlangValue
or'  = booleanOp B.or
and' = booleanOp B.and

negate' :: BonlangValue
negate' = BonlangClosure { cParams = ["b0"]
                         , cEnv    = M.empty
                         , cBody   = BonlangPrimFunc B.negate
                         }
