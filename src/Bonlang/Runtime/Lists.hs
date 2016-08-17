module Bonlang.Runtime.Lists
    ( map'
    ) where

import           Bonlang.Lang
import           Control.Monad.Trans.Except as Except
import qualified Data.Map                   as M

-- This first list should be already resolved
map' :: BonlangValue
map' = BonlangClosure { cParams = ["f0", "ps0"]
                      , cEnv    = M.empty
                      , cBody   = BonlangPrimIOFunc mapFunc
                      }

mapFunc :: PrimIOFunc
mapFunc ps = case ps of
   (primIO@BonlangPrimIOFunc {}:[list@BonlangList {}])
       -> return $ BonlangList $ map (applyFunc primIO) (unList list)
   (prim@BonlangPrimFunc {}:[list@BonlangList {}])
       -> return $ BonlangList $ map (applyFunc prim) (unList list)
   (clos@BonlangClosure {}:[list@BonlangList {}])
       -> return $ BonlangList $ map (applyFunc clos) (unList list)
   []    -> Except.throwE $ DefaultError "No arguments for map"
   [_]   -> Except.throwE $ NumArgs 0 []
   (x:_) -> Except.throwE $ TypeMismatch "Can't run map on non-function" x

applyFunc :: BonlangValue -> BonlangValue -> BonlangValue
applyFunc x p' = BonlangFuncApply { fResolver = x
                                  , fParams   = [p']
                                  }
