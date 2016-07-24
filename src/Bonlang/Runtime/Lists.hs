module Bonlang.Runtime.Lists
    ( map'
    ) where

import           Bonlang.Lang

-- This first list should be already resolved
map' :: [BonlangValue] -> IOThrowsException BonlangValue
map' (BonlangPrimIOFunc f:params)
  = do xs' <- sequence $ fmap (\x -> f [x]) params
       return $ BonlangList xs'
map' _ = error "TODO: Rest of space of functions"
