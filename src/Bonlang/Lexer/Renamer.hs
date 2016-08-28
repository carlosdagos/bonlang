-- | To avoid naming conflicts, this module will rename every function and
-- | variable to the full name of its "path" during lexing process, but
-- | should also be used when encountering closures during runtime
module Bonlang.Lexer.Renamer
      ( renameFunction
      , renameClosure
      ) where

import           Bonlang.Lang

renameFunction :: (Monad m)      -- Random monad
               => String         -- Current module name
               -> BonlangValue   -- Function def
               -> m BonlangValue -- Renamed result
renameFunction = undefined

renameClosure :: (Monad m)       -- Random monad
              => BonlangValue    -- Closure encountered
              -> m BonlangValue  -- Renamed result
renameClosure = undefined
