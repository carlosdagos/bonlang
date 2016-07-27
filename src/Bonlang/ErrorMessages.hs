module Bonlang.ErrorMessages where

import           Bonlang.Lang
import           Text.Parsec  (SourcePos)

noMainModule :: SourcePos -> BonlangError
noMainModule at
  = DefaultError $ "No 'Main' module defined at: " ++ show at

noMainFunction :: SourcePos -> BonlangError
noMainFunction at
  = DefaultError $ "No 'main' function defined at: " ++ show at

cantStartNonModule :: BonlangValue -> BonlangError
cantStartNonModule x
  = DefaultError $ "Can't start eval on non module. Provided: " ++ show x
