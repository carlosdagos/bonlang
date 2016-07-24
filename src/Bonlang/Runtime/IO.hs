module Bonlang.Runtime.IO
    ( print
    , puts
    , putsln
    ) where

import           Bonlang.Lang
import           Bonlang.Lang.Strings
import           Control.Monad.IO.Class     (liftIO)
import qualified Control.Monad.Trans.Except as Except
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T
import           Prelude                    hiding (print)
import qualified System.IO                  as IO

ioOutput :: (BonlangValue -> IO ()) -> [BonlangValue] -> IOThrowsException BonlangValue
ioOutput _ [] = Except.throwE $ NumArgs 0 []
ioOutput f xs = do _ <- liftIO $ mapM f xs
                   return $ BonlangBool True

print, puts, putsln :: [BonlangValue] -> IOThrowsException BonlangValue
print  = ioOutput IO.print
puts   = ioOutput (IO.putStr . T.unpack . fromJust . toString)
putsln = ioOutput (IO.putStrLn . T.unpack . fromJust . toString)

