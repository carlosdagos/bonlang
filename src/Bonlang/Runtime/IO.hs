module Bonlang.Runtime.IO
    ( print
    , puts
    , putsln
    ) where

import           Bonlang.Lang
import           Bonlang.Lang.Strings
import           Control.Monad.IO.Class     (liftIO)
import qualified Control.Monad.Trans.Except as Except
import           Data.Maybe                 (fromJust, isNothing)
import qualified Data.Text                  as T
import           Prelude                    hiding (print)
import qualified System.IO                  as IO

ioOutput :: (BonlangValue -> IO ()) -> [BonlangValue] -> IOThrowsException BonlangValue
ioOutput _ [] = Except.throwE $ NumArgs 0 []
ioOutput f xs = do _ <- liftIO $ mapM f xs
                   return $ BonlangBool True

print, puts, putsln :: [BonlangValue] -> IOThrowsException BonlangValue
print  = ioOutput IO.print
puts   = dPuts IO.putStr
putsln = dPuts IO.putStrLn

dPuts :: (String -> IO ()) -> [BonlangValue] -> IOThrowsException BonlangValue
dPuts f x = let ms = map toString x in
            if any isNothing ms
               then Except.throwE $
                  InternalTypeMismatch "Can't turn to string" x
               else ioOutput (f . T.unpack . fromJust . toString) x
