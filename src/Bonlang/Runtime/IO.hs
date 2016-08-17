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

ioOutput :: (BonlangValue -> IO ())
         -> [BonlangValue]
         -> IOThrowsException BonlangValue
ioOutput _ []  = Except.throwE $ NumArgs 0 []
ioOutput f [x] = do _ <- liftIO $ f x
                    return $ BonlangBool True
ioOutput _ xs  = Except.throwE $ NumArgs (length xs) []

print, puts, putsln :: IO.Handle
                    -> [BonlangValue]
                    -> IOThrowsException BonlangValue
print  h = ioOutput (IO.hPrint h)
puts   h = dPuts    (IO.hPutStr h)
putsln h = dPuts    (IO.hPutStrLn h)

dPuts :: (String -> IO ()) -> [BonlangValue] -> IOThrowsException BonlangValue
dPuts f x = let ms = map toString x in
            if any isNothing ms
               then Except.throwE $
                  InternalTypeMismatch "Can't turn to string" x
               else ioOutput (f . T.unpack . fromJust . toString) x
