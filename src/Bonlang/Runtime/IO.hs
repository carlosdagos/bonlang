module Bonlang.Runtime.IO
    ( print
    , puts
    , putsln
    ) where

import           Bonlang.Lang
import           Bonlang.Lang.Strings
import           Control.Monad.IO.Class     (liftIO)
import qualified Control.Monad.Trans.Except as Except
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Prelude                    hiding (print)
import qualified System.IO                  as IO

ioOutput :: (String -> IO ()) -> BonlangValue
ioOutput f = BonlangClosure { cParams = ["s0"]
                            , cEnv    = M.empty
                            , cBody   = BonlangPrimIOFunc printFunc
                            }
  where
    printFunc :: PrimIOFunc
    printFunc xs = case toString xs of
      Right x  -> do _ <- liftIO $ f (T.unpack . unString $ x)
                     return $ BonlangBool True
      Left err -> Except.throwE err

print, puts, putsln :: IO.Handle -> BonlangValue
print  h = ioOutput (IO.hPrint h)
puts   h = ioOutput (IO.hPutStr h)
putsln h = ioOutput (IO.hPutStrLn h)
