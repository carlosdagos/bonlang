{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import qualified Bonlang               as B
import           Control.Monad.Except
import           Data.Functor.Identity
import           System.Environment
import           System.Exit
import           Text.Parsec

runIOThrows :: B.IOThrowsException String -> IO String
runIOThrows action = fmap extractValue (runExceptT (trapError action))

readOrThrow
    :: (MonadError B.BonlangError m, Stream s Identity t)
    => Parsec s () a -> s -> m a
readOrThrow parser input
  = case parse parser "bonlang" input of
      Left e  -> throwError $ B.Parser e
      Right x -> return x

readExpr :: String -> B.ThrowsError B.BonlangValue
readExpr = readOrThrow B.bonlangParser

extractValue :: B.ThrowsError a -> a
extractValue (Right x)  = x
extractValue (Left err) = error $ show err

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

evalString :: B.Scope -> String -> IO String
evalString scope expr
  = runIOThrows $
      fmap show $ B.liftThrows (readExpr expr)
      >>= B.startEval scope

main :: IO ()
main = do
    file  <- fmap head getArgs
    c     <- readFile file
    s     <- B.primitiveBindings
    d     <- evalString s c
    putStrLn d -- TODO: Remove this line
    exitSuccess
