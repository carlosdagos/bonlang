{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import qualified Bonlang               as B
import           Control.Monad.Except
import           Data.Functor.Identity
import           System.Environment
import           Text.Parsec

readExpr :: String -> B.ThrowsError B.BonlangValue
readExpr = readOrThrow B.bonlangParser

readOrThrow :: (MonadError B.BonlangError m, Stream s Identity t)
            => Parsec s () a -> s -> m a
readOrThrow parser input = case parse parser "bonlang" input of
                              Left e  -> throwError $ B.Parser e
                              Right x -> return x

evalString :: String -> IO (Either B.BonlangError B.BonlangValue)
evalString expr = runExceptT $ B.liftThrows (readExpr expr)

main :: IO ()
main = do
    file <- fmap head getArgs
    c    <- readFile file
    d    <- evalString c
    print d
