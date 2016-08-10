{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import qualified Bonlang                       as B
import           Bonlang.Lang
import           Bonlang.Lexer
import           Control.Monad.Except
import           Data.Aeson
import           Data.Functor.Identity
import           Data.Maybe                    (fromJust)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Yaml                     as Y
import           GHC.Generics
import           System.Directory
import           System.FilePath
import           System.IO
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec
import           Text.ParserCombinators.Parsec

data TestFile = TestFile { meta :: T.Text
                         , code :: T.Text
                         , name :: String
                         }
                         deriving (Show)

data TestOuput = TestOuput { output :: String
                           }
                           deriving (Generic, Show)

instance FromJSON TestOuput
instance ToJSON TestOuput

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = do p <- sequence [parsing, testExpectations]
           return $ testGroup "Bonlang" p

--------------------------------------------------------------------------------
-- | Match running each example with the expected output
testExpectations :: IO TestTree
testExpectations = getFiles "test/examples/" >>=
    \files -> return $ testGroup "Example tests" $ map exampleFiles files

--------------------------------------------------------------------------------
-- | Parsing test tree
parsing :: IO TestTree
parsing = getFiles "test/parser_tests/" >>=
    \files -> return $ testGroup "Parsing tests" $ map testCaseFiles files

--------------------------------------------------------------------------------
-- | For an example file, get a test tree
exampleFiles :: FilePath -> TestTree
exampleFiles f = testCase ("Example file: " `mappend` f) exampleAssertion
    where
        exampleAssertion :: Assertion
        exampleAssertion = do testFile <- makeTestFile f
                              case testFile of
                                Right testF -> runTestFile testF
                                Left  x     -> error x

--------------------------------------------------------------------------------
-- | Read an expression
readExpr :: String -> B.ThrowsError B.BonlangValue
readExpr = readOrThrow B.bonlangParser

--------------------------------------------------------------------------------
-- | Read an expression or throw an error
readOrThrow :: (MonadError B.BonlangError m, Stream s Identity t)
            => Parsec s () a -> s -> m a
readOrThrow parser input = case parse parser "bonlang" input of
                              Left e  -> throwError $ B.Parser e
                              Right x -> return x

--------------------------------------------------------------------------------
-- | Eval a string
evalString :: B.Scope -> String -> IO (Either B.BonlangError B.BonlangValue)
evalString scope expr = runExceptT $ B.liftThrows (readExpr expr)
                    >>= B.startEval scope


--------------------------------------------------------------------------------
-- | For a TestFile, assert that running it returns the expected output
runTestFile :: TestFile -> Assertion
runTestFile testFile
  = do output'         <- programOutput
       expectedOutput' <- expectedOutput
       output' @?= output (fromJust expectedOutput')
    where
        programOutput
          = do (tempFile, tempH) <- createTmpFile

               let testInput  = B.BonHandle stdin
               let testOutput = B.BonHandle tempH

               s <- B.primitiveBindings testInput testOutput
               d <- evalString s ((T.unpack . code) testFile)
               hClose tempH
               case d of
                 Right _ -> readFile tempFile
                 Left  _ -> return $ show d

        expectedOutput
          = let tioSource = E.encodeUtf8 $ meta testFile in
            return $ Y.decode tioSource :: IO (Maybe TestOuput)

--------------------------------------------------------------------------------
-- | For a file path, get a TestFile value
makeTestFile :: FilePath -> IO (Either String TestFile)
makeTestFile f = do fileContents <- T.pack <$> readFile f
                    case T.splitOn "---" fileContents of
                      [_, meta', code']
                            ->  return $ Right TestFile { meta = meta'
                                                        , code = code'
                                                        , name = f
                                                        }
                      _ -> return $ Left "Invalid syntax for example file"

createTmpFile :: IO (FilePath, Handle)
createTmpFile = do tmpdir <- getTemporaryDirectory
                   openTempFile tmpdir ""

--------------------------------------------------------------------------------
-- | For a file name, parse it and assert that it was parsed correctly
testCaseFiles :: String -> TestTree
testCaseFiles f = testCase ("Parsing file: " `mappend` f) fileAssertion
                  where
                      fileAssertion :: Assertion
                      fileAssertion = do (_, b) <- parse' f
                                         b @?= True

--------------------------------------------------------------------------------
-- | Parse a file and return a pair of file name and Either
parse' :: String -> IO (String, Bool)
parse' f = parseFromFile bonlangParser f >>= \r -> return (f, parsedCorr r)
           where
                parsedCorr :: Either ParseError BonlangValue -> Bool
                parsedCorr (Left _)  = False
                parsedCorr (Right _) = True

--------------------------------------------------------------------------------
-- | Get files from a directory
getFiles :: FilePath -> IO [String]
getFiles dir = do files <- getDirectoryContents dir
                  let filtered = filter (\f -> takeExtension f == ".bl") files
                  return $ map (dir ++) filtered
