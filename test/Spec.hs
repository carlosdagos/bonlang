{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Bonlang.Lang
import           Bonlang.Lexer
import           Data.Aeson
import           Data.Maybe                    (fromJust)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Yaml                     as Y
import           GHC.Generics
import           System.Directory
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.ParserCombinators.Parsec

data TestFile = TestFile { meta :: T.Text
                         , code :: T.Text
                         }
                         deriving (Show)

data TestInputOutput = TestInputOutput { output :: String
                                       }
                                       deriving (Generic, Show)

instance FromJSON TestInputOutput
instance ToJSON TestInputOutput

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = do p <- sequence [parsing, testExpectations]
           return $ testGroup "Bonlang" p

testExpectations :: IO TestTree
testExpectations = getFiles "test/examples/" >>=
    \files -> return $ testGroup "Example tests" $ map exampleFiles files


exampleFiles :: FilePath -> TestTree
exampleFiles f = testCase ("Example file: " `mappend` f) exampleAssertion
    where
        exampleAssertion :: Assertion
        exampleAssertion = do testFile <- makeTestFile f
                              case testFile of
                                Right testF -> runTestFile testF
                                Left  x     -> error x

runTestFile :: TestFile -> Assertion
runTestFile testFile
  = do output'         <- programOutput
       expectedOutput' <- expectedOutput
       output' @?= output (fromJust expectedOutput')
    where
        programOutput = return "" :: IO String
        expectedOutput
          = let tioSource = E.encodeUtf8 $ meta testFile in
            return $ Y.decode tioSource :: IO (Maybe TestInputOutput)

makeTestFile :: FilePath -> IO (Either String TestFile)
makeTestFile f = do fileContents <- T.pack <$> readFile f
                    case T.splitOn "---" fileContents of
                      [_, meta', code']
                            ->  return $ Right TestFile { meta = meta'
                                                        , code = code'
                                                        }
                      _ -> return $ Left "Invalid syntax for example file"

parsing :: IO TestTree
parsing = getFiles "test/parser_tests/" >>=
    \files -> return $ testGroup "Parsing tests" $ map testCaseFiles files

testCaseFiles :: String -> TestTree
testCaseFiles f = testCase ("Parsing file: " `mappend` f) fileAssertion
                  where
                      fileAssertion :: Assertion
                      fileAssertion = do (_, b) <- parse' f
                                         b @?= True

parse' :: String -> IO (String, Bool)
parse' f = parseFromFile bonlangParser f >>= \r -> return (f, parsedCorr r)
           where
                parsedCorr :: Either ParseError BonlangValue -> Bool
                parsedCorr (Left _)  = False
                parsedCorr (Right _) = True

getFiles :: FilePath -> IO [String]
getFiles dir = do files <- getDirectoryContents dir
                  let filtered = filter (\f -> takeExtension f == ".bl") files
                  return $ map (dir ++) filtered
