{-# LANGUAGE OverloadedStrings #-}

import           Bonlang.Lexer
import           System.Directory
import           System.FilePath
import           Test.Tasty.HUnit
import           Test.Tasty
import           Text.ParserCombinators.Parsec

main :: IO ()
main = do tests' <- tests
          defaultMain tests'

testFiles :: IO [String]
testFiles = do let testDir = "test/parser_tests/"
               files <- getDirectoryContents testDir
               let filtered = filter (\f -> takeExtension f == ".bl") files
               return $ map (testDir ++) filtered

parse' :: String -> IO (String, Bool)
parse' f = parseFromFile bonlangParser f >>= \r -> return (f, parsedCorr r)
    where
        parsedCorr (Left _)  = False
        parsedCorr (Right _) = True

tests :: IO TestTree
tests = parsing >>= \p -> return $ testGroup "Bonlang" [p]

parsing :: IO TestTree
parsing = testFiles >>= \files -> return $ testGroup "Parsing tests" $ map testCaseFiles files

testCaseFiles :: String -> TestTree
testCaseFiles f = testCase ("Parsing file: " `mappend` f) fileAssertion
    where
        fileAssertion :: Assertion
        fileAssertion = do (_, b) <- parse' f
                           b @?= True

