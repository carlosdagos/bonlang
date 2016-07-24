import           Bonlang.Lang
import           Bonlang.Lexer
import           Control.Monad
import           System.Directory
import           System.FilePath
import           Text.Parsec
import           Text.ParserCombinators.Parsec

testFiles :: IO [FilePath]
testFiles = do
    let testDir = "test/examples/"
    files <- getDirectoryContents testDir
    let filtered = filter (\f -> takeExtension f == ".bl") files
    return $ map (testDir ++) filtered

parse' :: SourceName -> IO (Either ParseError BonlangValue)
parse' = parseFromFile bonlangParser

printResult :: Either ParseError BonlangValue -> IO ()
printResult (Left e)  = print e
printResult (Right e) = putStrLn ""

main :: IO ()
main = testFiles >>= mapM_ (parse' >=> printResult)

