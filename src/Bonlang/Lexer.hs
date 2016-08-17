module Bonlang.Lexer where

import qualified Bonlang.Lang          as L
import           Data.Functor.Identity
import qualified Data.Map              as Map
import qualified Data.Text             as T
import qualified Text.Parsec           as Parsec
import           Text.Parsec.Language
import           Text.Parsec.Prim
import qualified Text.Parsec.Token     as P

symbols :: String
symbols = ":!#$%&*+./<=>?@\\^|-~λ¬×≈"

bonlangStyle :: LanguageDef st
bonlangStyle = emptyDef
    { P.commentStart    = "/*"
    , P.commentEnd      = "*/"
    , P.commentLine     = "//"
    , P.nestedComments  = True
    , P.identStart      = Parsec.oneOf symbols
                      <|> Parsec.letter
    , P.identLetter     = Parsec.alphaNum
                      <|> Parsec.oneOf "_-'"
                      <|> Parsec.oneOf symbols
    , P.opStart         = P.opLetter bonlangStyle
    , P.opLetter        = Parsec.oneOf symbols
    , P.reservedOpNames = [ "=", ".", "$", "=>" ]
    , P.reservedNames   = [ "if",     "then",     "else"
                          , "case",   "of"
                          , "typeof", "type",     "class"
                          , "do",     "import"
                          , "infix",  "infix1"
                          , "infixr", "instance", "lambda"
                          , "module", "where"
                          , "var",    "val",      "def"
                          , "true",   "false"
                          , "return"
                          ]
    , P.caseSensitive   = True
    }

bonlang :: P.TokenParser st
bonlang = P.makeTokenParser bonlangStyle

-------------------------------------------------------------------------------
-- | Parsec alias
type BonlangParsec u a = Parsec.ParsecT String u Identity a

symbol :: String -> BonlangParsec u String
symbol = P.symbol bonlang

reserved :: String -> BonlangParsec u ()
reserved = P.reserved bonlang

lexeme :: BonlangParsec u a -> BonlangParsec u a
lexeme = P.lexeme bonlang

parenthesis :: BonlangParsec u a -> BonlangParsec u a
parenthesis = P.parens bonlang

brackets :: BonlangParsec u a -> BonlangParsec u a
brackets = P.brackets bonlang

braces :: BonlangParsec u a -> BonlangParsec u a
braces = P.braces bonlang

string :: BonlangParsec u L.BonlangValue
string = L.BonlangString . T.pack <$> P.stringLiteral bonlang

number :: BonlangParsec u L.BonlangValue
number = L.BonlangNumber <$> P.naturalOrFloat bonlang

boolean :: BonlangParsec u L.BonlangValue
boolean =  (reserved "true"  >> return (L.BonlangBool True))
       <|> (reserved "false" >> return (L.BonlangBool False))

operator :: BonlangParsec u String
operator = P.operator bonlang

reservedOp :: String -> BonlangParsec u ()
reservedOp = P.reservedOp bonlang

identifier :: BonlangParsec u String
identifier = P.identifier bonlang

refIdentifier :: BonlangParsec u L.BonlangValue
refIdentifier =  L.BonlangRefLookup <$> identifier

list :: BonlangParsec u L.BonlangValue
list = L.BonlangList <$> brackets (P.commaSep bonlang expression)

whiteSpace :: BonlangParsec u ()
whiteSpace = P.whiteSpace bonlang

paramsList :: BonlangParsec u L.ParamsList
paramsList = brackets $ P.commaSep bonlang identifier

closure :: BonlangParsec u L.BonlangValue
closure = do _ <- lexeme $ reserved "lambda"
             params <- lexeme paramsList
             _ <- reservedOp "=>"
             bExp   <- simpleExpression
             return L.BonlangClosure { L.cParams = params
                                     , L.cBody   = bExp
                                     , L.cEnv    = Map.empty
                                     }

conditional :: BonlangParsec u L.BonlangValue
conditional = do reserved "if"
                 condition <- simpleExpression
                 reserved "then"
                 trueExpr  <- simpleExpression
                 reserved "else"
                 falseExpr <- simpleExpression
                 return L.BonlangIfThenElse { L.condition  = condition
                                            , L.valueTrue  = trueExpr
                                            , L.valueFalse = falseExpr
                                            }

aliasAssign :: BonlangParsec u L.BonlangValue
aliasAssign = do reserved "val"
                 aName  <- identifier
                 reservedOp "="
                 aValue <- simpleExpression
                 return L.BonlangAlias { L.aliasName       = aName
                                       , L.aliasExpression = aValue
                                       }

-- TODO: This is fugly
instructionBlock :: BonlangParsec u L.BonlangValue
instructionBlock
  = lexeme $ do _ <- symbol "{"
                e <- actions `Parsec.sepEndBy` symbol ";"
                _ <- symbol "}"
                return e
    >>= \x -> return L.BonlangBlock { L.instructions = x }
    where
        actions = do whiteSpace
                     lexeme $ aliasAssign <|> simpleExpression

functionDef :: (String -> [String] -> BonlangParsec u a) -> BonlangParsec u a
functionDef f = Parsec.try $ do reserved "def"
                                funName   <- lexeme identifier
                                funParams <- lexeme paramsList
                                _ <- lexeme $ Parsec.char '='
                                Parsec.try $ f funName funParams

pureFunctionDef :: BonlangParsec u L.BonlangValue
pureFunctionDef
  = functionDef $ \funName funParams -> do
      fDef' <- expression
      return L.BonlangAlias { L.aliasName       = funName
                            , L.aliasExpression = L.BonlangClosure
                                 { L.cParams = funParams
                                 , L.cBody   = fDef'
                                 , L.cEnv    = Map.empty
                                 }
                            }

-- The idea is to have more than just functions in the module
-- in the future... but ok like this for now
moduleDef :: BonlangParsec u L.BonlangValue
moduleDef
  = do pos <- Parsec.getPosition
       reserved "module"
       mName <- identifier
       reserved "where"
       mImports <- Parsec.many importStatements
       fs       <- Parsec.manyTill pureFunctionDef Parsec.eof
       return L.BonlangDirective
           { L.directive = L.ModuleDef
               { L.moduleName    = mName
               , L.moduleImports = mImports
               , L.moduleItems   = Map.fromList (map nameAndValue fs)
               , L.mDefinedAt    = pos
               }
           }
  where
    nameAndValue :: L.BonlangValue -> (String, L.BonlangValue)
    nameAndValue x@L.BonlangAlias {} = (L.aliasName x, x)
    nameAndValue _ = error "Panic: This should never happen"
    importStatements :: BonlangParsec u L.BonlangDirectiveType
    importStatements = L.ModuleImport <$> (reserved "import" *> identifier)

functionApply :: BonlangParsec u L.BonlangValue
functionApply = do funName   <- Parsec.try $ refIdentifier <* reservedOp "$"
                   funParams <- Parsec.many simpleExpression
                   return L.BonlangFuncApply { L.fResolver = funName
                                             , L.fParams   = funParams
                                             }

scalarExpression :: BonlangParsec u L.BonlangValue
scalarExpression =  parenthesis scalarExpression
                <|> string
                <|> number
                <|> list
                <|> boolean

simpleExpression :: BonlangParsec u L.BonlangValue
simpleExpression =  parenthesis simpleExpression
                <|> scalarExpression
                <|> functionApply
                <|> refIdentifier
                <|> conditional
                <|> instructionBlock
                <|> closure

expression :: BonlangParsec u L.BonlangValue
expression =  parenthesis expression
          <|> simpleExpression
          <|> aliasAssign
          <|> pureFunctionDef

bonlangParser :: BonlangParsec u L.BonlangValue
bonlangParser = whiteSpace >> (moduleDef <|> expression)
