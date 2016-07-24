module Bonlang.Lexer where

import           Bonlang.Lang          as Lang
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

string :: BonlangParsec u Lang.BonlangValue
string = Lang.BonlangString . T.pack <$> P.stringLiteral bonlang

number :: BonlangParsec u Lang.BonlangValue
number = Lang.BonlangNumber <$> P.naturalOrFloat bonlang

boolean :: BonlangParsec u Lang.BonlangValue
boolean =  (reserved "true"  >> return (Lang.BonlangBool True))
       <|> (reserved "false" >> return (Lang.BonlangBool False))

operator :: BonlangParsec u String
operator = P.operator bonlang

reservedOp :: String -> BonlangParsec u ()
reservedOp = P.reservedOp bonlang

identifier :: BonlangParsec u String
identifier = P.identifier bonlang

refIdentifier :: BonlangParsec u Lang.BonlangValue
refIdentifier =  Lang.BonlangRefLookup <$> identifier

list :: BonlangParsec u Lang.BonlangValue
list = Lang.BonlangList <$> brackets (P.commaSep bonlang expression)

whiteSpace :: BonlangParsec u ()
whiteSpace = P.whiteSpace bonlang

paramsList :: BonlangParsec u [String]
paramsList = brackets $ P.commaSep bonlang identifier

closure :: BonlangParsec u BonlangValue
closure = do _ <- lexeme $ reserved "lambda"
             params <- lexeme $ paramsList
             _ <- reservedOp "=>"
             bExp   <- simpleExpression
             return Lang.BonlangClosure { cParams = params
                                        , cBody   = bExp
                                        , cEnv    = Map.fromList []
                                        }

conditional :: BonlangParsec u Lang.BonlangValue
conditional = do reserved "if"
                 condition <- simpleExpression
                 reserved "then"
                 trueExpr  <- simpleExpression
                 reserved "else"
                 falseExpr <- simpleExpression
                 return Lang.BonlangIfThenElse { predicate  = condition
                                               , valueTrue  = trueExpr
                                               , valueFalse = falseExpr
                                               }

aliasAssign :: BonlangParsec u Lang.BonlangValue
aliasAssign = do reserved "val"
                 aName  <- identifier
                 reservedOp "="
                 aValue <- simpleExpression
                 return Lang.BonlangAlias { aliasName       = aName
                                          , aliasExpression = aValue
                                          }

-- TODO: This is fugly
instructionBlock :: BonlangParsec u Lang.BonlangValue
instructionBlock
  = lexeme $ do _ <- symbol "{"
                e <- actions `Parsec.sepEndBy` symbol ";"
                _ <- symbol "}"
                return e
    >>= \x -> return Lang.BonlangBlock { instructions = x }
    where
        actions = do whiteSpace
                     lexeme $ aliasAssign <|> simpleExpression

functionDef :: (String -> [String] -> BonlangParsec u a) -> BonlangParsec u a
functionDef f = Parsec.try $ do reserved "def"
                                funName   <- lexeme identifier
                                funParams <- lexeme paramsList
                                _ <- lexeme $ Parsec.char '='
                                Parsec.try $ f funName funParams

pureFunctionDef :: BonlangParsec u Lang.BonlangValue
pureFunctionDef
  = functionDef $ \funName funParams -> do
      fDef' <- expression
      defAt <- Parsec.getPosition
      return BonlangFunc { fName       = funName
                         , fParameters = funParams
                         , fDef        = Lang.BonlangClosure
                             { cParams = funParams
                             , cBody   = fDef'
                             , cEnv    = Map.fromList []
                             }
                         , fDefinedAt  = defAt
                         }

-- The idea is to have more than just functions in the module
-- in the future... but ok like this for now
moduleDef :: BonlangParsec u Lang.BonlangValue
moduleDef
  = do pos <- Parsec.getPosition
       reserved "module"
       mName <- identifier
       reserved "where"
       fs <- Parsec.manyTill pureFunctionDef Parsec.eof
       return Lang.BonlangDirective
           { directive = Lang.ModuleDef
                         { moduleName  = mName
                         , moduleItems = Map.fromList (map nameAndValue fs)
                         , mDefinedAt  = pos
                         }
           }
  where
    nameAndValue :: Lang.BonlangValue -> (String, Lang.BonlangValue)
    nameAndValue x@(Lang.BonlangFunc _ _ _ _) = (Lang.fName x, x)
    nameAndValue _ = error "Panic: This should never happen"


functionApply :: BonlangParsec u Lang.BonlangValue
functionApply = do funName   <- Parsec.try $ refIdentifier <* reservedOp "$"
                   funParams <- Parsec.many simpleExpression
                   return Lang.BonlangFuncApply { fResolver = funName
                                                , fParams   = funParams
                                                }

scalarExpression :: BonlangParsec u Lang.BonlangValue
scalarExpression =  parenthesis scalarExpression
                <|> string
                <|> number
                <|> list
                <|> boolean

simpleExpression :: BonlangParsec u Lang.BonlangValue
simpleExpression =  parenthesis simpleExpression
                <|> scalarExpression
                <|> functionApply
                <|> refIdentifier
                <|> conditional
                <|> instructionBlock
                <|> closure

expression :: BonlangParsec u Lang.BonlangValue
expression =  parenthesis expression
          <|> simpleExpression
          <|> aliasAssign
          <|> pureFunctionDef

bonlangParser :: BonlangParsec u Lang.BonlangValue
bonlangParser = whiteSpace >> (moduleDef <|> expression)
