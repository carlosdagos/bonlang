{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE Strict              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bonlang.Lang
    ( BonlangValue(..)
    , BonlangError(..)
    , BonlangDirectiveType(..)
    , ParamsList
    , ThrowsError
    , IOThrowsException
    , BonlangNum
    ) where

import           Control.Monad.Trans.Except    as Except
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified System.IO                     as IO
import           Text.Parsec                   (SourcePos)
import           Text.ParserCombinators.Parsec hiding (spaces)

-------------------------------------------------------------------------------
-- | A parameter list is just a list of strings
type ParamsList        = [String]
type ThrowsError       = Either BonlangError
type IOThrowsException = Except.ExceptT BonlangError IO.IO
type BonlangNum        = Either Integer Double
type PrimFunc          = [BonlangValue] -> ThrowsError BonlangValue
type PrimIOFunc        = [BonlangValue] -> IOThrowsException BonlangValue
type Bindings          = M.Map String BonlangValue

data BonlangDirectiveType
    = ModuleDef { moduleName    :: String
                , moduleImports :: [BonlangDirectiveType]
                , moduleItems   :: Bindings
                , mDefinedAt    :: SourcePos
                }
   | ModuleImport { moduleName :: String }
   | NoOp
   deriving (Show)

-------------------------------------------------------------------------------
-- | All the possible value types
data BonlangValue = BonlangList       { unList :: [BonlangValue] }
                  | BonlangNumber     { unNumber :: BonlangNum }
                  | BonlangString     { unString :: T.Text }
                  | BonlangBool       { unBool :: Bool }
                  | BonlangIfThenElse { condition  :: BonlangValue
                                      , valueTrue  :: BonlangValue
                                      , valueFalse :: BonlangValue
                                      }
                  | BonlangClosure    { cParams :: ParamsList
                                      , cEnv    :: Bindings
                                      , cBody   :: BonlangValue
                                      }
                  | BonlangPrimFunc   { fPrimDef :: PrimFunc }
                  | BonlangPrimIOFunc { fPrimIODef :: PrimIOFunc }
                  | BonlangBlock      { instructions :: [BonlangValue] }
                  | BonlangFuncApply  { fResolver :: BonlangValue
                                      , fParams   :: [BonlangValue]
                                      }
                  | BonlangAlias      { aliasName       :: String
                                      , aliasExpression :: BonlangValue
                                      }
                  | BonlangRefLookup  { referenceName :: String }
                  | BonlangDirective  { directive :: BonlangDirectiveType }
                  deriving (Show)

-------------------------------------------------------------------------------
-- | Different types of errors we can get
data BonlangError = NumArgs              Int        [BonlangValue]
                  | UnboundReference     String
                  | TypeMismatch         String     BonlangValue
                  | InternalTypeMismatch String     [BonlangValue]
                  | Parser               ParseError
                  | NotFunction          String     String
                  | DefaultError         String
                  deriving (Show)

instance Eq BonlangValue where
    x == y = isEqual x y

isEqual :: BonlangValue -> BonlangValue -> Bool
isEqual (BonlangString x) (BonlangString y) = x == y
isEqual (BonlangNumber x) (BonlangNumber y) = x == y
isEqual (BonlangBool x)   (BonlangBool y)   = x == y
isEqual (BonlangList x)   (BonlangList y)   = x == y
isEqual x y                                 = error $ "Can't do this\n" ++ show x ++ "\n" ++ show y

instance Show PrimFunc where
    show _ = "<bonlang:primitive function>"

instance Show PrimIOFunc where
    show _ = "<bonlang:primitive io function>"

instance Show BonlangNum where
    show (Left x)  = show x
    show (Right x) = show x

instance Num BonlangNum where
    Left x  + Left y  = Left $ x + y
    Right x + Right y = Right $ x + y
    Left x  + Right y = Right $ fromIntegral x + y
    Right x + Left y  = Right $ x + fromIntegral y

    Left x  - Left y  = Left $ x - y
    Right x - Right y = Right $ x - y
    Left x  - Right y = Right $ fromIntegral x - y
    Right x - Left y  = Right $ x - fromIntegral y

    Left x  * Left y  = Left $ x * y
    Right x * Right y = Right $ x * y
    Left x  * Right y = Right $ fromIntegral x * y
    Right x * Left y  = Right $ x * fromIntegral y

    abs (Left x)     = Left  $ abs x
    abs (Right x)    = Right $ abs x
    signum (Left x)  = Left  $ signum x
    signum (Right x) = Right $ signum x
    fromInteger      = Left

instance Fractional BonlangNum where
    Left x  / Left y  = Right $ fromIntegral x / fromIntegral y
    Right x / Right y = Right $ x / y
    Left x  / Right y = Right $ fromIntegral x / y
    Right x / Left y  = Right $ x / fromIntegral y
    fromRational x    = Right $ fromRational x

instance Real BonlangNum where
    toRational (Left x)  = toRational x
    toRational (Right x) = toRational x

instance Enum BonlangNum where
    toEnum x           = Left $ toInteger x
    fromEnum (Left x)  = fromEnum x
    fromEnum (Right x) = fromEnum x

instance Integral BonlangNum where
    quotRem (Left x) (Left y) = let (x', y') = quotRem x y
                                in (Left x', Left y')
    quotRem _ _         = error "Can't do this"
    toInteger (Left x)  = toInteger x
    toInteger (Right x) = toInteger (truncate x :: Integer)
