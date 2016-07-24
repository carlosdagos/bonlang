module Bonlang.Runtime.Primitives
    ( primitives
    , ioPrimitives
    ) where

import           Bonlang.Lang
import qualified Bonlang.Runtime.Bool    as Bool
import qualified Bonlang.Runtime.IO      as BonIO
import qualified Bonlang.Runtime.Lists   as Lists
import qualified Bonlang.Runtime.Numeric as Numeric
import qualified Bonlang.Runtime.Strings as Strings
import qualified Bonlang.Runtime.Types   as Types

primitives :: [(String, [BonlangValue] -> ThrowsError BonlangValue)]
primitives = [ ("is-scalar",   Types.isScalar')        -- Boolean primitives
             , ("is-string",   Types.isString')
             , ("is-number",   Types.isNumber')
             , ("is-integer",  Types.isInteger')
             , ("is-double",   Types.isDouble')
             , ("is-list",     Types.isList')
             , ("is-function", Types.isFunction')
             , ("eq",          Types.isEqual')
             , ("==",          Types.isEqual')
             , ("||",          Bool.or')
             , ("or",          Bool.or')
             , ("&&",          Bool.and')
             , ("and",         Bool.and')
             , ("not",         Bool.negate')
             , ("!",           Bool.negate')
             , ("+",           Numeric.add')           -- Numeric primitives
             , ("-",           Numeric.subtract')
             , ("*",           Numeric.multiply')
             , ("/",           Numeric.divide')
             , ("mod",         Numeric.modulo')
             , ("%",           Numeric.modulo')
             , ("gt",          Numeric.greaterThan')
             , (">",           Numeric.greaterThan')
             , ("gte",         Numeric.greaterThanOrEquals')
             , (">=",          Numeric.greaterThanOrEquals')
             , ("lt",          Numeric.lessThan')
             , ("<",           Numeric.lessThan')
             , ("lte",         Numeric.lessThanOrEquals')
             , ("<=" ,         Numeric.lessThanOrEquals')
             , ("++",          Strings.concat')         -- String primitives
             , ("concat",      Strings.concat')
             , ("to-string",   Strings.toString')
             ]

ioPrimitives :: [(String, [BonlangValue] -> IOThrowsException BonlangValue)]
ioPrimitives = [ ("print",   BonIO.print)
               , ("puts",    BonIO.puts)
               , ("puts-ln", BonIO.putsln)
               , ("map",     Lists.map')
               ]

