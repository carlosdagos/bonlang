module Bonlang.Runtime.Numeric
    ( add'
    , subtract'
    , multiply'
    , divide'
    , modulo'
    , greaterThan'
    , greaterThanOrEquals'
    , lessThan'
    , lessThanOrEquals'
    ) where

import           Bonlang.Lang
import           Bonlang.Lang.Numeric
import qualified Data.Map             as M
import           Prelude              hiding (subtract)

binaryNumericOp :: PrimFunc -> BonlangValue
binaryNumericOp op = BonlangClosure { cParams = ["n0", "n1"]
                                    , cEnv    = M.empty
                                    , cBody   = BonlangPrimFunc op
                                    }

add', subtract', multiply', divide', modulo'                     :: BonlangValue
greaterThan', greaterThanOrEquals', lessThan', lessThanOrEquals' :: BonlangValue

add'                 = binaryNumericOp add
subtract'            = binaryNumericOp subtract
multiply'            = binaryNumericOp multiply
divide'              = binaryNumericOp divide
modulo'              = binaryNumericOp modulo
greaterThan'         = binaryNumericOp greaterThan
greaterThanOrEquals' = binaryNumericOp greaterThanOrEquals
lessThan'            = binaryNumericOp lessThan
lessThanOrEquals'    = binaryNumericOp lessThanOrEquals
