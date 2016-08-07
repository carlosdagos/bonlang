{-# LANGUAGE OverloadedStrings #-}

module Bonlang.Runtime
    ( Scope
    , BonHandle(..)
    , OutputHandle
    , InputHandle
    , eval
    , startEval
    , nullScope
    , getReference
    , isReferenceDefined
    , defineReference
    , primitiveBindings
    , liftThrows
    ) where

import           Bonlang.ErrorMessages
import           Bonlang.Lang
import           Bonlang.Lang.Types
import           Bonlang.Runtime.Primitives
import           Control.Monad.IO.Class     (liftIO)
import qualified Control.Monad.Loops        as Loops
import           Control.Monad.Trans.Except as Except
import qualified Data.IORef                 as IORef
import qualified Data.Map                   as M
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromJust, isJust)
import qualified System.IO                  as IO

-- | Adhoc types to use as phantoms for handles, useful for testing input and
-- | output from specific IO sources
data OutputHandle
data InputHandle
data BonHandle a  = BonHandle IO.Handle

type Bindings = M.Map String BonlangValue
type Scope    = IORef.IORef Bindings

nullScope :: IO.IO Scope
nullScope = IORef.newIORef $ Map.fromList []

getReference :: Scope -> BonlangValue -> IOThrowsException BonlangValue
getReference scope (BonlangRefLookup name)
  = do s <- liftIO $ IORef.readIORef scope
       let maybeVal = Map.lookup name s
       maybe (Except.throwE $ UnboundReference name) return maybeVal
getReference _ value
  = Except.throwE $ InternalTypeMismatch "Can't lookup" [value]

isReferenceDefined :: Scope -> String -> IO Bool
isReferenceDefined scope name
  = fmap (isJust . Map.lookup name) (IORef.readIORef scope)

defineReference :: Scope
                -> BonlangValue
                -> IOThrowsException BonlangValue
defineReference scope x
  = case x of
      (BonlangAlias name value) -> define' name value
      bad                       -> error' bad
  where
      define' name value =
            do alreadyDefined <- liftIO $ isReferenceDefined scope name
               if alreadyDefined
                  then Except.throwE $ DefaultError "alreadyDefined"
                  else liftIO $ do s <- IORef.readIORef scope
                                   IORef.writeIORef scope $
                                       Map.insert name value s
                                   return value
      error' bad = Except.throwE $
          InternalTypeMismatch "Can't define reference" [bad]

primitiveBindings :: BonHandle InputHandle -> BonHandle OutputHandle -> IO Scope
primitiveBindings (BonHandle _) (BonHandle out)
  = nullScope >>= flip bindVars (Map.fromList primitives')
    where
      makeFunc c (v, f) = (v, c f)
      primitives' :: [(String, BonlangValue)]
      primitives' = fmap (makeFunc BonlangPrimIOFunc) (ioPrimitives out)
                ++ fmap (makeFunc BonlangPrimFunc) primitives

bindVars :: Scope -> Map.Map String BonlangValue -> IO Scope
bindVars scope bindings
  = do s <- IORef.readIORef scope
       r <- extendScope (Map.toList bindings) (Map.toList s)
       IORef.newIORef r
    where
        extendScope bds s = fmap (Map.fromList . (++ s)) (mapM return bds)

startEval :: Scope -> BonlangValue -> IOThrowsException BonlangValue
startEval s (BonlangDirective dir@(ModuleDef m _ is at))
  = if m == "Main"
       then if hasMain is
               then evalDirective s dir
               else Except.throwE $ noMainFunction at
               else Except.throwE $ noMainModule at
    where
        hasMain moduleDefs = isJust $ Map.lookup "main" moduleDefs
startEval _ x = Except.throwE $ cantStartNonModule x

eval :: Scope -> BonlangValue -> IOThrowsException BonlangValue
eval _ x@BonlangString {}      = return x
eval _ x@BonlangNumber {}      = return x
eval _ x@BonlangBool {}        = return x
eval s (BonlangDirective dir)  = evalDirective s dir
eval s (BonlangBlock is)       = do l <- last <$> sequence (fmap (eval s) is)
                                    evalUntilReduced s l
eval s (BonlangList xs)        = evalList s xs
eval s x@BonlangRefLookup {}   = getReference s x
eval _ x@BonlangPrimIOFunc {}  = return x
eval _ x@BonlangPrimFunc {}    = return x
eval s (BonlangFuncApply x@BonlangPrimFunc {} ps)
  = evalList s ps >>= runPrim x s
eval s (BonlangFuncApply x@BonlangPrimIOFunc {} ps)
  = evalList s ps >>= runPrim x s
eval s (BonlangFuncApply x@BonlangClosure {} ps)
   = evalList s ps >>= evalClosure s x . unList
eval s (BonlangFuncApply r ps)
  = do ref' <- getReference s r
       if isReduced ref'
          then do evaledPs <- evalList s ps
                  if isPrimFunction ref'
                     then runPrim ref' s evaledPs
                     else evalClosure s ref' ps
          else do evald' <- eval s ref'
                  eval s (BonlangFuncApply evald' ps)
eval s x@BonlangAlias {}
  = if aliasName x == "main"
       then eval s (aliasExpression x)
       else defineReference s x
eval _ x
  = Except.throwE $ InternalTypeMismatch "Don't know how to eval" [x]

evalList :: Scope -> [BonlangValue] -> IOThrowsException BonlangValue
evalList s xs = do xs' <- sequence $ evalUntilReduced s <$> xs
                   return $ BonlangList xs'

evalUntilReduced :: Scope -> BonlangValue -> IOThrowsException BonlangValue
evalUntilReduced s = Loops.iterateUntilM isReduced (eval s)

isReduced :: BonlangValue -> Bool
isReduced (BonlangList xs) = all isReduced xs
isReduced x = isScalar x || isFunction x || isPrimFunction x

evalClosure :: Scope
            -> BonlangValue
            -> [BonlangValue]
            -> IOThrowsException BonlangValue
evalClosure s c@BonlangClosure {} ps
  = do s' <- liftIO $ IORef.readIORef s
       if notEnoughParams
          then return BonlangClosure { cParams = cParams c
                                     , cEnv    = Map.union (cEnv c) newParams'
                                     , cBody   = cBody c
                                     }
          else if tooManyParams
               then Except.throwE $ DefaultError "Too many params applied"
               else do let cBody' = cBody c
                       let rScope = Map.union s' newParams'
                       s'' <- liftIO $ IORef.newIORef rScope
                       case cBody' of
                         BonlangClosure {}    -> evalClosure s'' cBody' newArgs
                         _                    -> eval s'' cBody'
    where
        notEnoughParams = existingArgs + length ps < length (cParams c)
        tooManyParams   = existingArgs + length ps > length (cParams c)
        existingArgs    = length (Map.toList (cEnv c))
        newParams'      = Map.fromList (zip (drop existingArgs $ cParams c) ps)
        newArgs         = map snd $ Map.toList newParams'
evalClosure _ x _
  = Except.throwE $ InternalTypeMismatch "Can't eval non closure" [x]

runPrim :: BonlangValue
        -> Scope
        -> BonlangValue
        -> IOThrowsException BonlangValue
runPrim (BonlangPrimIOFunc f) s (BonlangList ps) = do params <- mapM (eval s) ps
                                                      f params
runPrim (BonlangPrimFunc f) s (BonlangList ps)   = do params <- mapM (eval s) ps
                                                      liftThrows $ f params
runPrim x _ _
  = Except.throwE $
       TypeMismatch "Can't run primary function application on non-primary" x

evalDirective :: Scope -> BonlangDirectiveType -> IOThrowsException BonlangValue
evalDirective s (ModuleDef _ _ is _)
  = do sequence_ (fmap (\(_, f) -> defineReference s f) is')
       evalClosure s (aliasExpression . fromJust $ Map.lookup "main" is) []
    where
        is' = filter (\(f,_) -> f /= "main") (Map.toList is)
evalDirective _ (ModuleImport _) = error "TODO: Module imports"
evalDirective _ _
  = Except.throwE
      $ InternalTypeMismatch "Attempting eval directive for undefined" []

liftThrows :: ThrowsError a -> IOThrowsException a
liftThrows (Left err) = Except.throwE err
liftThrows (Right x)  = return x
