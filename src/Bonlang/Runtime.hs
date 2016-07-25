{-# LANGUAGE OverloadedStrings #-}

module Bonlang.Runtime
    ( Scope
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
import           Control.Monad.Trans.Except as Except
import qualified Data.IORef                 as IORef
import qualified Data.Map                   as M
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromJust, isJust)
import qualified System.IO                  as IO

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
      (BonlangFunc fn _ fdef _) -> define' fn fdef
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

primitiveBindings :: IO Scope
primitiveBindings = nullScope >>= flip bindVars (Map.fromList primitives')
    where
        makeFunc c (v, f) = (v, c f)
        primitives'       = fmap (makeFunc BonlangPrimIOFunc) ioPrimitives
                         ++ fmap (makeFunc BonlangPrimFunc) primitives

bindVars :: Scope -> Map.Map String BonlangValue -> IO Scope
bindVars scope bindings
  = do s <- IORef.readIORef scope
       r <- extendScope (Map.toList bindings) (Map.toList s)
       IORef.newIORef r
    where
        extendScope bdings s = fmap (Map.fromList . (++ s)) (mapM addBinding bdings)
        addBinding t@(_, _)  = return t

startEval :: Scope -> BonlangValue -> IOThrowsException BonlangValue
startEval s (BonlangDirective dir@(ModuleDef m is at))
  = if m == "Main"
       then if hasMain is
               then evalDirective s dir
               else Except.throwE $ noMainFunction at
               else Except.throwE $ noMainModule at
    where
        hasMain moduleDefs = isJust $ Map.lookup "main" moduleDefs
startEval _ _ = Except.throwE cantStartNonModule

eval :: Scope -> BonlangValue -> IOThrowsException BonlangValue
eval s (BonlangDirective dir)  = evalDirective s dir
eval s (BonlangBlock is)       = do l' <- last <$> sequence (fmap (eval s) is)
                                    eval s l'
eval s ref@BonlangRefLookup {} = getReference s ref
eval _ x@BonlangString {}      = return x
eval _ x@BonlangNumber {}      = return x
eval _ x@BonlangBool {}        = return x
eval _ x@BonlangPrimFunc {}    = return x
eval _ x@BonlangPrimIOFunc {}  = return x
eval s (BonlangList xs)        = do xs' <- sequence $ fmap (eval s) xs
                                    if all isScalar xs'
                                       then return $ BonlangList xs'
                                       else eval s (BonlangList xs')
eval s x@BonlangAlias {}       = defineReference s x
eval s (BonlangFuncApply r ps)
  = do resolved <- eval s r
       if isPrimFunction resolved
          then runPrim resolved s ps
          else case resolved of
                 x@BonlangClosure {} -> evalClosure s x ps
                 _ -> error "TODO: func apply for non primary"
eval s f@BonlangFunc {}
  = if fName f == "main"
       then eval s (fDef f)
       else defineReference s f
eval _ x = Except.throwE $ InternalTypeMismatch "Don't know how to eval" [x]

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
               else do let rScope = Map.union s' newParams'
                       s'' <- liftIO $ IORef.newIORef rScope
                       eval s'' (cBody c)
    where
        notEnoughParams = existingArgs + length ps < length (cParams c)
        tooManyParams   = existingArgs + length ps > length (cParams c)
        existingArgs    = length (Map.toList (cEnv c))
        newParams'      = Map.fromList (zip (drop existingArgs $ cParams c) ps)
evalClosure _ x _
  = Except.throwE $ InternalTypeMismatch "Can't eval non closure" [x]

runPrim :: BonlangValue
        -> Scope
        -> [BonlangValue]
        -> IOThrowsException BonlangValue
runPrim (BonlangPrimIOFunc f) s ps = do params <- mapM (eval s) ps
                                        f params
runPrim (BonlangPrimFunc f) s ps   = do params <- mapM (eval s) ps
                                        liftThrows $ f params
runPrim x _ _
  = Except.throwE $ TypeMismatch "Can't run primary on non-primary" x

evalDirective :: Scope -> BonlangDirectiveType -> IOThrowsException BonlangValue
evalDirective s (ModuleDef _ is _)
  = do sequence_ (fmap (\(_, f) -> defineReference s f) is')
       evalClosure s (fDef . fromJust $ Map.lookup "main" is) []
    where
        is' = filter (\(f,_) -> f /= "main") (Map.toList is)
evalDirective _ (ModuleImport _) = error "TODO: Module imports"
evalDirective _ _                = error "TODO: eval directive undefined"

liftThrows :: ThrowsError a -> IOThrowsException a
liftThrows (Left err) = Except.throwE err
liftThrows (Right x)  = return x

