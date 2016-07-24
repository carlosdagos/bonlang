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

type Bindings
  = M.Map String (IORef.IORef BonlangValue)
type Scope
  = IORef.IORef Bindings

nullScope :: IO.IO Scope
nullScope = IORef.newIORef $ Map.fromList []

getReference :: Scope -> BonlangValue -> IOThrowsException BonlangValue
getReference scope (BonlangRefLookup name)
  = do s <- liftIO $ IORef.readIORef scope
       maybe (Except.throwE $ UnboundReference name)
             (liftIO . IORef.readIORef)
             (Map.lookup name s)
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
                  else liftIO $ do valueRef <- IORef.newIORef value
                                   s        <- IORef.readIORef scope
                                   IORef.writeIORef scope $
                                       Map.insert name valueRef s
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
        extendScope bdings s
          = fmap (Map.fromList . (++ s)) (mapM addBinding bdings)
        addBinding (var, value)
          = do ref <- IORef.newIORef value
               return (var, ref)

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
eval s (BonlangBlock is)       = last <$> sequence (fmap (eval s) is)
eval s ref@BonlangRefLookup {} = getReference s ref >>= eval s
eval s x@BonlangAlias {}       = defineReference s x
eval s (BonlangFuncApply r ps)
  = do resolved <- eval s r
       if isPrimFunction resolved
          then runPrim resolved s ps
          else error "TODO: func apply for non primary"
eval s f@BonlangFunc {}
  = if fName f == "main"
       then eval s (fDef f)
       else defineReference s f
eval _ x
  = return x

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
  = do sequence_ (fmap (\(_, f) -> eval s f) (Map.toList is))
       eval s (fromJust $ Map.lookup "main" is)
evalDirective _ (ModuleImport _)
  = undefined
evalDirective _ _
  = undefined

liftThrows :: ThrowsError a -> IOThrowsException a
liftThrows (Left err) = Except.throwE err
liftThrows (Right x)  = return x

