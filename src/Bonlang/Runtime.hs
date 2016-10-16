{-# LANGUAGE MultiWayIf        #-}
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
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, isJust, isJust,
                                             isNothing)
import qualified System.IO                  as IO

-- | Adhoc types to use as phantoms for handles, useful for testing input and
-- | output from specific IO sources
data OutputHandle
data InputHandle
data BonHandle a  = BonHandle IO.Handle

type Bindings = M.Map String BonlangValue
type Scope    = IORef.IORef Bindings

nullScope :: IO.IO Scope
nullScope = IORef.newIORef M.empty

getReference :: Scope -> BonlangValue -> IOThrowsException BonlangValue
getReference scope (BonlangRefLookup name)
  = do s <- liftIO $ IORef.readIORef scope
       let maybeVal = M.lookup name s
       maybe (Except.throwE $ UnboundReference name) return maybeVal
getReference _ value
  = Except.throwE $ InternalTypeMismatch "Can't lookup" [value]

isReferenceDefined :: Scope -> String -> IO Bool
isReferenceDefined scope name = isJust . M.lookup name <$> IORef.readIORef scope

defineReference :: Scope -> BonlangValue -> IOThrowsException BonlangValue
defineReference scope x
  = case x of
      BonlangAlias name value -> define' name value
      bad                     -> error' bad
  where
      define' name value =
            do alreadyDefined <- liftIO $ isReferenceDefined scope name
               if alreadyDefined
                  then Except.throwE $ DefaultError "alreadyDefined"
                  else writeReference scope name value
      error' bad = Except.throwE $
          InternalTypeMismatch "Can't define reference" [bad]

writeReference :: Scope
               -> String
               -> BonlangValue
               -> IOThrowsException BonlangValue
writeReference scope name value
  = liftIO $ do s <- IORef.readIORef scope
                IORef.writeIORef scope $ M.insert name value s
                return value

primitiveBindings :: BonHandle InputHandle -> BonHandle OutputHandle -> IO Scope
primitiveBindings (BonHandle _) (BonHandle out)
  = nullScope >>= flip bindVars (M.fromList primitives')
    where
      primitives' :: [(String, BonlangValue)]
      primitives' = ioPrimitives out ++ primitives

bindVars :: Scope -> M.Map String BonlangValue -> IO Scope
bindVars scope bindings
  = do s <- IORef.readIORef scope
       r <- extendScope (M.toList bindings) (M.toList s)
       IORef.newIORef r
    where
        extendScope bds s = fmap (M.fromList . (++ s)) (mapM return bds)

startEval :: Scope -> BonlangValue -> IOThrowsException BonlangValue
startEval s (BonlangDirective dir@(ModuleDef m _ is at))
  = if m == "Main"
       then if hasMain is
               then evalDirective s dir
               else Except.throwE $ noMainFunction at
               else Except.throwE $ noMainModule at
    where
        hasMain :: Bindings -> Bool
        hasMain moduleDefs = isJust $ M.lookup "main" moduleDefs
startEval _ x = Except.throwE $ cantStartNonModule x

eval :: Scope -> BonlangValue -> IOThrowsException BonlangValue
eval _ x@BonlangString {}      = return x
eval _ x@BonlangNumber {}      = return x
eval _ x@BonlangBool {}        = return x
eval _ x@BonlangClosure {}     = return x
eval s (BonlangDirective dir)  = evalDirective s dir
eval s (BonlangBlock is)       = do l <- last <$> sequence (fmap (eval s) is)
                                    evalUntilReduced s l
eval s (BonlangList xs)        = evalList s xs
eval s x@BonlangRefLookup {}   = getReference s x
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
                     else evalClosure s ref' (unList evaledPs)
          else do evald' <- eval s ref'
                  eval s (BonlangFuncApply evald' ps)
eval s x@BonlangAlias {}
  = if aliasName x == "main"
       then eval s (aliasExpression x)
       else defineReference s x
eval s x@BonlangIfThenElse {}
  = do x' <- evalUntilReduced s $ condition x
       case x' of
          BonlangBool True -> eval s $ valueTrue x
          _                -> eval s $ valueFalse x
-- TODO: Clean this up
-- TODO: This ranks on my "Top 10 ugliest code", pls forgive me
-- TODO: This isn't well tested!
eval s x@BonlangPatternMatch {}
  = do toMatch <- evalUntilReduced s (match x)
       let maybeClause = L.find (matchingClause toMatch) (clauses x)
       case maybeClause of
         Just clause  -> evalMatchedClause s clause toMatch
         Nothing      -> Except.throwE $ DefaultError "No match"
  where
    matchingClause val ( p, _ ) = matchValue val p
    matchValue val p =
      case p of
          ScalarPattern scalar -> scalar == val
          ListPattern xs rest  -> case val of
                                     BonlangList ys -> matchList xs rest ys
                                     _              -> False
          ReferencePattern _   -> True
          RegexPattern _       -> undefined
          WildcardPattern      -> True
    matchList xs rest ys
      -- Trying to match a smaller list
      -- i.e. `match [1, 2] { [x, y, z] -> ... }` does not match
      | isNothing rest = length xs == length ys
      -- Matching exact length lists, or shorter, values must
      -- match, and `rest` can be present
      -- i.e.
      --   `match [1, 2] { [x, y] -> ... } => x = 1, y = 2`
      --   `match [1, 2, 3] { [ x, y | xs ] -> ... } => x = 1, y = 2, xs = [3]`
      | otherwise      = length ys >= length xs
                      && all (uncurry matchValue) (zip ys xs)
    evalMatchedClause s' (p, v) matchedValue =
      case p of
          ListPattern xs rest
            -> evalMatchedList s' xs rest matchedValue (eval s' v)
          _
            -> eval s' v
    -- val = [ 1, 2 ], pattern = [ x | xs ]
    --    => x = 1, xs = [2]
    -- val = [ 1, 2, 3 ], pattern = [ x, y | xs ]
    --    => x = 1, y = 2, xs = [3]
    -- val = [ 1, 2, 3 ], pattern = [ x, y, z | xs ]
    --    => x = 1, y = 2, z, = 3, xs = []
    -- val = [ 1, 2, 3 ], pattern = [ x | _ ]
    --    => x = 1
    evalMatchedList s' (ReferencePattern ref : xs) rest (BonlangList (y : ys)) end
      = writeReference s' ref y >> evalMatchedList s' xs rest (BonlangList ys) end
    evalMatchedList s' (_ : xs) rest (BonlangList (_ : ys)) end
      = evalMatchedList s' xs rest (BonlangList ys) end
    evalMatchedList s' [] (Just (ReferencePattern ref)) ys@(BonlangList _) end
      = writeReference s' ref ys >> end
    evalMatchedList _ [] Nothing (BonlangList _) end
      = end
    evalMatchedList _ [] (Just WildcardPattern) (BonlangList _) end
      = end
    evalMatchedList _ _ _ _ _
      = Except.throwE $ InternalTypeMismatch "Invalid match statement" []
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
       let u = M.union
       if | notEnoughParams ->
              return BonlangClosure { cParams = cParams c
                                     , cEnv    = cEnv c `u` newParams
                                     , cBody   = cBody c
                                     }
          | tooManyParams ->
              Except.throwE $ DefaultError "Too many params applied"
          | otherwise ->
                    do let cBody'   = cBody c
                       let newScope = newParams `u` cEnv c `u` s'
                       let primArgs = map snd $ M.toList $ cEnv c `u` newParams
                       s'' <- liftIO $ IORef.newIORef newScope
                       case cBody' of
                          f@BonlangPrimFunc {} ->
                              evalList s'' primArgs >>= runPrim f s''
                          g@BonlangPrimIOFunc {} ->
                              evalList s'' primArgs >>= runPrim g s''
                          BonlangClosure {} ->
                              evalClosure s'' cBody' newArgs
                          _ ->
                              eval s'' cBody'
    where
        notEnoughParams = existingArgs + length ps < length (cParams c)
        tooManyParams   = existingArgs + length ps > length (cParams c)
        existingArgs    = length $ M.toList (cEnv c)
        newParams       = M.fromList $ zip (drop existingArgs $ cParams c) ps
        newArgs         = map snd $ M.toList newParams
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
  = Except.throwE $ InternalTypeMismatch
              "Can't run primary function application on non-primary" [x]

evalDirective :: Scope -> BonlangDirectiveType -> IOThrowsException BonlangValue
evalDirective s (ModuleDef _ _ is _)
  = do sequence_ (fmap (\(_, f) -> defineReference s f) is')
       evalClosure s (aliasExpression . fromJust $ M.lookup "main" is) []
    where
        is' = filter (\(f,_) -> f /= "main") (M.toList is)
evalDirective _ (ModuleImport _) = error "TODO: Module imports"
evalDirective _ _
  = Except.throwE
      $ InternalTypeMismatch "Attempting eval directive for undefined" []

liftThrows :: ThrowsError a -> IOThrowsException a
liftThrows (Left err) = Except.throwE err
liftThrows (Right x)  = return x
