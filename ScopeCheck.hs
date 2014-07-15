{-# LANGUAGE RankNTypes,FlexibleContexts,NoMonomorphismRestriction,TypeSynonymInstances,PatternGuards,RelaxedPolyRec #-}

module ScopeCheck (scopecheck) where

import Control.Applicative
import Control.Monad.Identity
-- "local" is used as a variable name a lot in this file, let's not get it confused with the Reader definition
import Control.Monad.Reader hiding (local)
import Control.Monad.State

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Data.Generics

import System.IO

import AST

import Counter -- o.o Also implements Applicative for StateT

data SCState = SCState { depth :: Int, used :: Set Name, nameMap :: Map Name (Int,Name) }
type SC m = StateT SCState (CounterT Int m)

scopecheck :: Functor m => MonadIO m => Unit LocE -> m (Unit LocE)
scopecheck = --traceFunM "scopecheck" $
  everywhereM (mkM applySC)

applySC :: Functor m => MonadIO m => Def LocE -> m (Def LocE)
applySC (FunctionDef ret args code) = FunctionDef ret args <$> runSC args (sc code)
applySC x = return x
runSC args = runCounterT 0 . flip evalStateT (SCState 0 (S.fromList argNames) (M.fromList argsInit))
  where
    argNames = mapMaybe f args
    f (FormalParam _ name) = name
    f _ = Nothing
    argsInit = zip argNames (zip (repeat 0) argNames)
scError loc msg = error (show loc++": "++msg)
scWarn loc msg = traceIO (show loc++": "++msg)

--traceFunM str m x = liftIO (putStrLn (str++": "++show x)) >> m x >>= \x -> liftIO (putStrLn (str++" DONE: "++show x)) >> return x
--traceM str m = liftIO (putStrLn str) >> m >>= \x -> liftIO (putStr (str++": done\n")) >> return x
traceIO str = liftIO (hPutStrLn stderr str)

inNewScope :: (MonadIO m, Typeable a, Data a) => SC m a -> SC m a
inNewScope m = do
  d <- gets depth
  oldNameMap <- gets nameMap
  --traceIO (show d)
  modify (\s -> s { depth = succ d })
  x <- m
  d' <- gets depth
  --traceIO ("Returned to depth "++show d'++" (should be "++show (succ d)++")")
  --newNameMap <- gets nameMap
  --traceIO (show newNameMap)
  if d' /= d + 1 then error "scope depth error... :(" else return ()
  modify (\s -> s { depth = d, nameMap = oldNameMap })
  return x

mapName name newName = modify (\s -> s { used = S.insert name (used s), nameMap = M.insert name (depth s, newName) (nameMap s) })
addName name = modify (\s -> s { nameMap = M.insert name (depth s, name) (nameMap s) })

-- The scope checking pass is divided into three steps:
-- (1) Check that no CompoundStmt defines the same variable twice, respecting shadowing. (TODO: Detect shadowing so that we can optionally warn about it)
-- (2) Convert all VarDecl's to nested CompoundStmt's
-- (2.1) Optimize runs of VarDecl's into a single CompoundStmt
-- (3) Apply renaming of shadowed variables & variables reused in unrelated scopes within a function
-- (4?) Correctness check: no VarDecl nodes remain, no CompoundStmt has duplicate names

-- define our own top-down traversal...
sc :: Functor m => MonadIO m => LocStatement LocE -> SC m (LocStatement LocE)
sc =
  preCheck >=>
  {-traceFunM "convertVarDecl1"-} convertVarDecl1 >=>
  {-traceFunM "renameShadowed"-} renameShadowed -- >=> postCheck

-- FIXME This should run in its own State (Int,Map Name Int), the state produced
-- is not interesting for the outer checking step
--preCheck :: (MonadIO m, Typeable a, Data a, Show a) => a -> SC m a
preCheck :: Functor m => MonadIO m => LocStatement LocE -> SC m (LocStatement LocE)
preCheck (Loc loc stmt) = Loc loc <$> f stmt -- traceFunM "preCheck" f
  where
    f :: Functor m => MonadIO m => Statement LocE -> SC m (Statement LocE)
    f (CompoundStmt [] stmts) = CompoundStmt [] <$> mapM preCheck stmts
    f (IfStmt cond t f) = IfStmt cond <$> inNewScope (preCheck t) <*> inNewScope (preCheck f)
    f (WhileStmt cond body) = WhileStmt cond <$> inNewScope (preCheck body)
    f stmt@(VarDecl vars) = do
      --traceIO ("VarDecl: "++show stmt)
      forM_ vars $ \(Loc loc (Decl name _)) -> checkName loc name
      return stmt
    f x@(TypDecl name _) = checkName loc name >> return x
    f x@(ExprStmt _) = return x
    f x@(ReturnStmt _) = return x
    f x@EmptyStmt = return x
    f x@BreakStmt = return x
    f x@ContinueStmt = return x
    f x = scError loc ("Unknown statement in scope check: "++show x)

    checkName loc name = do
      existingVar <- gets (M.lookup name . nameMap)
      addName name
      --traceIO ("existingVar "++show name++": "++show existingVar)
      --newVar <- gets (M.lookup name . nameMap)
      --traceIO ("newVar "++show name++": "++show newVar)
      case existingVar of
        Just (d',_) -> do
          d <- gets depth
          if d == d' then scError loc "Redeclaration at same depth." else scWarn loc "WARNING: Declared variable shadows outer variable" >> return ()
        Nothing -> return ()

getVars :: Show e => [LocStatement e] -> ([LocDecl e], [LocStatement e])
getVars (Loc _ (VarDecl vs):xs) = (vs++ws, ys) where (ws,ys) = getVars xs
getVars (Loc loc (TypDecl n typ):xs) = (Loc loc (Decl n (TypeDef typ)):ws, ys) where (ws,ys) = getVars xs
getVars xs = ([],xs)

mapCont :: Monad m => (a -> ([a] -> m [a])) -> [a] -> m [a]
mapCont _ [] = return []
mapCont k (x:xs) = k x =<< mapCont k xs

convertVarDecl1 :: Functor m => MonadIO m => LocStatement LocE -> SC m (LocStatement LocE)
convertVarDecl1 stmt@(Loc loc _) = do
  xs <- convertVarDecls stmt []
  case xs of
    [x] -> return x
    _   -> scError loc "convertVarDecl1: One statement became several :("

-- TODO SYB:ify this - for all "other" statements: keep structure of everything that isn't a Statement, apply convertVarDecl1 to every statement
convertVarDecls :: Functor m => MonadIO m => LocStatement LocE -> ([LocStatement LocE] -> SC m [LocStatement LocE])
convertVarDecls (Loc loc stmt) k = case stmt of
  (CompoundStmt [] stmts) -> do
    let (vars,tail) = getVars stmts
    (:k) . Loc loc . CompoundStmt vars <$> mapCont convertVarDecls tail
  (VarDecl vars) -> return [Loc loc (CompoundStmt vars k)]
  (TypDecl name typ) -> return [Loc loc (CompoundStmt [Loc loc (Decl name (TypeDef typ))] k)]
  (IfStmt c t f) -> (:k) . Loc loc <$> (IfStmt c <$> convertVarDecl1 t <*> convertVarDecl1 f)
  (WhileStmt c body) -> (:k) . Loc loc <$> (WhileStmt c <$> convertVarDecl1 body)
  x -> return (Loc loc x:k)


renameShadowed :: Functor m => MonadIO m => LocStatement LocE -> SC m (LocStatement LocE)
renameShadowed (Loc loc stmt) = Loc loc <$>
  {- traceFunM "renameShadowed" -} case stmt of
    (CompoundStmt vars stmts) -> inNewScope $ do
      vars' <- forM vars $ \(Loc loc (Decl name def)) -> do
        --traceIO =<< gets (show . used)
        exists <- gets (S.member name . used)
        --traceIO (show name++": "++show exists)
        newName <- if exists then qualifyName1 name . show <$> lift getAndInc else return name
        mapName name newName
        newDef <- fDef loc def
        -- TODO Make a testcase for shadowed-renaming in initializers
        return (Loc loc (Decl newName newDef))
      CompoundStmt vars' <$> mapM renameShadowed stmts
    (IfStmt cond t f) -> IfStmt <$> g cond <*> inNewScope (renameShadowed t) <*> inNewScope (renameShadowed f)
    (WhileStmt cond body) -> WhileStmt <$> g cond <*> inNewScope (renameShadowed body)
    (VarDecl _) -> scError loc "VarDecl left over from rewrite step!"
    (TypDecl _ _) -> scError loc "TypDecl left over from rewrite step!"
    (ExprStmt e) -> ExprStmt <$> g e
    (ReturnStmt e) -> ReturnStmt <$> g e
    EmptyStmt -> return stmt
    ReturnStmtVoid -> return stmt
    BreakStmt -> return stmt
    ContinueStmt -> return stmt
    --f x = scError ("Unhandled statement: "++show x)
  where
    -- Should only need to handle definitions that are allowed in local scope.
    fDef loc (VarDef typ init) = do
        typ <- fType loc typ
        newInit <- case init of
          Just x -> Just <$> g x
          Nothing -> return Nothing
        return (VarDef typ newInit)
    fDef loc (TypeDef typ) = TypeDef <$> fType loc typ
    fDef loc def = scError loc ("Unexpected local def "++ show def)

    fType = foldFTypeM f
      where
        f _ (TNamedType name) = TNamedType <$> getName name
        f _ t = return t

    getName name = do
      existingVar <- gets (M.lookup name . nameMap)
      case existingVar of
        Just (_,name) -> return name
        Nothing -> do
          -- scWarn loc "WARNING: Use of undeclared variable"
          return name

    g = everywhereM (mkM g_)
    g_ :: Functor m => MonadIO m => LocE -> SC m LocE
    g_ (LocE loc expr) = LocE loc <$> case expr of
      (EVarRef name) -> EVarRef <$> getName name
      _ -> g expr
