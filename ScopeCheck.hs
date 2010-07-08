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

import Data.Generics hiding (Unit)

import AST

import Counter -- o.o Also implements Applicative for StateT

data SCState = SCState { depth :: Int, used :: Set Name, nameMap :: Map Name (Int,Name) }
type SC m = StateT SCState (CounterT Int m)

scopecheck :: Functor m => MonadIO m => Unit ExprF -> m (Unit ExprF)
scopecheck = --traceFunM "scopecheck" $
  everywhereM (mkM applySC)

applySC :: Functor m => MonadIO m => Def ExprF -> m (Def ExprF)
applySC (FunctionDef ret args code) = FunctionDef ret args <$> runSC args (sc code)
applySC x = return x
runSC args = runCounterT 0 . flip evalStateT (SCState 0 (S.fromList argNames) (M.fromList argsInit))
  where
    argNames = mapMaybe f args
    f (FormalParam _ name) = name
    f _ = Nothing
    argsInit = zip argNames (zip (repeat 0) argNames)
scError = error
scWarn = traceIO

traceFunM str m x = liftIO (putStrLn (str++": "++show x)) >> m x >>= \x -> liftIO (putStrLn (str++" DONE: "++show x)) >> return x
traceM str m = liftIO (putStrLn str) >> m >>= \x -> liftIO (putStr (str++": done\n")) >> return x
traceIO str = liftIO (putStrLn str)

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
sc :: MonadIO m => Statement ExprF -> SC m (Statement ExprF)
sc =
  preCheck >=>
  {-traceFunM "convertVarDecl1"-} (return . convertVarDecl1) >=>
  {-traceFunM "renameShadowed"-} renameShadowed -- >=> postCheck

-- FIXME This should run in its own State (Int,Map Name Int), the state produced
-- is not interesting for the outer checking step
--preCheck :: (MonadIO m, Typeable a, Data a, Show a) => a -> SC m a
preCheck :: MonadIO m => Statement ExprF -> SC m (Statement ExprF)
preCheck = f -- traceFunM "preCheck" f
  where
    f :: MonadIO m => Statement ExprF -> SC m (Statement ExprF)
    f (CompoundStmt [] stmts) = CompoundStmt [] <$> mapM preCheck stmts
    f (IfStmt cond t f) = IfStmt cond <$> inNewScope (preCheck t) <*> inNewScope (preCheck f)
    f (WhileStmt cond body) = WhileStmt cond <$> inNewScope (preCheck body)
    f stmt@(VarDecl vars) = do
      d <- gets depth
      --traceIO ("VarDecl: "++show stmt)
      forM_ vars $ \(_typ,name,_init) -> do
        existingVar <- gets (M.lookup name . nameMap)
        addName name
        --traceIO ("existingVar "++show name++": "++show existingVar)
        --newVar <- gets (M.lookup name . nameMap)
        --traceIO ("newVar "++show name++": "++show newVar)
        case existingVar of
          Just (d',_) -> if d == d' then scError "Redeclaration at same depth." else scWarn "WARNING: Declared variable shadows outer variable" >> return ()
          Nothing -> return ()
      return stmt
    f x@(ExprStmt _) = return x
    f x@(ReturnStmt _) = return x
    f EmptyStmt = return EmptyStmt
    f x = scError ("Unknown statement in scope check: "++show x)

getVars :: Show e => [Statement e] -> ([(Type,Name,Maybe e)], [Statement e])
getVars (VarDecl vs:xs) = (vs++ws, ys) where (ws,ys) = getVars xs
getVars xs = ([],xs)

mapCont :: (a -> ([a] -> [a])) -> [a] -> [a]
mapCont _ [] = []
mapCont k (x:xs) = k x (mapCont k xs)

convertVarDecl1 :: Statement ExprF -> Statement ExprF
convertVarDecl1 stmt = case convertVarDecls stmt [] of [x] -> x; _ -> error "convertVarDecl1: One statement became several :("

-- TODO SYB:ify this - for all "other" statements: keep structure of everything that isn't a Statement, apply convertVarDecl1 to every statement
convertVarDecls :: Statement ExprF -> ([Statement ExprF] -> [Statement ExprF])
convertVarDecls stmt = case stmt of
  (CompoundStmt [] stmts) -> let (vars,tail) = getVars stmts in \k -> CompoundStmt vars (mapCont convertVarDecls tail) : k
  (VarDecl vars) -> (:[]) . CompoundStmt vars
  (IfStmt c t f) -> (IfStmt c (convertVarDecl1 t) (convertVarDecl1 f):)
  (WhileStmt c body) -> (WhileStmt c (convertVarDecl1 body):)
  x -> (:) x


renameShadowed :: MonadIO m => Statement ExprF -> SC m (Statement ExprF)
renameShadowed = f -- traceFunM "renameShadowed" f
  where
    f :: MonadIO m => Statement ExprF -> SC m (Statement ExprF)
    f (CompoundStmt vars stmts) = inNewScope $ do
      vars' <- forM vars $ \(typ,name,init) -> do
        traceIO =<< gets (show . used)
        exists <- gets (S.member name . used)
        traceIO (show name++": "++show exists)
        newName <- if exists then qualifyName1 name . show <$> lift getAndInc else return name
        mapName name newName
        newInit <- case init of
          Just x -> Just <$> g x
          Nothing -> return Nothing
        -- TODO Make a testcase for shadowed-renaming in initializers
        return (typ,newName,newInit)
      CompoundStmt vars' <$> mapM renameShadowed stmts
    f (IfStmt cond t f) = IfStmt <$> g cond <*> inNewScope (renameShadowed t) <*> inNewScope (renameShadowed f)
    f (WhileStmt cond body) = WhileStmt <$> g cond <*> inNewScope (renameShadowed body)
    f (VarDecl _) = scError "VarDecl left over from rewrite step!"
    f (ExprStmt e) = ExprStmt <$> g e
    f (ReturnStmt e) = ReturnStmt <$> g e
    f EmptyStmt = return EmptyStmt
    f ReturnStmtVoid = return ReturnStmtVoid
    --f x = scError ("Unhandled statement: "++show x)

    g = everywhereM (mkM g_)
    g_ :: MonadIO m => ExprF -> SC m ExprF
    g_ expr = case outF expr of
      (EVarRef name) -> do
        existingVar <- gets (M.lookup name . nameMap)
        newName <- case existingVar of
          Just (_,name) -> return name
          Nothing -> scWarn "WARNING: Use of undeclared variable" >> return name
        return (InF (EVarRef newName))
      x -> InF <$> g x
