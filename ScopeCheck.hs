{-# LANGUAGE RankNTypes,FlexibleContexts,NoMonomorphismRestriction,TypeSynonymInstances,PatternGuards,RelaxedPolyRec #-}

module ScopeCheck (scopecheck) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Identity
-- "local" is used as a variable name a lot in this file, let's not get it confused with the Reader definition
import Control.Monad.Reader hiding (local)
import Control.Monad.State
import Control.Functor.Fix

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

import Data.Generics hiding (Unit)

import Debug.Trace

import Text.Printf
import Text.ParserCombinators.Parsec.Pos

import AST
import CppToken
import Types.Conv

import Counter -- o.o Also implements Applicative for StateT

type SC m = StateT (Int,Map Name (Int,Name)) (CounterT Int m)

scopecheck :: MonadIO m => Unit ExprF -> m (Unit ExprF)
scopecheck = runSC . traceFunM "sc" sc

runSC = runCounterT 0 . flip evalStateT (0,M.empty)
scError = error

traceFunM str m x = liftIO (putStrLn (str++": "++show x)) >> m x >>= \x -> liftIO (putStrLn (str++" DONE: "++show x)) >> return x
traceM str m = liftIO (putStrLn str) >> m >>= \x -> liftIO (putStr (str++": done\n")) >> return x
traceIO str = liftIO (putStrLn str)

newScope :: (MonadIO m, Typeable a, Data a) => SC m a -> SC m a
newScope m = do
  d <- gets fst
  traceIO (show d)
  modify (first succ)
  x <- m
  d' <- gets fst
  traceIO (show d)
  if d' /= d + 1 then error "scope depth error... :(" else return ()
  modify (first (const d))
  return x

-- define our own top-down traversal...
sc :: (MonadIO m, Typeable a, Data a, Show a) => a -> SC m a
sc = gmapM (mkM f)

f :: MonadIO m => Statement ExprF -> SC m (Statement ExprF)
f (CompoundStmt stmts) = CompoundStmt <$> newScope (mapM sc stmts)
f (VarDecl name typ init stmt) = do
  d <- gets fst
  existingVar <- gets (M.lookup name . snd)
  traceIO ("existingVar: "++show existingVar)
  newName <- return $ case existingVar of
    Just (d',name') -> if d == d' then scError "Redeclaration at same depth." else qualifyName1 name (show d)
    Nothing -> name
  VarDecl newName typ <$> sc init <*> sc stmt
f x = sc x

g (EVarRef name) = EVarRef <$> gets (fromMaybe name . fmap snd . M.lookup name . snd)
g x = sc x
