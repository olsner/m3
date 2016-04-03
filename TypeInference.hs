{-# LANGUAGE RankNTypes,FlexibleContexts,NoMonomorphismRestriction,TypeSynonymInstances,PatternGuards,CPP #-}

module TypeInference (inferTypes) where

import Control.Applicative
import Control.Monad.Identity
-- "local" is used as a variable name a lot in this file, let's not get it confused with the Reader definition
import Control.Monad.Reader hiding (local)
import Control.Monad.State

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

import Debug.Trace

import Text.Printf

import AST
import CType
import CppToken
import Types.Conv

import Counter() -- Also implements Applicative for StateT o.o

-- based on http://www21.in.tum.de/~nipkow/pubs/aplas11.pdf

-- inference:
-- 1. constraint generation
--    term t, context Gamma -> type Tau, constraint set S
-- 2. weak unification (termination check)
--    S weakly unifiable?
-- 3. constraint simplification (simp)
--    S, 0 ==>simp S', Th'simp
-- 4. build constraint graph (cyc)
--    G(S'), Th'simp ==>cyc (G, Th'cyc)
-- 5. resolve constraints (sol)
--    G, Th'cyc ==>sol (G', Th'sol)
-- 6. unification (unif) => theta
--    G'[TV(G')], Th'sol ==>unif ((0,0), Th)
-- 7. coercion generation and insertion
--    term, context and type from 1, with final substitutions applied
--    Th * Gamma |- Th * t ~> u : Th * Tau


-- Could perhaps simplify this since we have already checked for circular
-- dependencies before trying to infer types.
inferTypes :: (Functor m, MonadIO m) =>
    UnitMap CTypedE -> Loc Name -> m (UnitMap FTypedE)
inferTypes mods (Loc loc name) = do
  let untyped = M.map Untyped mods
  newMods <- modules . snd <$> runTI (tiUnitByName loc name) untyped
  return (typedBindings newMods)

data ModBinding = CTyped (Unit CTypedE) | FTyped (Unit FTypedE) | Blackhole

typedBindings :: Map Name ModBinding -> Map Name (Unit UTypedE)
typedBindings = M.mapMaybe f
  where
    f (Typed x) = Just x
    f _ = Nothing
