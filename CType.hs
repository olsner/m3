{-# LANGUAGE TypeSynonymInstances,DeriveDataTypeable,DeriveFunctor,TypeFamilies,StandaloneDeriving,FlexibleContexts,UndecidableInstances,NoMonomorphismRestriction,MultiParamTypeClasses,FunctionalDependencies #-}

module CType where

import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import AST

baseT (TStruct _) = True
baseT t = elem t basetypes
  where basetypes = [TVoid, TInt, TChar, TBool, TNullPtr, TDummyBase]
dummyBaseT = TDummyBase

subBaseT TVoid _ = False
-- Widening conversions
subBaseT TChar TInt = True
subBaseT TBool TInt = True
-- dubious, narrowing cast from int to char
subBaseT TInt TChar = True
-- Implicit comparison to zero/null
subBaseT TInt TBool = True
subBaseT TChar TBool = True
subBaseT TNullPtr TBool = True
-- There are other subtype relations too, but not between base types
subBaseT _ _ = False

-- Inferred constraint type. No "fresh variable placeholders" remain, they have
-- been given variables now.
data CType =
    CTypeVar Name
  | CType (Type CType)
  deriving (Eq,Ord)

instance Show CType where
  showsPrec p (CTypeVar n) = showsPrec p n
  showsPrec p (CType t) = showsPrec p t

base (CTypeVar _) = False
base (CType t) = baseT t

-- | First argument is a subtype of the second argument, both are base types.
subBase (CType t) (CType u) = subBaseT t u

instance TypeF CType where
  wrapT = CType
  foldTM f (CType t) = f =<< foldTypeM (foldTM f) t
  foldTM f t = pure t

data CTypedE = CTypedE Location CType (Expr CType CTypedE)
  deriving (Show,Eq)

-- foldExprM :: (Applicative f) => (e -> f e') -> (t -> f t') -> Expr t e -> f (Expr t' e')

data Constraint t = Eq t t | Sub t t
  deriving (Show,Eq,Ord,Functor)
type FConstr = Constraint UType
type CSet t = Set (Constraint t)
type CCSet = CSet CType

