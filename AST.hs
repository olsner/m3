{-# LANGUAGE TypeSynonymInstances #-}

module AST where

import Control.Functor.Fix
import Control.Monad.RWS

import Data.List (intercalate)
import Data.Maybe (fromJust)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import CppToken (Token)

newtype Name = QualifiedName [String] deriving (Show,Eq,Ord)
qualifyName (QualifiedName xs) (QualifiedName ys) = QualifiedName (xs++ys)
encodeName (QualifiedName xs) = intercalate "__" xs

-- A unit is a set of imports and *one* declaration of the toplevel entity.
data Unit e = Unit { unitImports :: [Name], unitDecl :: Decl e } deriving (Show,Eq)
importedUnits :: Map Name (Unit e) -> Name -> [Name]
importedUnits units name = snd $ execRWS (go name) units S.empty
  where
    go :: Name -> RWS (Map Name (Unit e)) [Name] (Set Name) ()
    go name = gets (S.member name) >>= \member -> when (not member) (add name)
    add name = do
      modify (S.insert name)
      imps <- asks (unitImports . fromJust . M.lookup name)
      mapM go imps
      tell [name]

data Show e => Statement e =
    EmptyStmt
  | ReturnStmt e
  | ReturnStmtVoid
  | ExprStmt e
  | VarDecl Name Type (Statement e)
  | CompoundStmt [Statement e]
  | IfStmt e (Statement e) (Statement e)
  deriving (Show,Eq)

type CompoundStatement e = [Statement e]

data Decl e = Decl Name (Def e) deriving (Show,Eq)

data FormalParam =
    FormalParam Type (Maybe Name)
  | VarargParam
  deriving (Show,Eq,Ord)
type FormalParams = [FormalParam]

data Def e =
    ModuleDef [Decl e]
  | FunctionDef { funReturnType :: Type, funArgs :: FormalParams, funCode :: CompoundStatement e }
  | ExternalFunction { funLinkage :: Maybe String, funReturnType :: Type, funArgs :: FormalParams }

  deriving (Show,Eq)

data Type =
    TVoid
  | TInt
-- Should probably be revived and used to replace TInt, TChar, TBool by the same type. Or just deleted.
--  | TSizedInt Int -- i8 => TSizedInt 8, etc
  | TChar
  | TConst Type
  | TPtr Type
  | TArray Int Type
  | TFunction Type [FormalParam]
  deriving (Show,Eq,Ord)

data TypedE = TypedE Type (Expr TypedE) deriving (Show,Eq)

data Expr e =
    EVarRef Name
  | EFunCall e [e]
  | EBinary Token e e
  | EUnary Token e
  | EConditional e e e -- ^ ?: expressions: condition, true-value, false-value
  | EAssignment Token e e
  | EString String
  | EInt Integer
  | EDeref e -- ^ Doubles as lvalue-to-rvalue conversion, inserted by type checking. All variables are pointers, surprisingly
  | EArrToPtr e
  deriving (Show,Eq)

type ExprF = FixF Expr
instance Show ExprF where
  show (InF e) = show e

