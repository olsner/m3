{-# LANGUAGE TypeSynonymInstances #-}

module AST where

import Data.List (intercalate)
import CppToken (Tok)
import Control.Functor.Fix

newtype Name = QualifiedName [String] deriving (Show,Eq,Ord)
qualifyName (QualifiedName xs) (QualifiedName ys) = QualifiedName (xs++ys)
encodeName (QualifiedName xs) = intercalate "__" xs

-- A unit is a set of imports and *one* declaration of the toplevel entity.
data Unit e = Unit { unitImports :: [Name], unitDecl :: Decl e } deriving (Show,Eq)

data Show e => Statement e =
    EmptyStmt
  | ReturnStmt e
  | ReturnStmtVoid
  | ExprStmt e
  | VarDecl Name Type (Statement e)
  | CompoundStmt [Statement e]
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
  | TSizedInt Int -- i8 => TSizedInt 8, etc
  | TChar
  | TConst Type
  | TPtr Type
  | TArray Int Type
  | TFunction Type [FormalParam]
  | TUnknown String -- delayed type error, or unchecked expression
  deriving (Show,Eq,Ord)

data TypedE = TypedE Type (Expr TypedE) deriving (Show,Eq)

data Expr e =
    EVarRef Name
  | EFunCall e [e]
  | EBinary Tok e e
  | EUnary Tok e
  | EConditional e e e -- ^ ?: expressions: condition, true-value, false-value
  | EAssignment Tok e e
  | EString String
  | EInt Integer
  | EDeref e -- ^ Doubles as lvalue-to-rvalue conversion, inserted by type checking. All variables are pointers, surprisingly
  | EArrToPtr e
  deriving (Show,Eq)

type ExprF = FixF Expr
instance Show ExprF where
  show (InF e) = show e

