{-# LANGUAGE TypeSynonymInstances,DeriveDataTypeable #-}

module AST where

import Control.Monad.RWS

import Data.Data (Data,Typeable)
import Data.List (intercalate)
import Data.Maybe (fromJust)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import CppToken (Token)

newtype Name = QualifiedName [String] deriving (Show,Eq,Ord,Data,Typeable)
qualifyName (QualifiedName xs) (QualifiedName ys) = QualifiedName (xs++ys)
-- Used for e.g. renaming passes or anything that wants to add suffixes to
-- names
qualifyName1 (QualifiedName xs) x = QualifiedName (xs++[x])
encodeName (QualifiedName xs) = intercalate "__" xs

-- A unit is a set of imports and *one* declaration of the toplevel entity.
data Unit e =
  Unit { unitImports :: [Name], unitDecl :: Decl e }
  deriving (Show,Eq,Data,Typeable)
importedUnits :: Map Name (Unit e) -> Name -> [Name]
importedUnits units name = snd $ execRWS (go name) units S.empty
  where
    go :: Name -> RWS (Map Name (Unit e)) [Name] (Set Name) ()
    go name = gets (S.member name) >>= \member -> when (not member) (add name)
    add name = do
      modify (S.insert name)
      mapM_ go =<< asks (unitImports . fromJust . M.lookup name)
      tell [name]

data Show e => Statement e =
    EmptyStmt
  | ReturnStmt e
  | ReturnStmtVoid
  | ExprStmt e
  -- Special entry for *parsing* variable declarations - should be transformed
  -- into nested CompoundStmt's after checking for conflicting variables as part of the
  -- scopechecking.
  | VarDecl [(Type,Name,Maybe e)]
  | TypDecl Name Type
  | CompoundStmt [(Type,Name,Maybe e)] [Statement e]
  | IfStmt e (Statement e) (Statement e)
  | WhileStmt e (Statement e)
  deriving (Show,Eq,Data,Typeable)

data Decl e = Decl Name (Def e) deriving (Show,Eq,Data,Typeable)

data FormalParam =
    FormalParam Type (Maybe Name)
  | VarargParam
  deriving (Show,Eq,Ord,Data,Typeable)
type FormalParams = [FormalParam]

data Def e =
    ModuleDef [Decl e]
  | FunctionDef { funReturnType :: Type, funArgs :: FormalParams, funCode :: Statement e }
  | ExternalFunction { funLinkage :: Maybe String, funReturnType :: Type, funArgs :: FormalParams }
  | VarDef Type (Maybe e)
  | TypeDef Type

  deriving (Show,Eq,Data,Typeable)

data Type =
    TVoid
  | TInt
-- Should probably be revived and used to replace TInt, TChar, TBool by the same type. Or just deleted.
--  | TSizedInt Int -- i8 => TSizedInt 8, etc
  | TChar
  | TBool
  | TConst Type
  | TPtr Type
  | TNullPtr -- pointer of any type...
  | TArray Int Type
  | TFunction Type [FormalParam]
  deriving (Show,Eq,Ord,Data,Typeable)

data TypedE = TypedE Type (Expr TypedE) deriving (Show,Eq,Data,Typeable)

data Expr e =
    EFunCall e [e]
  | EBinary Token e e
  | EUnary Token e
  | EPostfix Token e
  | EConditional e e e -- ^ ?: expressions: condition, true-value, false-value
  | EAssignment Token e e -- FIXME EBinary with assignment operator...
  | EDeref e -- ^ Dereferencing of a pointer. Doubles as lvalue-to-rvalue conversion, inserted by type checking. All variables are pointers, surprisingly.
  | EArrayIndex e e -- ^ An array indexing expression. The first argument is the index, the second is the array/pointer. EArrayIndex a b == b[a]
  | EArrToPtr e
  | ESeq e e -- ^ Sequencing expression, ESeq a b = (a,b)
  | ECast Type e -- ^ Cast of an expression to a given type. ECast typ e == cast<[typ]>(e)
  -- 
  | EVarRef Name
  -- Literals
  | EString String
  | EInt Integer
  | EBool Bool
  | EChar Char
  | ENullPtr
  deriving (Show,Eq,Data,Typeable)

data ExprF = InF { outF :: Expr ExprF } deriving (Eq,Data,Typeable)

instance Show ExprF where
  showsPrec d (InF e) = showsPrec (d+1) e

