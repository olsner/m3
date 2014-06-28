{-# LANGUAGE TypeSynonymInstances,DeriveDataTypeable,DeriveFunctor #-}

module AST where

import Control.Applicative
import Control.Monad.RWS

import Data.Data (Data,Typeable)
import Data.List (intercalate)
import Data.Maybe (fromJust)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import CppToken (Token)
import SourcePos

newtype Name = QualifiedName [String] deriving (Show,Eq,Ord,Data,Typeable)
qualifyName (QualifiedName xs) (QualifiedName ys) = QualifiedName (xs++ys)
-- Used for e.g. renaming passes or anything that wants to add suffixes to
-- names
qualifyName1 (QualifiedName xs) x = QualifiedName (xs++[x])
encodeName (QualifiedName xs) = intercalate "__" xs

data Location = Location { locStart :: SourcePos, locEnd :: SourcePos } deriving (Eq,Ord,Data,Typeable)
dummyLocation = Location x x where x = initialPos "<unknown>"
instance Show Location where
  show (Location start end) = if sourceName start == sourceName end
    then sourceName start++" ("++showSameFile++")"
    else show start++" .. "++show end
    where
      showSameFile = if sourceLine start == sourceLine end
        then showLine start++", "++showSameLine
        else showLineCol start++" .. "++showLineCol end
      showSameLine = if sourceColumn start == sourceColumn end
        then showCol start
        else showCol start++".."++show (sourceColumn end)
      showLineCol p = showLine p++", "++showCol p
      showLine p = "line "++show (sourceLine p)
      showCol p = "column "++show (sourceColumn p)

data Loc a = Loc Location a deriving (Show,Eq,Ord,Data,Typeable,Functor)
locData (Loc _ x) = x

-- A unit is a set of imports and *one* declaration of the toplevel entity.
data Unit e =
  Unit { unitImports :: [Name], unitDecl :: LocDecl e }
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

-- unitExports :: Unit e -> LocDecl e

type VarDecl e = Loc (Type,Name,Maybe e)
type TypDecl = Loc (Name,Type)

type LocStatement e = Loc (Statement e)
data Show e => Statement e =
    EmptyStmt
  | ReturnStmt e
  | ReturnStmtVoid
  | ExprStmt e
  -- Special entry for *parsing* variable declarations - should be transformed
  -- into nested CompoundStmt's after checking for conflicting variables as
  -- part of the scopechecking.
  | VarDecl [LocDecl e]
  | TypDecl Name Type
  | CompoundStmt [LocDecl e] [LocStatement e]
  | IfStmt e (LocStatement e) (LocStatement e)
  | WhileStmt e (LocStatement e)
  | BreakStmt
  | ContinueStmt
  deriving (Show,Eq,Data,Typeable)

data Decl e = Decl Name (Def e) deriving (Show,Eq,Data,Typeable)
type LocDecl e = Loc (Decl e)

data FormalParam =
    FormalParam Type (Maybe Name)
  | VarargParam
  deriving (Show,Eq,Ord,Data,Typeable)
type FormalParams = [FormalParam]

data Def e =
    ModuleDef [LocDecl e]
  | FunctionDef { funReturnType :: Type, funArgs :: FormalParams, funCode :: LocStatement e }
  | ExternalFunction { funLinkage :: Maybe String, funReturnType :: Type, funArgs :: FormalParams, funExternalName :: Name }
  | VarDef Type (Maybe e)
  | TypeDef Type

  deriving (Show,Eq,Data,Typeable)
type LocDef e = Loc (Def e)

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
  | TFunction Type FormalParams
  | TStruct [Loc (Name,Type)]
  | TNamedType Name
  deriving (Show,Eq,Ord,Data,Typeable)

mapFormalParamTypes :: Monad m => (Type -> m Type) -> FormalParams -> m FormalParams
mapFormalParamTypes f = mapM f'
  where
    f' VarargParam = return VarargParam
    f' (FormalParam typ name) = f typ >>= \typ -> return (FormalParam typ name)

--type TypeFold m = Location -> Type -> m Type
--foldTypeM :: Functor m => Monad m => TypeFold m -> TypeFold m
foldTypeM f loc t = f loc =<< case t of
  (TConst typ) -> TConst <$> g typ
  (TPtr typ) -> TPtr <$> g typ
  (TArray n typ) -> TArray n <$> g typ
  (TFunction ret fps) -> TFunction <$> g ret <*> mapFormalParamTypes g fps
  (TStruct fields) -> TStruct <$> mapM namedG fields
  _ -> return t
  where
    g = g' loc
    g' = foldTypeM f
    namedG (Loc loc (name,typ)) = do
      typ <- g' loc typ
      return (Loc loc (name,typ))

data Expr e =
    EFunCall e [e]
  -- TODO Use Tok instead of Token - expressions have associated location info anyway.
  | EBinary Token e e
  | EUnary Token e
  | EPostfix Token e
  | EConditional e e e -- ^ ?: expressions: condition, true-value, false-value
  | EAssignment Token e e -- FIXME EBinary with assignment operator...
  | EDeref e -- ^ Dereferencing of a pointer. Doubles as lvalue-to-rvalue conversion, inserted by type checking. All variables are pointers, surprisingly.
  | EArrayIndex e e -- ^ An array indexing expression. The first argument is the index, the second is the array/pointer. EArrayIndex a b == b[a]
  | EFieldAccess Name e -- ^ A field access (foo.bar where foo is of a struct type having 'bar' as a member)
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

data TypedE = TypedE Location Type (Expr TypedE) deriving (Show,Eq,Data,Typeable)
data LocE = LocE Location (Expr LocE) deriving (Show,Eq,Data,Typeable)

