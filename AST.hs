{-# LANGUAGE TypeSynonymInstances,DeriveDataTypeable,DeriveFunctor,TypeFamilies,StandaloneDeriving,FlexibleContexts,UndecidableInstances #-}

module AST where

import Control.Applicative
import Control.Monad.RWS

import Data.Data (Data,Typeable,Typeable1)
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

data Location = Location { locStart :: SourcePos, locEnd :: SourcePos } | CommandLine deriving (Eq,Ord,Data,Typeable)
dummyLocation = Location x x where x = initialPos "<unknown>"
instance Show Location where
  show CommandLine = "command line"
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
  Unit { unitImports :: [Loc Name], unitDecl :: LocDecl e }
  --deriving (Show,Eq,Data,Typeable)
deriving instance (Show e, Show (ET e)) => Show (Unit e)
deriving instance (Show e, Data e, Typeable e, Data (ET e)) => Data (Unit e)
deriving instance Typeable1 Unit
importedUnits :: Map Name (Unit e) -> Name -> [Name]
importedUnits units name = snd $ execRWS (go name) units S.empty
  where
    -- go :: Name -> RWS (Map Name (Unit e)) [Name] (Set Name) ()
    go name = gets (S.member name) >>= \member -> when (not member) (add name)
    add name = do
      modify (S.insert name)
      mapM_ go =<< asks (map locData . unitImports . fromJust . M.lookup name)
      tell [name]

-- unitExports :: Unit e -> LocDecl e

type VarDecl e = Loc (ET e,Name,Maybe e)
type TypDecl t = Loc (Name,t)

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
  | TypDecl Name (ET e)
  | CompoundStmt [LocDecl e] [LocStatement e]
  | IfStmt e (LocStatement e) (LocStatement e)
  | WhileStmt e (LocStatement e)
  | BreakStmt
  | ContinueStmt
  --deriving (Show,Eq,Data,Typeable)
deriving instance (Show e, Show (ET e)) => Show (Statement e)
deriving instance Typeable1 Statement
deriving instance (Data (ET e), Show e, Data e, Typeable e) => Data (Statement e)

data Decl e = Decl Name (Def e) -- deriving (Show,Eq,Data,Typeable)
deriving instance (Show e, Show (ET e)) => Show (Decl e)
deriving instance Typeable1 Decl
deriving instance (Show e, Data e, Typeable e, Data (ET e)) => Data (Decl e)
type LocDecl e = Loc (Decl e)

data FormalParam t =
    FormalParam t (Maybe Name)
  | VarargParam
  deriving (Show,Eq,Ord,Data,Typeable)
type FormalParams t = [FormalParam t]

data Def e =
    ModuleDef [LocDecl e]
  | FunctionDef { funReturnType :: ET e, funArgs :: FormalParams (ET e), funCode :: LocStatement e }
  | ExternalFunction { funLinkage :: Maybe String, funReturnType :: ET e, funArgs :: FormalParams (ET e), funExternalName :: Name }
  | VarDef (ET e) (Maybe e)
  | TypeDef (ET e)
  --deriving (Show,Eq,Data,Typeable)
deriving instance (Show e, Show (ET e)) => Show (Def e)
deriving instance Typeable1 Def
deriving instance (Show e, Data e, Typeable e, Data (ET e)) => Data (Def e)
type LocDef e = Loc (Def e)

-- Perhaps make this parameterized too, so we can easily make locationed types,
-- types with variables (pre-type-inference), etc.
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
  | TFunction Type (FormalParams Type)
  -- Maybe the struct type doesn't need to include names of fields?
  -- But struct field access currently requires that we know at the use site
  -- which fields are available. Maybe a desugaring for fields where
  -- x.y expands to *get_y(x) with get_y declared as "[Y] get_y(X);".
  -- The struct syntax would automatically declare functions for you, but they
  -- could also be implemented manually.
  -- (This means we need overloading to support multiple structs with the same
  -- field names.)
  | TStruct [Loc (Name,Type)]
  -- TODO Use UType, UTypeVar instead
  | TNamedType Name
  deriving (Show,Eq,Ord,Data,Typeable)

-- "Full" type - everything has an exact type. The result of type inference.
data FType t = FType t
  deriving (Show,Eq,Ord,Data,Typeable)

-- Uninferred, unresolved type
data UType =
  -- Expands to a freshly generated type variable name when inferred
    UFreshVar
  -- Wildcard type unifies with anything, can be used to span the shape of
  -- something without specifying the whole type.
  -- Not sure what the syntax for this will be, probably ?
  | UWildcard
  -- May refer to user-defined types or type-function arguments or fresh type
  -- vars, but will start out referring only to user-defined types.
  | UTypeVar Name
  | UType (FType UType)
  deriving (Show,Eq,Ord,Data,Typeable)

mapFormalParamTypes :: Monad m => (t -> m t) -> FormalParams t -> m (FormalParams t)
mapFormalParamTypes f = mapM f'
  where
    f' VarargParam = return VarargParam
    f' (FormalParam typ name) = f typ >>= \typ -> return (FormalParam typ name)

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

data Expr t e =
    EFunCall e [e]
  -- TODO Use Tok instead of Token - expressions have associated location info anyway.
  | EBinary Token e e
  | EUnary Token e
  | EPostfix Token e -- ^ postfix ++ and --
  | EPrefix Token e -- ^ prefix ++ and --
  | EConditional e e e -- ^ ?: expressions: condition, true-value, false-value
  | EAssignment Token e e -- FIXME EBinary with assignment operator...
  | EDeref e -- ^ Dereferencing of a pointer. Doubles as lvalue-to-rvalue conversion, inserted by type checking. All variables are pointers, surprisingly.
  | EArrayIndex e e -- ^ An array indexing expression. The first argument is the index, the second is the array/pointer. EArrayIndex a b == b[a]
  | EFieldAccess Name e -- ^ A field access (foo.bar where foo is of a struct type having 'bar' as a member)
  | EArrToPtr e
  | ESeq e e -- ^ Sequencing expression, ESeq a b = (a,b)
  | ECast t e -- ^ Cast of an expression to a given type. ECast typ e == cast<[typ]>(e)
  -- 
  | EVarRef Name
  -- Literals
  | EString String
  | EInt Integer
  | EBool Bool
  | EChar Char
  | ENullPtr
  deriving (Show,Eq,Data,Typeable,Functor)

-- TODO Use this instead of the explicit 't' parameter to Statement etc
type family ET e :: *
type instance ET (Expr t e) = t
type instance ET LocE = Type
type instance ET TypedE = Type
type instance ET UTypedE = UType

data UTypedE = UTypedE Location UType (Expr UType UTypedE)
  deriving (Show,Eq,Data,Typeable)
data TypedE = TypedE Location Type (Expr Type TypedE)
  deriving (Show,Eq,Data,Typeable)
data LocE = LocE Location (Expr Type LocE) deriving (Show,Eq,Data,Typeable)

