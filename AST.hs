module AST where

data Name = StringName String | QualifiedName [String] deriving (Show,Eq,Ord)

-- A unit is a set of imports and *one* declaration of the toplevel entity.
data Unit = Unit { unitImports :: [Name], unitDecl :: Decl } deriving (Show,Eq)

data Statement =
  EmptyStatement
  
  deriving (Show,Eq)

type CompoundStatement = [Statement]

data Decl = Decl Name Def deriving (Show,Eq)

data VariableDecl = VarDecl Name Type deriving (Show,Eq)
data FormalParam = FormalParam Type (Maybe Name) deriving (Show,Eq)
type FormalParams = [FormalParam]

data Def =
  ModuleDef [Decl]
  | FunctionDef { funReturnType :: Type, funArgs :: FormalParams, funCode :: CompoundStatement }

  deriving (Show,Eq)

data Type =
    TVoid
  | TInt
  | TSizedInt Int -- i8 => TSizedInt 8, etc
  | TChar
  | TConst Type
  | TPtr Type
  | TArray Integer Type

  deriving (Show,Eq)
