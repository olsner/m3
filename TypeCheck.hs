{-# LANGUAGE RankNTypes,FlexibleContexts #-}

module TypeCheck where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Functor.Fix

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

import AST

data Binding = Var { varType :: Type } | Alias { varAliasName :: Name }
type TC a = forall m . Monad m => StateT (Map Name Binding) m a

std_io_printf = QualifiedName ["std","io","printf"]
-- HACK to work around missing module import system...
-- HACK also to work around missing name resolution that can add qualifications
preregistered = [(std_io_printf, Var (TFunction TVoid [FormalParam (TPtr (TConst TChar)) Nothing, VarargParam])), (QualifiedName["printf"], Alias std_io_printf)]
preregisteredDecls = concatMap (\(name,bind) -> Decl name <$> mkDef bind) preregistered
  where
    mkDef (Var (TFunction ret args)) = [ExternalFunction Nothing ret args]
    mkDef (Alias _) = []

runTC = flip runStateT (M.fromList preregistered)

addBinding name typ = modify (M.insert name (Var typ))
getBindingType :: Name -> TC Type
getBindingType name = do
  bind <- getBinding name
  case bind of
    (Var typ) -> return typ
    (Alias name) -> getBindingType name
getBinding :: Name -> TC Binding
getBinding name = fromJust . M.lookup name <$> get

inScope :: Name -> Type -> TC a -> TC a
inScope name typ m = do
  s <- get
  put (M.insert name (Var typ) s)
  r <- m
  put s
  return r

type ModMap = Map Name (Unit ExprF)

typecheck :: (Functor m, Monad m, MonadReader ModMap m) => Name -> Unit ExprF -> m (Unit TypedE)
typecheck name (Unit imports decl) = Unit imports . fst <$> runTC (tcDecl name decl)

tcDecl :: Name -> Decl ExprF -> TC (Decl TypedE)
tcDecl name (Decl local def) =
  Decl local <$> tcDef (qualifyName name local) def

tcDef :: Name -> Def ExprF -> TC (Def TypedE)
tcDef name def = case def of
  (ModuleDef decls) -> ModuleDef <$> mapM (tcDecl name) (preregisteredDecls++decls)
  (FunctionDef retT args code) -> do
    addBinding name (TFunction retT args)
    FunctionDef retT args <$> mapM (tcStmt retT args) code
  (ExternalFunction linkage ret args) -> do
    addBinding name (TFunction ret args)
    return (ExternalFunction linkage ret args) -- TODO Extend name-to-type map with type information

tcStmt :: Type -> [FormalParam] -> Statement ExprF -> TC (Statement TypedE)
tcStmt ret args stmt = case stmt of
  (ReturnStmt e) -> ReturnStmt <$> tcExprAsType ret e
  (ExprStmt e) -> ExprStmt <$> tcExpr e
  ReturnStmtVoid -> return ReturnStmtVoid
  EmptyStmt -> return EmptyStmt
  CompoundStmt [stmt] -> tcStmt ret args stmt
  CompoundStmt xs -> CompoundStmt <$> mapM (tcStmt ret args) xs
  VarDecl name typ stmt -> VarDecl name typ <$> inScope name typ (tcStmt ret args stmt)

tcExprAsType :: Type -> ExprF -> TC TypedE
tcExprAsType expT e = do
  e@(TypedE t _) <- tcExpr e
  if not (t == expT) then error ("Expression "++show e++" not of expected type "++show expT++" but "++show t) else return e

tcParams :: [FormalParam] -> [ExprF] -> TC [TypedE]
tcParams (FormalParam typ _:ps) (x:xs) = liftM2 (:) (tcExprAsType typ x) (tcParams ps xs)
tcParams [VarargParam] xs = mapM tcExpr xs
tcParams [] [] = return []
-- missing cases represent various errors...

tcExpr :: ExprF -> TC TypedE
tcExpr e = case outF e of
  (EInt i) -> return (TypedE TInt (EInt i))
  (EString str) -> return (TypedE (TPtr (TConst TChar)) (EString str))
  (EVarRef name) -> do
    typ <- getBindingType name
    bind <- getBinding name
    return $ TypedE typ $ EDeref $ TypedE (TPtr typ) $ EVarRef $
      case bind of
        (Var typ) -> name
        (Alias name) -> name
  (EFunCall fun args) -> do
    (TypedE typ fune) <- tcExpr fun
    let fun_ = TypedE (TPtr typ) $ case fune of (EDeref (TypedE _ e)) -> e
    case typ of
      (TFunction retT params) -> do
        args_ <- tcParams params args
        return (TypedE retT (EFunCall fun_ args_))
      other -> error ("Function call on "++show other++" of non-function type "++show typ)
  (EAssignment op lval rval) -> do
    lv@(TypedE lvT (EDeref _)) <- tcExpr lval
    rv <- tcExprAsType lvT rval
    return (TypedE lvT (EAssignment op lv rv))
  other -> error ("tcExpr: Unknown expression "++show other)
