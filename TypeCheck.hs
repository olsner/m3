module TypeCheck where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Functor.Fix

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

import AST

type TC a = State (Map Name Type) a

-- HACK to work around missing module import system...
-- HACK also to work around missing name resolution that can add qualifications
preregistered = [(QualifiedName [{-"std","io",-}"printf"], TFunction TVoid [FormalParam (TPtr (TConst TChar)) Nothing, VarargParam])]
preregisteredDecls = map (\(name,typ) -> Decl name (mkDef typ)) preregistered
  where
    mkDef (TFunction ret args) = ExternalFunction Nothing ret args

runTC = flip runState (M.fromList preregistered)

addBinding name typ = modify (M.insert name typ)
getBinding name = fromJust . M.lookup name <$> get

typecheck :: Name -> Unit ExprF -> Unit TypedE
typecheck name (Unit imports decl) = Unit imports $ fst $ runTC $ tcDecl name decl

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
  -- TODO 

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
  (EString str) -> return (TypedE (TPtr (TConst TChar)) (EString str)) -- TODO Array-to-pointer conversion
  (EVarRef name) -> do
    typ <- getBinding name
    return (TypedE typ (EVarRef name)) -- TODO Lookup name and give that type!
  (EFunCall fun args) -> do
    fun_@(TypedE typ _) <- tcExpr fun
    case typ of
      (TFunction retT params) -> do
        args_ <- tcParams params args
        return (TypedE retT (EFunCall fun_ args_))
      other -> error ("Function call on "++show other++" of non-function type "++show typ)
