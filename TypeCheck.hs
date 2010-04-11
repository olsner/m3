{-# LANGUAGE RankNTypes,FlexibleContexts,NoMonomorphismRestriction,TypeSynonymInstances #-}

module TypeCheck (typecheck) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Functor.Fix

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

import Text.Printf

import AST

data Binding = Var Type | Alias Name deriving (Show)
type ModBinding = Either (Unit ExprF) (Unit TypedE)
type ModMap = Map Name ModBinding
data TCState = TCState { bindings :: Map Name Binding, modules :: ModMap }
type TC = StateT TCState

instance (Monad m) => Applicative (TC m) where
  pure = return
  (<*>) = ap

runTC m mods = runStateT m (TCState { bindings = M.empty, modules = mods })

modifyBindings f = modify (\s -> s { bindings = f (bindings s) })
modifyModules f = modify (\s -> s { modules = f (modules s) })

addBinding name bind = liftIO (printf "addBinding %s -> %s\n" (show name) (show bind)) >> modifyBindings (M.insert name bind) >> (liftIO . print =<< gets bindings)
getBindingType name = traceM ("getBindingType "++show name) $ do
  --liftIO . print =<< gets bindings
  bind <- getBinding name
  case bind of
    (Var typ) -> return typ
    (Alias name) -> getBindingType name
getBinding name = fromJust . M.lookup name <$> gets bindings

getModule name = fromJust . M.lookup name <$> gets modules
putModule name bind = modifyModules (M.insert name bind)

inScope :: MonadIO m => Name -> Binding -> TC m a -> TC m a
inScope name bind m = do
  s <- gets bindings
  addBinding name bind
  r <- m
  modifyBindings (const s)
  return r

typecheck :: (Functor m, MonadIO m, MonadReader (Map Name (Unit ExprF)) m) => Name -> m (Map Name (Unit TypedE))
typecheck name = do
  mods <- asks (M.map Left)
  snd . M.mapEither id . modules . snd <$> runTC (tcUnitByName name) mods

traceM :: MonadIO m => String -> m a -> m a
--traceM str m = liftIO (putStrLn str) >> m <* liftIO (putStr (str++": done\n"))
traceM = flip const

withUnitImported (Unit _ decl) = withDecl (QualifiedName []) decl
withDecls name = flip (foldr (withDecl name))
withDecl name decl@(Decl local def) = traceM ("withDecl "++show decl++" in "++show name) .
  withDef (qualifyName name local) local def
withDef name local def = traceM (printf "withDef %s local %s: %s" (show name) (show local) (show def)) . case def of
  (ModuleDef decls) -> withDecls name decls
  (FunctionDef retT args code) -> inScope name (Var (TFunction retT args)) . inScope local (Alias name)
  (ExternalFunction linkage ret args) -> inScope name (Var (TFunction ret args)) . inScope local (Alias name)

withImport name m = do
  unit <- tcUnitByName name
  traceM (printf "withImport %s" (show name)) (withUnitImported unit m)

tcUnitByName :: MonadIO m => Name -> TC m (Unit TypedE)
tcUnitByName name = do
  mod <- getModule name -- errors if module not found - it must be found
  liftIO (printf "tcUnitByName: %s -> %s\n" (show name) (show mod))
  case mod of
    Left untyped -> tcUnit name untyped
    Right typed -> return typed

putBlackhole name = putModule name (error "Recursive module inclusion")

tcUnit :: MonadIO m => Name -> Unit ExprF -> TC m (Unit TypedE)
tcUnit name (Unit imports decl) = traceM ("tcUnit "++show name) $ do
  putBlackhole name
  unit <- Unit imports <$> foldr withImport (tcDecl name decl) imports
  putModule name (Right unit)
  return unit

tcDecl :: MonadIO m => Name -> Decl ExprF -> TC m (Decl TypedE)
tcDecl name decl@(Decl local def) = traceM (printf "tcDecl %s %s %s" (show name)(show local) (show decl)) $
  Decl local <$> tcDef (qualifyName name local) def

tcDef :: MonadIO m => Name -> Def ExprF -> TC m (Def TypedE)
tcDef name def = traceM ("tcDef "++show name++": "++show def) $ case def of
  (ModuleDef decls) -> ModuleDef <$> mapM (tcDecl name) (decls)
  (FunctionDef retT args code) -> do
    addBinding name (Var (TFunction retT args))
    FunctionDef retT args <$> mapM (tcStmt retT args) code
  (ExternalFunction linkage ret args) -> do
    addBinding name (Var (TFunction ret args))
    return (ExternalFunction linkage ret args)

tcStmt :: MonadIO m => Type -> [FormalParam] -> Statement ExprF -> TC m (Statement TypedE)
tcStmt ret args stmt = traceM ("tcStmt "++show stmt) $ case stmt of
  (ReturnStmt e) -> ReturnStmt <$> tcExprAsType ret e
  (ExprStmt e) -> ExprStmt <$> tcExpr e
  ReturnStmtVoid -> return ReturnStmtVoid
  EmptyStmt -> return EmptyStmt
  CompoundStmt [stmt] -> tcStmt ret args stmt
  CompoundStmt xs -> CompoundStmt <$> mapM (tcStmt ret args) xs
  VarDecl name typ stmt -> VarDecl name typ <$> inScope name (Var typ) (tcStmt ret args stmt)

tcExprAsType :: MonadIO m => Type -> ExprF -> TC m TypedE
tcExprAsType expT e = do
  e@(TypedE t _) <- tcExpr e
  if not (t == expT) then error ("Expression "++show e++" not of expected type "++show expT++" but "++show t) else return e

tcParams :: MonadIO m => [FormalParam] -> [ExprF] -> TC m [TypedE]
tcParams (FormalParam typ _:ps) (x:xs) = liftM2 (:) (tcExprAsType typ x) (tcParams ps xs)
tcParams [VarargParam] xs = mapM tcExpr xs
tcParams [] [] = return []
-- missing cases represent various errors...

tcExpr :: MonadIO m => ExprF -> TC m TypedE
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
