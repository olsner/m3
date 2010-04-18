{-# LANGUAGE RankNTypes,FlexibleContexts,NoMonomorphismRestriction,TypeSynonymInstances,PatternGuards #-}

module TypeCheck (typecheck,maybeM) where

import Control.Applicative hiding (Const)
import Control.Monad.Identity
-- "local" is used as a variable name a lot in this file, let's not get it confused with the Reader definition
import Control.Monad.Reader hiding (local)
import Control.Monad.State
import Control.Functor.Fix

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

import Text.Printf

import AST
import CppToken

data VarType = Const | NonConst deriving (Show)
data Binding = Var VarType Type | Alias Name deriving (Show)
type ModBinding = Either (Unit ExprF) (Unit TypedE)
type ModMap = Map Name ModBinding
data TCState = TCState { bindings :: Map Name Binding, modules :: ModMap }
type TC = StateT TCState

instance (Monad m) => Applicative (TC m) where
  pure = return
  (<*>) = ap

runTC m mods = runStateT m (TCState { bindings = M.empty, modules = mods })

tcError :: String -> TC m a
tcError msg = error msg

modifyBindings f = modify (\s -> s { bindings = f (bindings s) })
modifyModules f = modify (\s -> s { modules = f (modules s) })

-- TODO Add checking to make sure that added aliases don't create loops.
addBinding name bind = do
  --liftIO (printf "addBinding %s -> %s\n" (show name) (show bind))
  modifyBindings (M.insert name bind)
  --liftIO . print =<< gets bindings
getBinding name = do
  r <- fromMaybe (error ("getBinding "++show name++": Name not found")) . M.lookup name <$> gets bindings
  case r of
    (Alias alias) -> getBinding alias
    _             -> return (name,r)

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
  (FunctionDef retT args _) -> inScope name (Var NonConst (TFunction retT args)) . inScope local (Alias name)
  (ExternalFunction _ ret args) -> inScope name (Var NonConst (TFunction ret args)) . inScope local (Alias name)

withImport name m = do
  unit <- tcUnitByName name
  traceM (printf "withImport %s" (show name)) (withUnitImported unit m)

withArg (FormalParam typ (Just name)) m = inScope name (Var Const typ) m
withArg _ m = m

tcUnitByName :: MonadIO m => Name -> TC m (Unit TypedE)
tcUnitByName name = do
  res <- getModule name -- errors if module not found - it must be found
  liftIO (printf "tcUnitByName: %s: %s\n" (show name) (either (const "not yet typechecked") (const "already typechecked") res))
  case res of
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
    addBinding name (Var NonConst (TFunction retT args))
    FunctionDef retT args <$> foldr withArg (mapM (tcStmt retT args) code) args
  (ExternalFunction linkage ret args) -> do
    addBinding name (Var NonConst (TFunction ret args))
    return (ExternalFunction linkage ret args)

maybeM :: Applicative m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f (Just x) = Just <$> f x
maybeM _ Nothing = pure Nothing

tcStmt :: MonadIO m => Type -> [FormalParam] -> Statement ExprF -> TC m (Statement TypedE)
tcStmt ret args stmt = traceM ("tcStmt "++show stmt) $ case stmt of
  (ReturnStmt e)     -> ReturnStmt <$> tcExprAsType ret e
  (ExprStmt e)       -> ExprStmt <$> tcExpr e
  ReturnStmtVoid | TVoid <- ret -> return ReturnStmtVoid
  ReturnStmtVoid     -> tcError ("void return in function returning "++show ret)
  EmptyStmt          -> return EmptyStmt
  CompoundStmt [x]   -> tcStmt ret args x
  CompoundStmt xs    -> CompoundStmt <$> mapM (tcStmt ret args) xs
  VarDecl name (TConst typ) init x -> inScope name (Var Const typ) $
    VarDecl name (TConst typ) <$> (Just <$> tcExprAsType typ (fromJust init)) <*> tcStmt ret args x
  VarDecl name typ init x -> inScope name (Var NonConst typ) $
    VarDecl name typ <$> maybeM (tcExprAsType typ) init <*> tcStmt ret args x
  IfStmt cond t f -> IfStmt <$> tcExprAsType TBool cond <*> tcStmt ret args t <*> tcStmt ret args f

tcExprAsType :: MonadIO m => Type -> ExprF -> TC m TypedE
tcExprAsType expT e = do
  typed@(TypedE t _) <- tcExpr e
  if not (t == expT) then error ("Expression "++show typed++" not of expected type "++show expT++" but "++show t) else return typed

tcParams :: MonadIO m => [FormalParam] -> [ExprF] -> TC m [TypedE]
tcParams (FormalParam typ _:ps) (x:xs) = liftM2 (:) (tcExprAsType typ x) (tcParams ps xs)
tcParams [VarargParam]          xs     = mapM tcExpr xs
tcParams (VarargParam:_)        _      = error "Vararg param in non-last position. The type-checker should have caught this already!"
tcParams []                     []     = return []
tcParams _                      _      = error "Argument number mismatch in code-generator. The type-checker should have caught this already!"

tcExpr :: MonadIO m => ExprF -> TC m TypedE
tcExpr e = case outF e of
  (EBool b) -> return (TypedE TBool (EBool b))
  (EInt i) -> return (TypedE TInt (EInt i))
  (EString str) -> return (TypedE (TPtr (TConst TChar)) (EString str))
  (EVarRef name) -> do
    (varName,Var qual typ) <- getBinding name
    return $ TypedE typ $ case qual of
      Const -> EVarRef varName
      _     -> EDeref $ TypedE (TPtr typ) $ EVarRef varName
  (EFunCall fun args) -> do
    (TypedE typ fune) <- tcExpr fun
    let fun_ = TypedE (TPtr typ) $ case fune of
                (EDeref (TypedE _ funptr)) -> funptr
                _ -> error ("Function call on non-lvalue "++show fune)
    case typ of
      (TFunction retT params) -> do
        args_ <- tcParams params args
        return (TypedE retT (EFunCall fun_ args_))
      other -> error ("Function call on "++show other++" of non-function type "++show typ)
  (EAssignment op lval rval) -> do
    lv@(TypedE lvT (EDeref _)) <- tcExpr lval
    rv <- tcExprAsType lvT rval
    return (TypedE lvT (EAssignment op lv rv))
  (EBinary op x y) -> do
    tx@(TypedE t _) <- tcExpr x
    (res,op2type) <- binopTypeRule (snd op) t
    ty <- tcExprAsType op2type y
    return (TypedE res (EBinary op tx ty))
  other -> error ("tcExpr: Unknown expression "++show other)

binopTypeRule Equal op1type = return (TBool, op1type) -- TODO Also needs to check that op1type is supported by the operator
