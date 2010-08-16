{-# LANGUAGE RankNTypes,FlexibleContexts,NoMonomorphismRestriction,TypeSynonymInstances,PatternGuards #-}

module TypeCheck (typecheck,maybeM) where

import Control.Applicative hiding (Const)
import Control.Monad.Identity
-- "local" is used as a variable name a lot in this file, let's not get it confused with the Reader definition
import Control.Monad.Reader hiding (local)
import Control.Monad.State

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

import Debug.Trace

import Text.Printf

import AST
import CppToken
import Types.Conv

import Counter() -- Also implements Applicative for StateT o.o

data VarType = Const | Global | NonConst deriving (Show)
data Binding = Var VarType Type | Alias Name deriving (Show)
type ModBinding = Either (Unit ExprF) (Unit TypedE)
type ModMap = Map Name ModBinding
data TCState = TCState { bindings :: Map Name Binding, modules :: ModMap }
type TC = StateT TCState

runTC m = runStateT m . TCState M.empty

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
  bs <- gets bindings
  let r = fromMaybe (error ("getBinding "++show name++": Name not found\n"++unlines (map show (M.toList bs)))) (M.lookup name bs)
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
--traceM str m = liftIO (putStrLn str) >> m >>= \x -> liftIO (putStr (str++": done\n")) >> return x
traceM = flip const

withUnitImported (Unit _ decl) = withDecl (QualifiedName []) decl
withDecls name = flip (foldr (withDecl name))
withDecl name decl@(Decl local def) = traceM ("withDecl "++show decl++" in "++show name) .
  withDef (qualifyName name local) local def
withDef name local def = traceM (printf "withDef %s local %s: %s" (show name) (show local) (show def)) . case def of
  (ModuleDef decls) -> withDecls name decls
  (FunctionDef retT args _) -> inScope name (Var NonConst (TFunction retT args)) . inScope local (Alias name)
  (ExternalFunction _ ret args) -> inScope name (Var NonConst (TFunction ret args)) . inScope local (Alias name)
  (VarDef typ _) -> inScope name (Var Global typ) . inScope local (Alias name)

withImport name m = do
  unit <- tcUnitByName name
  traceM (printf "withImport %s" (show name)) (withUnitImported unit m)

withArg (FormalParam typ (Just name)) m = inScope name (Var Const typ) m
withArg _ m = m

tcUnitByName :: MonadIO m => Name -> TC m (Unit TypedE)
tcUnitByName name = do
  res <- getModule name -- errors if module not found - it must be found
  _ <- liftIO (printf "tcUnitByName: %s: %s\n" (show name) (either (const "not yet typechecked") (const "already typechecked") res))
  case res of
    Left untyped -> tcUnit name untyped
    Right typed -> return typed

putBlackhole name = putModule name (error "Recursive module inclusion")

tcUnit :: MonadIO m => Name -> Unit ExprF -> TC m (Unit TypedE)
tcUnit name (Unit imports decl) = traceM ("tcUnit "++show name) $ do
  putBlackhole name
  unit <- Unit imports <$> foldr withImport (tcDecl (QualifiedName []) decl) imports
  putModule name (Right unit)
  return unit

tcDecl :: MonadIO m => Name -> Decl ExprF -> TC m (Decl TypedE)
tcDecl name decl@(Decl local def) = traceM (printf "tcDecl %s %s %s" (show name)(show local) (show decl)) $
  Decl local <$> tcDef (qualifyName name local) local def

invalidFormalParams [] = False
invalidFormalParams xs = any (== VarargParam) (init xs)

tcDef :: MonadIO m => Name -> Name -> Def ExprF -> TC m (Def TypedE)
tcDef name local def = traceM ("tcDef "++show name++": "++show def) $ case def of
  (ModuleDef decls) -> ModuleDef <$> mapM (tcDecl name) (decls)
  (FunctionDef retT args code) -> do
    when (invalidFormalParams args) (tcError "Malformed formal parameter list")
    addBinding name (Var NonConst (TFunction retT args))
    addBinding local (Alias name)
    FunctionDef retT args <$> foldr withArg (tcStmt retT args code) args
  (ExternalFunction linkage ret args) -> do
    addBinding name (Var NonConst (TFunction ret args))
    addBinding local (Alias name)
    return (ExternalFunction linkage ret args)
  (VarDef typ e) -> do
    let typ' = case typ of
          TConst t -> t
          _ -> typ
    addBinding name (Var Global typ')
    VarDef typ <$> maybeM (tcExprAsType typ') e
  (TypeDef typ) -> return (TypeDef typ)

maybeM :: Applicative m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f (Just x) = Just <$> f x
maybeM _ Nothing = pure Nothing

inScopeVars :: MonadIO m => [(Type,Name,Maybe ExprF)] -> ([(Type,Name,Maybe TypedE)] -> TC m a) -> TC m a
inScopeVars vars m = f [] vars
  where
    -- f :: [(Name,Type,Maybe TypedE)] -> [(Name,Type,Maybe ExprF)] -> TC m a
    f acc [] = m (reverse acc)
    f acc (v@(_,name,_):vs) = inScope name var (tcVar v >>= \v' -> f (v':acc) vs)
      where
        var = case v of
          (TConst typ,_,_) -> (Var Const typ)
          (typ,_,_) -> (Var NonConst typ)

    tcVar (typ,name,init) = (,,) typ name <$> g typ init
    g typ@(TConst _) (Just init) = Just <$> tcExprAsType typ init
    g (TConst _) Nothing = tcError "Constant variable without initializer"
    g typ init = maybeM (tcExprAsType typ) init

tcStmt :: MonadIO m => Type -> [FormalParam] -> Statement ExprF -> TC m (Statement TypedE)
tcStmt ret args stmt = traceM ("tcStmt "++show stmt) $ case stmt of
  (ReturnStmt e)     -> ReturnStmt <$> tcExprAsType ret e
  (ExprStmt e)       -> ExprStmt <$> tcExpr e
  ReturnStmtVoid | TVoid <- ret -> return ReturnStmtVoid
  ReturnStmtVoid     -> tcError ("void return in function returning "++show ret)
  EmptyStmt          -> return EmptyStmt
  CompoundStmt vars xs    -> inScopeVars vars (\vars' -> CompoundStmt vars' <$> mapM (tcStmt ret args) xs)
  IfStmt cond t f -> IfStmt <$> tcExprAsType TBool cond <*> tcStmt ret args t <*> tcStmt ret args f
  WhileStmt cond body -> WhileStmt <$> tcExprAsType TBool cond <*> tcStmt ret args body
  other -> tcError ("Unknown statement "++show other)

traceShowRes str x = trace str $ trace (str++show x) x

implicitlyConvertType to orig@(TypedE from _) =
  traceShowRes ("implicitlyConvertType "++show orig++" to "++show to++", from "++show from++": ") $
  case implicitConversions to from of
    Just fun -> Just (fun orig)
    Nothing -> Nothing

tcExprAsType :: MonadIO m => Type -> ExprF -> TC m TypedE
tcExprAsType expT e = do
  typed@(TypedE t _) <- tcExpr e
  case implicitlyConvertType expT typed of
    Just typed' -> return typed'
    Nothing -> tcError ("Expression "++show typed++" not of expected type "++show expT++" but "++show t++", and no implicit conversions were available")

tcParams :: MonadIO m => [FormalParam] -> [ExprF] -> TC m [TypedE]
tcParams (FormalParam typ _:ps) (x:xs) = liftM2 (:) (tcExprAsType typ x) (tcParams ps xs)
tcParams [VarargParam]          xs     = mapM tcExpr xs
tcParams (VarargParam:_)        _      = error "Vararg param in non-last position. The type-checker should have caught this already!"
tcParams []                     []     = return []
tcParams formal                 actual = tcError ("Argument number mismatch. Remaining formals: "++show formal++", actuals: "++show actual)

tcExpr :: MonadIO m => ExprF -> TC m TypedE
tcExpr e = case outF e of
  (EBool b) -> return (TypedE TBool (EBool b))
  (EInt i) -> return (TypedE TInt (EInt i))
  (EString str) -> return (TypedE (TPtr (TConst TChar)) (EString str))
  (EChar c) -> return (TypedE TChar (EChar c))
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
  (EArrayIndex arr ix) -> do
    arr'@(TypedE arrT _) <- tcExpr arr
    ix' <- tcExprAsType TInt ix
    elemType <- case arrT of
          TPtr x -> return x
          TArray _ elem -> return elem
          _ -> tcError ("Indexing performed on non-indexable type "++show arrT)
    return (TypedE elemType (EArrayIndex arr' ix'))
  (EFieldAccess field el) -> do
    (TypedE typ exp) <- tcExpr el
    trace ("EFieldAccess: "++show typ++" "++show exp++", "++show field) $ return ()
    ft <- case typ of
          TStruct fields -> case lookup field fields of
            Just ft -> return ft
            Nothing -> tcError ("Field "++show field++" not found in struct "++show typ)
          _ -> tcError ("Accessing field "++show field++" in non-struct type "++show typ)
    case exp of
      EDeref struct -> return (TypedE ft (EDeref (TypedE (TPtr ft) (EFieldAccess field struct))))
      struct -> return (TypedE ft (EFieldAccess field (TypedE typ struct)))
  (EPostfix op e) -> do
    -- must be an lvalue
    e@(TypedE typ ptrExpr) <- tcExpr e
    case ptrExpr of
      (EDeref _) -> return ()
      _ -> tcError ("Invalid postfix operator on non-lvalue "++show e)
    return (TypedE typ (EPostfix op e))
  (EDeref ptr) -> do
    e@(TypedE (TPtr typ) _) <- tcExpr ptr
    return (TypedE typ (EDeref e))
  (EUnary op e) -> do
    te@(TypedE res _) <- tcUnary (snd op) e
    return (TypedE res (EUnary op te))
  ENullPtr -> return (TypedE TNullPtr ENullPtr)
  ECast to e -> do
    typed@(TypedE typ _) <- tcExpr e
    when (not (checkCast to typ)) $ tcError ("Invalid cast from "++show typ++" to "++show to)
    return (TypedE to (ECast to typed))
  other -> error ("tcExpr: Unknown expression "++show other)

checkCast (TPtr _) (TPtr TVoid) = True
checkCast (TConst (TPtr _)) (TPtr TVoid) = True
checkCast (TPtr (TConst _)) (TPtr (TConst TVoid)) = True
checkCast _ _ = False

relopRule x = return (TBool, x)
-- FIXME This only handles pointer arithmetic where the *right-hand* argument is a pointer, not the flipped one...
additiveRule TInt = return (TInt,TInt)
additiveRule TChar = return (TChar,TChar)
additiveRule (TPtr t) = return (TPtr t, TInt)
additiveRule other = error ("additiveRule: unhandled type "++show other)

binopTypeRule Equal = relopRule
binopTypeRule LessThan = relopRule
binopTypeRule GreaterOrEqual = relopRule
binopTypeRule Plus = additiveRule
binopTypeRule Minus = additiveRule
binopTypeRule other = \_ -> tcError ("Unhandled binary operator: "++show other)

tcUnary LogicalNot = tcExprAsType TBool
tcUnary Minus = tcExpr -- TODO Check that it's something that we can do arithmetic on...
tcUnary op = \_ -> tcError ("Unknown unary operator "++show op)
