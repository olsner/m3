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
data Binding = Var VarType Type | Alias Name | Type Type deriving (Show)
type ModBinding = Either (Unit LocE) (Unit TypedE)
type ModMap = Map Name ModBinding
data TCState = TCState { bindings :: Map Name Binding, modules :: ModMap }
type TC = StateT TCState

runTC m = runStateT m . TCState M.empty

tcError :: Location -> String -> TC m a
tcError loc msg = error ("[TC] "++show loc ++ ": " ++ msg)

modifyBindings f = modify (\s -> s { bindings = f (bindings s) })
modifyModules f = modify (\s -> s { modules = f (modules s) })

-- TODO Add checking to make sure that added aliases don't create loops.
addBinding name bind = do
  --liftIO (printf "addBinding %s -> %s\n" (show name) (show bind))
  modifyBindings (M.insert name bind)
  --liftIO . print =<< gets bindings
getBinding loc name = do
  bs <- gets bindings
  let r = M.lookup name bs
  case r of
    Just (Alias alias) -> getBinding loc alias
    Just r             -> return (name,r)
    Nothing -> tcError loc ("getBinding "++show name++": Name not found\n"++unlines (map show (M.toList bs)))
getBoundType loc name = do
  b <- getBinding loc name
  case b of
    (_,Type t) -> return t
    _ -> tcError loc ("getBoundType "++show name++": Name not a type")

getModule name = fromJust . M.lookup name <$> gets modules
putModule name bind = modifyModules (M.insert name bind)

inScope :: MonadIO m => Name -> Binding -> TC m a -> TC m a
inScope name bind m = do
  s <- gets bindings
  addBinding name bind
  r <- m
  modifyBindings (const s)
  return r

typecheck :: (Functor m, MonadIO m, MonadReader (Map Name (Unit LocE)) m) => Name -> m (Map Name (Unit TypedE))
typecheck name = do
  mods <- asks (M.map Left)
  snd . M.mapEither id . modules . snd <$> runTC (tcUnitByName name) mods

traceM :: MonadIO m => Show a => String -> m a -> m a
--traceM str m = liftIO (putStrLn str) >> m >>= \x -> liftIO (putStr (str++": => "++show x++"\n")) >> return x
traceM = flip const

withUnitImported (Unit _ decl) = withDecl (QualifiedName []) decl
withDecls name = flip (foldr (withDecl name))
withDecl name (Loc loc decl@(Decl local def)) = traceM ("withDecl "++show decl++" in "++show name) .
  withDef loc (qualifyName name local) local def

withDef _loc name local def = traceM (printf "withDef %s local %s: %s" (show name) (show local) (show def)) . case def of
  (ModuleDef decls) -> withDecls name decls
  (FunctionDef retT args _) -> inScope name (Var NonConst (TFunction retT args)) . inScope local (Alias name)
  (ExternalFunction _ ret args _) -> inScope name (Var NonConst (TFunction ret args)) . inScope local (Alias name)
  (VarDef typ _) -> inScope name (Var Global typ) . inScope local (Alias name)
  (TypeDef typ) -> inScope name (Type typ) . inScope local (Alias name)

withImport name m = do
  unit <- tcUnitByName name
  traceM (printf "withImport %s" (show name)) (withUnitImported unit m)

withArg (FormalParam typ (Just name)) m = inScope name (Var Const typ) m
withArg _ m = m

tcUnitByName :: Functor m => MonadIO m => Name -> TC m (Unit TypedE)
tcUnitByName name = do
  res <- getModule name -- errors if module not found - it must be found
  -- TODO debugging-printing
  -- _ <- liftIO (printf "tcUnitByName: %s: %s\n" (show name) (either (const "not yet typechecked") (const "already typechecked") res))
  case res of
    Left untyped -> tcUnit name untyped
    Right typed -> return typed

-- TODO Do this properly so that we can give an error message with a location
-- instead whenever there's recursion.
putBlackhole name = putModule name (error "Recursive module inclusion")

tcUnit :: Functor m => MonadIO m => Name -> Unit LocE -> TC m (Unit TypedE)
tcUnit name (Unit imports decl) = traceM ("tcUnit "++show name) $ do
  putBlackhole name
  unit <- Unit imports <$> foldr withImport (tcDecl (QualifiedName []) decl) imports
  putModule name (Right unit)
  return unit

tcDecl :: Functor m => MonadIO m => Name -> LocDecl LocE -> TC m (LocDecl TypedE)
tcDecl name (Loc loc decl@(Decl local def)) = traceM (printf "tcDecl %s %s %s" (show name)(show local) (show decl)) $
  Loc loc <$> Decl local <$> tcDef loc (qualifyName name local) local def

invalidFormalParams [] = False
invalidFormalParams xs = any (== VarargParam) (init xs)

-- tcType :: Functor m => Location -> Type -> TC m Type
tcType = foldTypeM f
  where
    f loc t = traceM (printf "tcType %s %s" (show loc) (show t)) $ case t of
      (TNamedType name) -> tcType loc =<< getBoundType loc name
      (TFunction ret fps) -> TFunction <$> tcType loc ret <*> tcFormalParams loc fps
      _ -> return t

tcFormalParams loc fps = do
  when (invalidFormalParams fps) (tcError loc "Malformed formal parameter list")
  mapFormalParamTypes (tcType loc) fps

tcDef :: Functor m => MonadIO m => Location -> Name -> Name -> Def LocE -> TC m (Def TypedE)
tcDef loc name local def = traceM ("tcDef ("++show loc++") "++show name++": "++show def) $ case def of
  (ModuleDef decls) -> ModuleDef <$> mapM (tcDecl name) decls
  (FunctionDef retT args code) -> do
    funType@(TFunction retT args) <- tcType loc (TFunction retT args)
    addBinding name (Var NonConst funType)
    addBinding local (Alias name)
    FunctionDef retT args <$> foldr withArg (tcStmt retT args code) args
  (ExternalFunction linkage ret args extname) -> do
    funType@(TFunction ret args) <- tcType loc (TFunction ret args)
    addBinding name (Var NonConst funType)
    addBinding local (Alias name)
    return (ExternalFunction linkage ret args extname)
  (VarDef typ e) -> do
    typ <- tcType loc typ
    let unconst (TConst t) = t; unconst t = t
    addBinding name (Var Global (unconst typ))
    addBinding local (Alias name)
    VarDef typ <$> maybeM (tcExprAsType typ) e
  (TypeDef typ) -> do
    typ <- tcType loc typ
    addBinding name (Type typ)
    addBinding local (Alias name)
    return (TypeDef typ)


tcLocalDef loc name def = traceM ("tcLocalDef ("++show loc++") "++show name++": "++show def) $ case def of
  (VarDef (TConst _) Nothing) -> tcError loc "Constant variable without initializer"
  (VarDef typ e) -> do
    typ <- tcType loc typ
    VarDef typ <$> maybeM (tcExprAsType typ) e
  (TypeDef typ) -> TypeDef <$> tcType loc typ
  _ -> tcError loc ("Unexpected local definition of "++show name++" as "++show def)

maybeM :: Applicative m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f (Just x) = Just <$> f x
maybeM _ Nothing = pure Nothing

inScopeVars :: Functor m => MonadIO m => [LocDecl LocE] -> ([LocDecl TypedE] -> TC m a) -> TC m a
inScopeVars vars m = f [] vars
  where
    -- f :: [(Name,Type,Maybe TypedE)] -> [(Name,Type,Maybe LocE)] -> TC m a
    f acc [] = m (reverse acc)
    f acc ((Loc loc (Decl name def)):vs) = do
      -- FIXME This should be done with 'name' in scope (but (var def) also
      -- needs to have the return value to get the right type).
      def <- tcLocalDef loc name def
      inScope name (var def) (f (Loc loc (Decl name def):acc) vs)

    var (VarDef typ _) = case typ of
      (TConst typ) -> Var Const typ
      _ -> Var NonConst typ
    var (TypeDef typ) = Type typ
    var x = error ("Unhandled definition "++show x)

tcStmt :: Functor m => MonadIO m => Type -> [FormalParam] -> LocStatement LocE -> TC m (LocStatement TypedE)
tcStmt ret args (Loc loc stmt) = traceM ("tcStmt "++show stmt) $ Loc loc <$> case stmt of
  (ReturnStmt e)     -> ReturnStmt <$> tcExprAsType ret e
  (ExprStmt e)       -> ExprStmt <$> tcExpr e
  ReturnStmtVoid | TVoid <- ret -> return ReturnStmtVoid
  ReturnStmtVoid     -> tcError loc ("void return in function returning "++show ret)
  EmptyStmt          -> return EmptyStmt
  CompoundStmt vars xs    -> inScopeVars vars (\vars' -> CompoundStmt vars' <$> mapM (tcStmt ret args) xs)
  IfStmt cond t f -> IfStmt <$> tcExprAsType TBool cond <*> tcStmt ret args t <*> tcStmt ret args f
  WhileStmt cond body -> WhileStmt <$> tcExprAsType TBool cond <*> tcStmt ret args body
  BreakStmt -> return BreakStmt
  ContinueStmt -> return ContinueStmt
  other -> tcError loc ("tcStmt: Unknown statement "++show other)

-- traceShowRes str x = trace str $ trace (str++show x) x

implicitlyConvertType to orig@(TypedE _ from _) =
  -- traceShowRes ("implicitlyConvertType "++show orig++" to "++show to++", from "++show from++": ") $
  case implicitConversions to from of
    Just fun -> Just (fun orig)
    Nothing -> Nothing

tcExprAsType :: Functor m => MonadIO m => Type -> LocE -> TC m TypedE
--tcExprAsType (TNamedType t) e@(LocE loc _) = tcError loc ("Expression "++show e++" has unresolved type "++show t)
tcExprAsType expT e@(LocE loc _) = traceM ("tcExprAsType "++show expT++" "++show e) $ do
  expT <- tcType loc expT
  typed@(TypedE _ t _) <- tcExpr e
  case implicitlyConvertType expT typed of
    Just typed' -> return typed'
    Nothing -> tcError loc ("Expression "++show typed++" not of expected type "++show expT++" but "++show t++", and no implicit conversions were available")

tcParams :: Functor m => MonadIO m => [FormalParam] -> [LocE] -> TC m [TypedE]
tcParams (FormalParam typ _:ps) (x:xs) = liftM2 (:) (tcExprAsType typ x) (tcParams ps xs)
tcParams [VarargParam]          xs     = mapM tcExpr xs
tcParams (VarargParam:_)        _      = tcError dummyLocation "Vararg param in non-last position. The type-checker should have caught this already!"
tcParams []                     []     = return []
tcParams formal                 actual = tcError dummyLocation ("Argument number mismatch. Remaining formals: "++show formal++", actuals: "++show actual)

tcLValueExpr loc e = do
  lv@(TypedE _ typ ptrExpr) <- tcExpr e
  case ptrExpr of
    (EDeref _) -> return lv
    _ -> tcError loc ("Not an lvalue: "++show e)

tcExpr :: Functor m => MonadIO m => LocE -> TC m TypedE
tcExpr (LocE loc e) = traceM ("tcExpr "++show loc++" "++show e) $ case e of
  (EBool b) -> return (typedE TBool (EBool b))
  (EInt i) -> return (typedE TInt (EInt i))
  (EString str) -> return (typedE (TPtr (TConst TChar)) (EString str))
  (EChar c) -> return (typedE TChar (EChar c))
  (EVarRef name) -> do
    (varName,Var qual typ) <- getBinding loc name
    return $ typedE typ $ case qual of
      Const -> EVarRef varName
      _     -> EDeref $ typedE (TPtr typ) $ EVarRef varName
  (EFunCall fun args) -> do
    (TypedE _ typ fune) <- tcExpr fun
    fun_ <- typedE (TPtr typ) <$> case fune of
                (EDeref (TypedE _ _ funptr)) -> return funptr
                _ -> tcError loc ("Function call on non-lvalue "++show fune)
    case typ of
      (TFunction retT params) -> do
        args_ <- tcParams params args
        return (typedE retT (EFunCall fun_ args_))
      other -> tcError loc ("Function call on "++show other++" of non-function type "++show typ)
  (EAssignment op lval rval) -> do
    lv@(TypedE _ lvT _) <- tcLValueExpr loc lval
    rv <- tcExprAsType lvT rval
    return (typedE lvT (EAssignment op lv rv))
  (EBinary op x y) -> do
    tx@(TypedE _ t _) <- tcExpr x
    (res,op2type) <- binopTypeRule (snd op) loc t
    ty <- tcExprAsType op2type y
    return (typedE res (EBinary op tx ty))
  (EArrayIndex arr ix) -> do
    arr'@(TypedE _ arrT _) <- tcExpr arr
    ix' <- tcExprAsType TInt ix
    elemType <- case arrT of
          TPtr x -> return x
          TArray _ elem -> return elem
          _ -> tcError loc ("Indexing performed on non-indexable type "++show arrT)
    return (typedE elemType (EArrayIndex arr' ix'))
  (EFieldAccess field el) -> do
    (TypedE structLoc typ exp) <- tcExpr el
    -- TODO Debugging/tracing output control
    -- trace ("EFieldAccess: "++show typ++" "++show exp++", "++show field) $ return ()
    ft <- case typ of
          TStruct fields -> case lookup field (map locData fields) of
            Just ft -> return ft
            Nothing -> tcError loc ("Field "++show field++" not found in struct "++show typ)
          _ -> tcError loc ("Accessing field "++show field++" in non-struct type "++show typ)
    case exp of
      EDeref struct -> return (typedE ft (EDeref (typedE (TPtr ft) (EFieldAccess field struct))))
      struct -> return (typedE ft (EFieldAccess field (TypedE structLoc typ struct)))
  (EPostfix op e) -> do
    e@(TypedE _ typ _) <- tcLValueExpr loc e
    return (typedE typ (EPostfix op e))
  (EPrefix op e) -> do
    te@(TypedE _ typ _) <- tcLValueExpr loc e
    return (typedE typ (EPrefix op te))
  (EDeref ptr) -> do
    e@(TypedE _ (TPtr typ) _) <- tcExpr ptr
    return (typedE typ (EDeref e))
  (EUnary op e) -> do
    te@(TypedE _ res _) <- tcUnary (snd op) loc e
    return (typedE res (EUnary op te))
  ENullPtr -> return (typedE TNullPtr ENullPtr)
  ECast to e -> do
    to <- tcType loc to
    typed@(TypedE _ typ _) <- tcExpr e
    when (not (checkCast to typ)) $ tcError loc ("Invalid cast from "++show typ++" to "++show to)
    return (typedE to (ECast to typed))
  other -> tcError loc ("tcExpr: Unknown expression "++show other)
  where
    typedE (TNamedType name) expr = error ("tcExpr: unresolved type "++show name++" in "++show expr)
    typedE t e = TypedE loc t e

checkCast (TPtr _) (TPtr TVoid) = True
checkCast (TConst (TPtr _)) (TPtr TVoid) = True
checkCast (TPtr (TConst _)) (TPtr (TConst TVoid)) = True
checkCast _ _ = False

relopRule _ x = return (TBool, x)
-- FIXME This only handles pointer arithmetic where the *right-hand* argument is a pointer, not the flipped one...
additiveRule _ (TPtr t) = return (TPtr t, TInt)
additiveRule loc t = arithmeticRule loc t

arithmeticRule _ TInt = return (TInt,TInt)
arithmeticRule _ TChar = return (TChar,TChar)
arithmeticRule loc other = tcError loc ("arithmeticRule: unhandled type "++show other)

binopTypeRule Equal = relopRule
binopTypeRule NotEqual = relopRule
binopTypeRule LessThan = relopRule
binopTypeRule GreaterThan = relopRule
binopTypeRule GreaterOrEqual = relopRule
binopTypeRule Plus = additiveRule
binopTypeRule Minus = additiveRule
binopTypeRule Asterix = arithmeticRule
binopTypeRule Division = arithmeticRule
binopTypeRule Modulo = arithmeticRule
binopTypeRule other = \loc _ -> tcError loc ("Unhandled binary operator: "++show other)

tcUnary LogicalNot = const (tcExprAsType TBool)
tcUnary Minus = const tcExpr -- TODO Check that it's something that we can do arithmetic on...
tcUnary op = \loc _ -> tcError loc ("Unknown unary operator "++show op)
