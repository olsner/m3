{-# LANGUAGE TypeSynonymInstances,DeriveDataTypeable,DeriveFunctor,TypeFamilies,StandaloneDeriving,FlexibleContexts,UndecidableInstances,NoMonomorphismRestriction,MultiParamTypeClasses,FunctionalDependencies #-}

import Control.Applicative
import Control.Monad.RWS

import Data.List (intercalate)
import Data.Maybe
import Data.Traversable hiding (mapM)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Counter
import CppToken (Token)
import SourcePos

import AST

formalParams ts = [FormalParam t Nothing | t <- ts]

baseT (TStruct _) = True
baseT t = elem t basetypes
  where basetypes = [TVoid, TInt, TChar, TBool, TNullPtr, TDummyBase]
dummyBaseT = TDummyBase

subBaseT TVoid _ = False
-- Widening conversions
subBaseT TChar TInt = True
subBaseT TBool TInt = True
-- dubious, narrowing cast from int to char
subBaseT TInt TChar = True
-- Implicit comparison to zero/null
subBaseT TInt TBool = True
subBaseT TChar TBool = True
subBaseT TNullPtr TBool = True
-- There are other subtype relations too, but not between base types
subBaseT _ _ = False

-- Inferred constraint type. No "fresh variable placeholders" remain, they have
-- been given variables now.
data CType =
    CTypeVar Name
  | CType (Type CType)
  deriving (Eq,Ord)

instance Show CType where
  showsPrec p (CTypeVar n) = showsPrec p n
  showsPrec p (CType t) = showsPrec p t

base (CTypeVar _) = False
base (CType t) = baseT t

-- | First argument is a subtype of the second argument, both are base types.
subBase (CType t) (CType u) = subBaseT t u

tBool = wrapT TBool
tChar = wrapT TChar
tInt = wrapT TInt
tVoid = wrapT TVoid
tNullPtr = wrapT TNullPtr
tPtr p = wrapT (TPtr p)
tArray n t = wrapT (TArray n t)
tConst t = wrapT (TConst t)
tFunction ret args = wrapT (TFunction ret args)

instance TypeF CType where
  wrapT = CType
  foldTM f (CType t) = f =<< foldTypeM (foldTM f) t
  foldTM f t = pure t

-- foldExprM :: (Applicative f) => (e -> f e') -> (t -> f t') -> Expr t e -> f (Expr t' e')

data Constraint t = Eq t t | Sub t t
  deriving (Show,Eq,Ord,Functor)
type FConstr = Constraint UType
type CSet t = Set (Constraint t)
type CCSet = CSet CType

class ExprF t e | e -> t where
  wrapE :: Expr t e -> e

eFunCall f args = wrapE (EFunCall f args)
eVarRef name = wrapE (EVarRef (mkName name))
eSeq e1 e2 = wrapE (ESeq e1 e2)

data CTypedE = CTypedE Location CType (Expr CType CTypedE)
  deriving (Show,Eq)

instance ExprF FType LocE where
  wrapE = LocE dummyLocation

foldExprM :: (Applicative f) => (e -> f e') -> (t -> f t') -> Expr t e -> f (Expr t' e')
foldExprM g tf e = case e of
  EFunCall fun args -> EFunCall <$> g fun <*> traverse g args
  EConditional co tr fa -> EConditional <$> g co <*> g tr <*> g fa
  EAssignment t l r -> EAssignment t <$> g l <*> g r
  EDeref e -> EDeref <$> g e
  EFieldAccess nm e -> EFieldAccess nm <$> g e
  ESeq l r -> ESeq <$> g l <*> g r
  EArrToPtr e -> EArrToPtr <$> g e
  ECast t e -> ECast <$> tf t <*> g e
  EVarRef n -> pure (EVarRef n)
  EInt i -> pure (EInt i)
  EBool b -> pure (EBool b)
  ENullPtr -> pure ENullPtr

foldLocE :: Applicative m => Monad m => (Location -> Expr t' e' -> m e') -> (FType -> m t') -> LocE -> m (Expr t' e')
foldLocE f tf (LocE loc e) = foldExprM g tf e
  where
    g e = f loc =<< foldLocE f tf e

type Context t = Map Name t

type CM_ t t' a = CounterT Int (RWS (Context t) [Constraint t'] ()) a
type CM a = CM_ CType CType a

constraints :: LocE -> Context CType -> (Set (Constraint CType), CTypedE)
constraints e ctx = (S.fromList out, e')
  where 
    (e',out) = evalRWS (runCounterT 0 $ cgExpr e) ctx ()

printSet = mapM_ print . S.toList
printMap = mapM_ print . M.toList

printConstraints e ctx = do
  putStr "Context: " >> print ctx
  putStr "Expr: " >> print e
  putStr "Type: " >> print typ
  putStrLn "Constraints:"
  printSet constrs
  putStrLn "Weak constraints"
  printSet (S.map upConstr constrs)
  putStr "Constraints weakly unifiable? "
  case weaklyUnifiable constrs M.empty of
    Just err -> putStrLn ("No; " ++ err)
    Nothing -> putStrLn "Yes"
  putStrLn "Simplified constraints:"
  printSet constrSimpl
  putStrLn "Simplification assignments:"
  printMap substSimpl
  putStrLn "Unhandled/unsimplified constraints:"
  printSet failedSimpl
  where
    (constrs, CTypedE _ typ _) = constraints e ctx
    (constrSimpl, failedSimpl, substSimpl) = simplifyConstraints constrs

testExpr :: LocE
testExpr = eSeq (eFunCall (eVarRef "fun") [eVarRef "arg1", eVarRef "arg2"])
                (eFunCall (eVarRef "fun2") [eVarRef "arg1", eVarRef "arg2"])
testContext = M.mapKeys mkName $ M.fromList $
  [("fun", CType (TFunction (CType TInt) (formalParams [CType TInt,CType TInt])))
  ,("fun2", CType (TFunction (CType TChar) (formalParams [CType TChar,CType TChar])))
  ,("arg1", cTypeVar "arg1")
  ,("arg2", cTypeVar "arg2")]
test = printConstraints testExpr testContext

cTypeVar s = CTypeVar (mkName s)

freshTypeVar = (\c -> cTypeVar ("$." ++ show c)) <$> getAndInc

-- cg* = constraint generation, CM = constraing-gen monad

cgExpr :: LocE -> CM CTypedE
cgExpr e@(LocE loc _) = f loc =<< foldLocE f tf e
  where
    tf :: FType -> CM CType
    tf (FType t) = CType <$> foldTypeM tf t
    -- This should probably rather take Expr t' e' -> e', i.e. with the
    -- transformation already applied in all subexpressions.
    f loc e = case e of
      (EFunCall fun@(CTypedE _ funT _) args) -> do
        argTypeVars <- replicateM (length args) freshTypeVar
        let formals = map (\t -> FormalParam t Nothing) argTypeVars
        ret <- freshTypeVar
        tell [Eq funT (CType (TFunction ret formals))]
        let argExprTypes = map (\(CTypedE _ t _) -> t) args
        tell (zipWith Sub argExprTypes argTypeVars)
        pure (CTypedE loc ret (EFunCall fun args))
      (ESeq e1 e2@(CTypedE _ t _)) -> do
        pure (CTypedE loc t (ESeq e1 e2))
      (EVarRef name) -> do
        Just tp <- asks (M.lookup name)
        pure (CTypedE loc tp (EVarRef name))

weaklyUnifiable cs ctx = unifiable (S.map upConstr cs) ctx
-- Nothing = unifiable, Just str = error
unifiable :: Set (CType, CType) -> Context CType -> Maybe String
unifiable cs _ | S.null cs = Nothing
unifiable ccs ctx = case c of
    (s,t) | s == t -> unifiable cs ctx
    (CTypeVar s, t)
      | Just s' <- M.lookup s ctx -> unifiable (S.insert (s', t) cs) ctx
      | not (s `occursIn` t) -> unifiable cs (M.insert s t ctx)
    (t, s@(CTypeVar _)) -> unifiable (S.insert (s,t) cs) ctx
    (CType s, CType t)
      | Just cs' <- eq s t -> unifiable (cs `S.union` cs') ctx
    _ -> Just ("Unsatisfiable constraint " ++ show c)
  where
    Nothing `or` x = x
    x `or` Nothing = x
    (c,cs) = S.deleteFindMin ccs
    eq s t | s == t = Just S.empty
    eq (TFunction ret1 args1) (TFunction ret2 args2) = Just $
       (ret1,ret2) `S.insert`
       S.fromList (zipParams (,) args1 args2)
    eq _ _ = Nothing

zipParams f p1 p2 = zipWith f (unf p1) (unf p2)
  where
    unf :: FormalParams t -> [t]
    unf (VarargParam:_) = []
    unf (FormalParam t _:xs) = t : unf xs
    unf [] = []
mapParams f [] = []
mapParams f [VarargParam] = []
mapParams f (FormalParam t n:xs) = FormalParam (f t) n : mapParams f xs

-- TODO Replace all base types with a replacement base type.
up (CType t) = CType (upT t)
up t = t

upT t | baseT t = dummyBaseT
upT (TFunction ret args) = TFunction (up ret) (mapParams up args)
upT t = t

upConstr (Sub s t) = (up s, up t)
upConstr (Eq s t) = (up s, up t)

occursIn :: Name -> CType -> Bool
occursIn s t = case t of
  CTypeVar n | s == n -> True
  CTypeVar n -> False
  CType (TFunction t ts) -> any formal ts || s `occursIn` t
  CType t | baseT t -> False
  _ -> error ("occursIn: Unhandled type " ++ show t)
  where
    formal VarargParam = False
    formal (FormalParam t _) = s `occursIn` t

simplifyConstraints :: CCSet -> (CCSet, CCSet, Context CType)
simplifyConstraints cs = go cs S.empty M.empty
  where
  go cs failed subs
    | failed' == failed || S.null failed' = (cs',failed',subs')
    | otherwise = go (S.union cs failed') failed' subs'
    where
      (cs',failed',subs') = simplifyConstraints' cs failed subs

simplifyConstraints' :: CCSet -> CCSet -> Context CType -> (CCSet, CCSet, Context CType)
simplifyConstraints' cs failed subs | S.null cs = (cs, failed, subs)
simplifyConstraints' ccs failed subs = simplifyConstraints' cs' (S.union failed' failed) (M.union subs' subs)
  where
    (c,cs) = S.deleteFindMin ccs
    (failed', subs') = f c
    cs' = subst subs' cs

    fail = (S.singleton c, M.empty)
    succeed subs = (S.empty, subs)

    f (Eq s t) = succeed (mgu s t)
    f (Sub s t)
        | base s && base t && s `subBase` t = succeed M.empty
    f _ = fail

mgu :: CType -> CType -> Context CType
mgu (CType s) (CType t) = mgu' s t
mgu (CTypeVar s) t = M.fromList [(s,t)]
mgu s (CTypeVar t) = M.fromList [(t,s)]
mgu' s t | s == t = M.empty
mgu' (TFunction ret1 args1) (TFunction ret2 args2) = M.unions $
    [mgu ret1 ret2] ++ zipParams mgu args1 args2
mgu' s t = error ("mgu': Tried to unify " ++ show s ++ " and " ++ show t)

class Substitutable a where
  subst :: Context CType -> a -> a

instance Substitutable a => Substitutable (Constraint a) where
  subst cs (Sub s t) = Sub (subst cs s) (subst cs t)
  subst cs (Eq s t) = Eq (subst cs s) (subst cs t)
instance (Ord t, Substitutable t) => Substitutable (Set t) where
  subst cs = S.map (subst cs)
instance Substitutable CType where
  subst cs s = case s of
    (CTypeVar n) -> fromMaybe s (M.lookup n cs)
    (CType t)    -> CType (subst cs t)
instance (Show t, Substitutable t) => Substitutable (Type t) where
  subst cs x = case x of
    TFunction ret args -> TFunction (subst cs ret) (mapParams (subst cs) args)
    TInt -> TInt
    TChar -> TChar
    _ -> error ("subst: Unhandled type " ++ show x)
