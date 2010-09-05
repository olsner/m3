{-# LANGUAGE FlexibleInstances,FlexibleContexts,UndecidableInstances,ScopedTypeVariables,PatternGuards #-}

module Types.Conv (implicitConversions) where

import Prelude hiding ((.),id)
import Control.Category

import qualified Data.Map as M
--import Data.Map (Map)
import qualified Data.Set as S
--import Data.Set (Set)

import Debug.Trace hiding (traceShow)

import CppToken
import AST -- for Types

instance Show (a -> b) where
  show _ = "<fun>"

type Conv = TypedE -> TypedE
-- Search: find paths from a to b, returning some kind of evidence of the path afterwards
-- A path from a to b to c should be able to reuse b->c information in the search from a to b (in the form of the evidence?)
newtype Search c a b = Search { runSearch :: a -> [(b,b -> a -> c)] }
type ConvF = Search (TypedE -> TypedE) Type Type

--traceShow msg x = x `seq` trace (msg++show x) x
--traceFun msg f = traceShow (msg++" OUT: ") . f . traceShow (msg++" IN: ")

traceShow _ x = x
traceFun _ f = f

concatMapMap f = traceFun "concatMapMap" (M.toList . M.fromList . concat . map f)

--runSearch :: Search c a b -> a -> [(b,b -> a -> c)]
--runSearch (Search f) = f

firstOf :: Show b => Ord b => [Search c a b] -> Search c a b
firstOf (Search f:xs) = Search $ \a ->
  case f a of [] -> runSearch (oneOf xs) a; xs -> xs
firstOf []     = Search (const [])

nothingOr :: ConvF -> ConvF
nothingOr (Search f) = Search $ \a ->
  case f a of [] -> [(a,(\_ _ e -> e))]; xs -> xs

-- Combine all possible conversions in this level. *But* don't allow duplicate
-- types...
oneOf :: Show b => Ord b => [Search c a b] -> Search c a b
oneOf xs = Search (snd . go xs)
  where
    go (Search f:fs) a = foldr g (go fs a) (f a)
    go [] _ = (S.empty, [])
    g x@(typ,_) (seen,rest) = case S.member typ seen of
      True -> error ("Duplicate possible type: "++show typ)
      False -> (S.insert typ seen, x:rest)
--      let xs = f a in
--      if any (flip S.member alreadySeen . fst) xs then error "Duplicate possible conversions..." else xs++go fs (S.union alreadySeen (S.fromList (map fst xs))) a

Search x <> Search y = Search $ \t ->
  flip concatMapMap (x t) $ \(t1,g) ->
    flip map (y t1) $ \(t2,h) ->
      -- g maps from 'from' (t) to 't1', h from 't1' to 't2' (to)
      (t2,(\to from -> h to t1 . g t1 from))

one = TypedE dummyLocation TInt (EInt 1)
zero = TypedE dummyLocation TInt (EInt 0)

retype to _ (TypedE loc _ e) = TypedE loc to e
cast to _ expr@(TypedE loc _ _) = TypedE loc to (ECast to expr)
boolToInt _ _ cond@(TypedE loc _ _) = TypedE loc TInt (EConditional cond one zero)
{-
toInt to TInt = id
toInt to TChar = cast to TChar
toInt to TBool = boolToInt to TBool
-}

toChar to from = case from of
  TChar -> id
  TInt -> cast TChar TInt
  TBool -> cast TChar TInt . boolToInt TInt TBool
  _ -> error ("toChar: to "++show to++" from "++show from)

intNotZero TBool TInt = TypedE dummyLocation TBool . EBinary (initialPos "<generated>",NotEqual) zero
intNotZero to TChar = intNotZero to TInt . cast TInt TChar
intNotZero a b = error ("intNotZero: to "++show a++" from "++show b)

ptrNotNull TBool from = TypedE dummyLocation TBool . EBinary (initialPos "<generated>",NotEqual) (TypedE dummyLocation from ENullPtr)
ptrNotNull a b = error ("ptrNotNull: to "++show a++" from "++show b)

arrToPtr to (TArray _ _) (TypedE _ _ (EDeref expr@(TypedE loc _ _))) = TypedE loc to (EArrToPtr expr)
arrToPtr to from _ = error ("arrToPtr: to "++show to++" from "++show from)

removeConst = Search $ \t -> case t of
  (TConst t') -> [(t',retype)]
  _ -> []
qualificationConv = Search $ traceFun "qualificationConv" $ \t -> case t of
  TPtr (TFunction _ _) -> []
  TPtr pointee -> [(TPtr (TConst pointee),retype),(t,retype)]
  (TFunction _ _) -> []
  _ -> []
integralPromo = Search $ traceFun "integralPromo" $ \t -> case t of
  TChar -> [(TInt,cast),(t,retype)]
  TBool -> [(TInt,boolToInt),(t,retype)]
  _ -> []
integralConv = Search $ \t -> case t of
  TInt -> convChar t
  TBool -> convChar t
  _ -> []
  where convChar t = [(TChar,toChar),(t,retype)]
pointerConv = Search $ \t -> case t of
  -- To avoid doubling types, don't convert void pointers to void pointers...
  TPtr (TConst TVoid) -> []
  TPtr TVoid -> []
  TPtr (TConst _) -> [(TPtr (TConst TVoid),cast),(t,retype)]
  TPtr _ -> [(TPtr TVoid,cast),(t,retype)]
  _ -> []
booleanConv :: ConvF
booleanConv = Search $ \t -> case t of
  (TPtr _) -> [(TBool,ptrNotNull)]
  TInt -> [(TBool,intNotZero)]
  TChar -> [(TBool,intNotZero)]
  _ -> []
arrayToPtr = Search $ \t -> case t of
  t@(TArray _ elem) -> [(t,const (const id)),(TPtr elem,arrToPtr)]
  _ -> []
funcToPtr = Search $ \t -> case t of
  (TFunction _ _) -> [(TPtr t,retype)]
  _ -> []
addConst = Search $ \t -> case t of
  TConst _ -> []
  TPtr t2 -> [(TPtr (TConst t2), retype), (t,retype)]
  _ -> [(TConst t, retype), (t,retype)]

implicitConversions :: Type -> Type -> Maybe Conv
implicitConversions to from
  | to == from = Just id
  | TNullPtr <- from = case to of
      TPtr _ -> Just (cast to from)
      TConst (TPtr _) -> Just (cast to from)
      _ -> error ("Null-pointer conversion to "++show to)
  | otherwise = fmap (\f -> f to from) $ lookup to $ runSearch implicitConversionSearch from

implicitConversionSearch :: ConvF
implicitConversionSearch = 
  nothingOr removeConst <>
  nothingOr qualificationConv <>
  nothingOr (oneOf
    [integralPromo
    ,integralConv
    ,pointerConv
    ,booleanConv
    ]) <>
  nothingOr (firstOf [arrayToPtr,funcToPtr]) <>
  -- TODO Should take the fix-point of addConst or something here...
  nothingOr addConst

