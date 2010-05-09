{-# LANGUAGE FlexibleInstances,FlexibleContexts,UndecidableInstances,ScopedTypeVariables #-}

module Types.Conv (implicitConversions) where

import Prelude hiding ((.),id)
import Control.Category

import Data.Maybe
import Data.Monoid

import qualified Data.Map as M
--import Data.Map (Map)
import qualified Data.Set as S
--import Data.Set (Set)

import Text.ParserCombinators.Parsec.Pos

import CppToken
import AST -- for Types

class ShowType a where
  showType :: a -> String
instance ShowType TypedE where
  showType _ = "TypedE"
instance ShowType Type where
  showType _ = "Type"
instance (ShowType a, ShowType b) => ShowType (a -> b) where
  showType f = let x = undefined :: a; y = f x :: b in showType x ++ " -> " ++ showType y
instance (ShowType (a -> b)) => Show (a -> b) where
  show f = '<' : showType f ++ ">"

type Conv = TypedE -> TypedE
-- | Maybe monoid returning zero or one non-Nothing value, erroring instead of
-- appending two values.
newtype One a = One { getOne :: Maybe a }
instance Monoid (One a) where
  mempty = One Nothing
  mappend x y = case getOne x of
    Nothing -> y
    Just _ -> case getOne y of
      Nothing -> x
      Just _ -> error "Two Just merged by One"
-- Search: find paths from a to b, returning some kind of evidence of the path afterwards
-- A path from a to b to c should be able to reuse b->c information in the search from a to b (in the form of the evidence?)
newtype Search c a b = Search { runSearch :: a -> b -> One c }
instance Monoid (Search c a b) where
  mempty = Search mempty
  mappend (Search x) (Search y) = Search (mappend x y)
type ConvF = Search (TypedE -> TypedE) Type Type

concatMapMap f = M.toList . M.fromList . concat . map f

firstOf :: Show b => Ord b => [Search c a b] -> Search c a b
firstOf (Search f:xs) = Search $ \from to ->
  case getOne (f from to) of Nothing -> runSearch (oneOf xs) from to; just -> One just
firstOf []     = mempty

-- Combine all possible conversions in this level. *But* don't allow duplicate
-- types...
oneOf :: [Search c a b] -> Search c a b
oneOf = mconcat
{-
oneOf :: Show b => Ord b => [Search c a b] -> Search c a b
oneOf xs = Search (snd . go xs)
  where
    go (Search f:fs) a = foldr g (go fs a) (f a)
    go [] _ = (S.empty, [])
    g x@(typ,_) (seen,rest) = case S.member typ seen of
      True -> error ("Duplicate possible type: "++show typ)
      False -> (S.insert typ seen, x:rest)-}
--      let xs = f a in
--      if any (flip S.member alreadySeen . fst) xs then error "Duplicate possible conversions..." else xs++go fs (S.union alreadySeen (S.fromList (map fst xs))) a

nothingOr :: ConvF -> ConvF
nothingOr (Search f) = \from to -> case f from to of
  One Nothing -> One (Just id)
  res -> res

one = TypedE TInt (EInt 1)
zero = TypedE TInt (EInt 0)

retype to _ (TypedE _ e) = TypedE to e
cast to _ expr = TypedE to (ECast to expr)
boolToInt _ _ cond = TypedE TInt (EConditional cond one zero)
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

intNotZero TBool TInt = TypedE TBool . EBinary (initialPos "<generated>",NotEqual) zero
intNotZero to TChar = intNotZero to TInt . cast TInt TChar
intNotZero a b = error ("intNotZero: to "++show a++" from "++show b)

ptrNotNull TBool from = TypedE TBool . EBinary (initialPos "<generated>",NotEqual) (TypedE from ENullPtr)
ptrNotNull a b = error ("ptrNotNull: to "++show a++" from "++show b)

arrToPtr to from expr = TypedE to (EArrToPtr expr)

qualificationConv = Search $ \t -> case t of
  TPtr (TFunction _ _) -> mempty
  TPtr pointee -> [(TPtr (TConst pointee),retype)]
  (TFunction _ _) -> []
  t -> [(TConst t,retype)]
integralPromo = Search $ \t -> case t of
  TChar -> [(TInt,cast)]
  TBool -> [(TInt,boolToInt)]
  _ -> []
integralConv = Search $ \t -> case t of
  TInt -> convChar
  TBool -> convChar
  _ -> []
  where convChar = [(TChar,toChar)]
pointerConv = Search $ \t -> case t of
  TNullPtr -> []
  TPtr (TConst _) -> [(TPtr (TConst TVoid),cast)]
  TPtr _ -> [(TPtr TVoid,cast)]
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

Search y <> Search x = Search $ \t ->
  flip concatMapMap (x t) $ \(t1,g) ->
    flip map (y t1) $ \(t2,h) ->
      -- g maps from 'from' (t) to 't1', h from 't1' to 't2' (to)
      (t2,(\to from -> h to t1 . g t1 from))

implicitConversions :: Type -> Type -> Maybe Conv
--implicitConversions to from = fmap (\f -> f to from) $ lookup to $ runSearch implicitConversionSearch from
implicitConversions to from = runSearch implicitConversionSearch from to
implicitConversionSearch :: ConvF
implicitConversionSearch = 
  nothingOr qualificationConv <>
  nothingOr (oneOf
    [integralPromo
    ,integralConv
    ,pointerConv
    ,booleanConv
    ]) <>
  nothingOr (firstOf [arrayToPtr,funcToPtr])

