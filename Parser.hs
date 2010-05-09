{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser
  (Parser,
  runParser,
  onFail,
  commit,
  next,
  satisfy,
  eof,
  manyFinally,
  many1,
  exactly,
  sepBy,
  sepBy1,
  match,
  choice,
  guardMsg,
  satisfyLook,
  lookNext,
  failParse)
  where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad (unless)

import Debug.Trace

data Res a = Success a | Retry String | Error String

newtype Parser t a = P { unP :: [t] -> (Res a, [t]) }

infixr 9 .:
{-# INLINE (.:) #-}
(.:) = (.).(.)

{-# INLINE onP #-}
onP f = P . f .: unP

runParser :: Parser t a -> [t] -> (Either String a, [t])
runParser (P p) = first convert . p
  where
    convert (Success x) = Right x
    convert (Retry e) = Left e
    convert (Error e) = Left e

instance Functor Res where
  fmap f (Success x) = Success (f x)
  fmap _ (Retry e) = (Retry e)
  fmap _ (Error e) = (Error e)

instance Functor (Parser t) where
  {-# INLINE fmap #-}
  fmap f p = first (fmap f) `onP` p

instance Applicative (Parser t) where
  {-# INLINE pure #-}
  pure x = P $ \ts -> (Success x, ts)
  pf <*> (P pa) = continue `onP` pf
    where
      continue (Error e, ts) = (Error e, ts)
      continue (Retry e, ts) = (Retry e, ts)
      continue (Success f, ts) = case pa ts of
        (Success x, ts') -> (Success (f x), ts')
        (Retry e, ts') -> (Retry e, ts')
        (Error e, ts') -> (Error e, ts')

{-instance Monad (Parser t) where
  return = pure
  fail e = P $ \ts -> (Left e, ts)
  {-# INLINE (>>=) #-}
  (P p) >>= q = P $ \ts ->
    case p ts of
      (Right x, ts') -> unP (q x) ts'
      (Left e, ts') -> (Left e, ts')-}

instance Alternative (Parser t) where
  empty = failParse "Alternative.empty"
  {-# INLINE (<|>) #-}
  (P p) <|> (P q) = P $ \ts -> case p ts of (Retry _, _) -> q ts; r -> r

commit (P p) = P $ \ts -> case p ts of
  (Retry e, ts) -> trace "Retry caught in commit" (Error ("commit: "++e), ts)
  other -> other

failParse e = {-trace ("failParse: "++e) $-} P $ \ts -> (Retry e, ts)

onFail = (<|>)

guardMsg b msg = unless b (failParse msg)

{-# INLINE next #-}
next = P f
  where
    -- guardM (gets (not . null)) "Ran out of input (EOF)" >> (gets head <* modify tail)
    f [] = (Retry "Ran out of input (EOF)", [])
    f (t:ts) = (Success t, ts)

lookNext = P f
  where
    f [] = (Retry "Ran out of input (EOF)", [])
    f ts@(t:_) = (Success t, ts)

{-# INLINE satisfy #-}
satisfyLook msg p = P f
  where
    f [] = (Retry ("Parser.satisfy: expected "++msg++" instead of EOF"), [])
    f ts@(t:_)
      | p t  = (Success t, ts)
      | True = (Retry ("Parser.satisfy: expected "++msg++", found "++show t), ts)
satisfy msg p = satisfyLook msg p <* next

eof = P $ \ts -> (if null ts then Success () else Retry "Expected end-of-file", ts)

list p ps = (:) <$> p <*> ps

manyFinally p z = many p <* z
many1 p = list p (many p)

exactly 0 _ = pure []
exactly n p = list p (exactly (n - 1) p)

sepBy1 p s = list p (many (s *> p))
sepBy p s = sepBy1 p s <|> pure []

match (x:xs) = list (satisfy (show x) (== x)) (match xs)
match [] = return []

{-# INLINE choice #-}
choice :: [Parser t a] -> Parser t a
choice = foldr (<|>) (failParse "choice ran out of alternatives")
