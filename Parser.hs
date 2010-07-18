{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser
  (Parser,
  runParser,
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

data Res s a = Success s a | Retry String | Error String

type PTRes s t a = (Res s a, [t])
newtype Parser s t a = P (s -> [t] -> PTRes s t a)

{-# INLINE onP #-}
onP :: (PTRes s t a -> PTRes s t b) -> Parser s t a -> Parser s t b
onP f (P p) = P $ \s ts -> f (p s ts)

runParser :: Parser s t a -> s -> [t] -> (Either String (s,a), [t])
runParser (P p) s = first convert . p s
  where
    convert (Success s x) = Right (s,x)
    convert (Retry e) = Left e
    convert (Error e) = Left e

instance Functor (Res s) where
  fmap f (Success s x) = Success s (f x)
  fmap _ (Retry e) = (Retry e)
  fmap _ (Error e) = (Error e)

instance Functor (Parser s t) where
  {-# INLINE fmap #-}
  fmap f p = first (fmap f) `onP` p

instance Applicative (Parser s t) where
  {-# INLINE pure #-}
  pure x = P $ \s ts -> (Success s x, ts)
  pf <*> (P pa) = continue `onP` pf
    where
      continue (Error e, ts) = (Error e, ts)
      continue (Retry e, ts) = (Retry e, ts)
      continue (Success s f, ts) = case pa s ts of
        (Success s x, ts') -> (Success s (f x), ts')
        (Retry e, ts') -> (Retry e, ts')
        (Error e, ts') -> (Error e, ts')

instance Alternative (Parser s t) where
  empty = failParse "Alternative.empty"
  {-# INLINE (<|>) #-}
  (P p) <|> (P q) = P $ \s ts -> case p s ts of (Retry _, _) -> q s ts; r -> r

commit (P p) = P $ \s ts -> case p s ts of
  (Retry e, ts) -> trace "Retry caught in commit" (Error ("commit: "++e), ts)
  other -> other

failParse e = {-trace ("failParse: "++e) $-} P $ \s ts -> (Retry e, ts)


guardMsg b msg = unless b (failParse msg)

{-# INLINE next #-}
next = P f
  where
    -- guardM (gets (not . null)) "Ran out of input (EOF)" >> (gets head <* modify tail)
    f _ [] = (Retry "Ran out of input (EOF)", [])
    f s (t:ts) = (Success s t, ts)

lookNext = P f
  where
    f _ [] = (Retry "Ran out of input (EOF)", [])
    f s ts@(t:_) = (Success s t, ts)

{-# INLINE satisfy #-}
satisfyLook msg p = P f
  where
    f _ [] = (Retry ("Parser.satisfy: expected "++msg++" instead of EOF"), [])
    f s ts@(t:_)
      | p t  = (Success s t, ts)
      | True = (Retry ("Parser.satisfy: expected "++msg++", found "++show t), ts)
satisfy msg p = satisfyLook msg p <* next

eof = P $ \s ts -> (if null ts then Success s () else Retry "Expected end-of-file", ts)

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
choice :: [Parser s t a] -> Parser s t a
choice = foldr (<|>) (failParse "choice ran out of alternatives")
