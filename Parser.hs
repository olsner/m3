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
import Control.Arrow

import Control.Monad.State

import Debug.Trace

newtype Parser t a = P { unP :: [t] -> (Either String a, [t]) }

infixr 9 .:
{-# INLINE (.:) #-}
(.:) = (.).(.)

{-# INLINE onP #-}
onP f = P . f .: unP

runParser = unP
  where
    convert r = case r of
      Right a -> a
      Left err -> error err

instance Functor (Parser t) where
  {-# INLINE fmap #-}
  fmap f p = first (fmap f) `onP` p

instance Applicative (Parser t) where
  {-# INLINE pure #-}
  pure x = P $ \ts -> (Right x, ts)
  pf <*> (P pa) = continue `onP` pf
    where
      continue (Left e, ts) = (Left e, ts)
      continue (Right f, ts) = case pa ts of
        (Right x, ts') -> (Right (f x), ts')
        (Left e, ts') -> (Left e, ts')
      -- first (Right . f) $ runParser pa ts

commit (P p) = P $ \ts -> case p ts of
  (Left e, ts) -> trace ("commit: "++e) (error e, ts)
  (Right x, ts) -> (Right x, ts)

failParse e = P $ \ts -> (Left e, ts)

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
  (P p) <|> (P q) = P $ \ts -> case p ts of (Left _, _) -> q ts; r -> r

onFail = (<|>)

guardMsg b msg = unless b (failParse msg)

{-# INLINE next #-}
next = P f
  where
    -- guardM (gets (not . null)) "Ran out of input (EOF)" >> (gets head <* modify tail)
    f [] = (Left "Ran out of input (EOF)", [])
    f (t:ts) = (Right t, ts)

lookNext = P f
  where
    f [] = (Left "Ran out of input (EOF)", [])
    f ts@(t:_) = (Right t, ts)

{-# INLINE satisfy #-}
satisfyLook p = P f
  where
    f [] = (Left "Ran out of input (EOF)", [])
    f ts@(t:_) | p t = (Right t, ts)
    f ts = (Left "Parser.satisfy: failed", ts)
satisfy p = satisfyLook p <* next

eof = P $ \ts -> (if null ts then Right () else Left "Expected end-of-file", ts)

list p ps = (:) <$> p <*> ps

manyFinally p z = many p <* z
many1 p = list p (many p)

exactly 0 _ = pure []
exactly n p = list p (exactly (n - 1) p)

sepBy1 p s = list p (many (s *> p))
sepBy p s = sepBy1 p s <|> pure []

match (x:xs) = list (satisfy (== x)) (match xs)
match [] = return []

{-# INLINE choice #-}
choice :: [Parser t a] -> Parser t a
choice = foldr (<|>) (failParse "choice ran out of alternatives")
