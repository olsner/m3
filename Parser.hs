{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser
  (Parser,
  runParser,
  onFail,
  next,
  satisfy,
  token,
  eof,
  manyFinally,
  many1,
  exactly,
  sepBy,
  sepBy1,
  match,
  choice,
  guardMsg,
  guardM)
  where

import Control.Applicative
import Control.Arrow
import Control.Monad.Instances

import Control.Monad.State
import Control.Monad.Error

newtype Parser t a = P { unP :: [t] -> (Either String a, [t]) }

infixr 9 .:
{-# INLINE (.:) #-}
(.:) = (.).(.)

{-# INLINE onP #-}
onP f = P . f .: unP

runParser = first convert .: unP
  where
    convert r = case r of
      Right a -> a
      Left err -> error err

instance Functor (Parser t) where
  {-# INLINE fmap #-}
  fmap = onP . first . fmap

instance Applicative (Parser t) where
  {-# INLINE pure #-}
  pure x = P $ \ts -> (Right x, ts)
  pf <*> pa = continue `onP` pf
    where
      continue (Left e, ts) = (Left e, ts)
      continue (Right f, ts) = first (Right . f) $ runParser pa ts

instance Monad (Parser t) where
  return = pure
  fail x = P $ \ts -> (Left x, ts)
  {-# INLINE (>>=) #-}
  (P p) >>= q = P $ \ts ->
    case p ts of
      (Right x, ts') -> unP (q x) ts'
      (Left e, ts') -> (Left e, ts')

instance Alternative (Parser t) where
  empty = fail "Alternative.empty"
  {-# INLINE (<|>) #-}
  (P p) <|> (P q) = P $ \ts -> case p ts of (Left _, _) -> q ts; r -> r

onFail = (<|>)

guardMsg b msg = unless b (fail msg)
guardM b msg = b >>= flip unless (fail msg)

{-# INLINE next #-}
next = P f
  where
    -- guardM (gets (not . null)) "Ran out of input (EOF)" >> (gets head <* modify tail)
    f [] = (Left "Ran out of input (EOF)", [])
    f (t:ts) = (Right t, ts)

{-# INLINE satisfy #-}
satisfy p = next >>= \x -> guardMsg (p x) "Parse.satisfy: failed" >> return x

token t = satisfy (== t)

eof = P $ \ts -> (if null ts then Right () else Left "Expected end-of-file", ts)

list p ps = (:) <$> p <*> ps

manyFinally p z = many p <* z
many1 p = list p (many p)

exactly 0 p = pure []
exactly n p = list p (exactly (n - 1) p)

sepBy1 p s = list p (many (s *> p))
sepBy p s = sepBy1 p s <|> pure []

match (x:xs) = list (satisfy (== x)) (match xs)
match [] = return []

{-# INLINE choice #-}
choice :: [Parser t a] -> Parser t a
choice = foldr (<|>) (fail "choice ran out of alternatives")
