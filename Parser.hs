{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser
  (Parser,
  runParser,
  next,
  look,
  satisfy,
  satisfyLook,
  satisfyLookState,
  eof,
  manyFinally,
  many1,
  exactly,
  sepBy,
  sepBy1,
  match,
  choice,
  commit,
  commitAp,
  failParse,
  localState,
  getState,
  putState,
  withState,
  modifyState)
  where

import Control.Applicative
import Parser.Internal

-- | Slightly more intuitive interface for committing to parses - the first
-- parameter is something that (if matching) commits the parse, and the second
-- is something that must successfully parse if the first parser succeeds.
commitAp :: Parser s t (a -> b) -> Parser s t a -> Parser s t b
commitAp p q = p <*> commit q

{-# INLINE satisfyLook #-}
-- | Like satisfyLookState but the predicate does not look at the state.
satisfyLook :: Show t => String -> (t -> Bool) -> Parser s t t
satisfyLook msg p = satisfyLookState msg (\s t -> if p t then pure t else empty)
{-# INLINE satisfy #-}
-- | Like satisfyLook but also consumes the token.
satisfy :: Show t => String -> (t -> Bool) -> Parser s t t
satisfy msg p = satisfyLook msg p <* next

-- | Combine a head-parser and a tail-parser into a list parser. Equivalent to
-- @liftM2 (:)@.
list :: Parser s t a -> Parser s t [a] -> Parser s t [a]
list p ps = (:) <$> p <*> ps

-- | Match zero or more 'p' followed by one 'z', returning the result of 'p'.
manyFinally :: Parser s t a -> Parser s t b -> Parser s t [a]
manyFinally p z = many p <* z
-- | Match one or more of 'p'.
many1 :: Parser s t a -> Parser s t [a]
many1 p = list p (many p)

-- | Match exactly 'n' occurences of 'p'.
exactly :: Int -> Parser s t a -> Parser s t [a]
exactly 0 _ = pure []
exactly n p = list p (exactly (n - 1) p)

-- | Match one or more items separated by a separator.
sepBy1 :: Parser s t a -> Parser s t b -> Parser s t [a]
sepBy1 p s = list p (many (s *> p))
-- | Match zero or more items separate by a separator.
sepBy :: Parser s t a -> Parser s t b -> Parser s t [a]
sepBy p s = sepBy1 p s <|> pure []

-- | Match a list of tokens exactly using '(==)'.
match :: Eq t => Show t => [t] -> Parser s t [t]
match (x:xs) = list (satisfy (show x) (== x)) (match xs)
match [] = pure []

{-# INLINE choice #-}
-- | Match the first in a list of parsers that matches.
choice :: [Parser s t a] -> Parser s t a
choice = foldr (<|>) (failParse "choice ran out of alternatives")

-- State management interface

-- | Run a parser with a local state, restoring the previous state before
-- continuing.
localState :: (s -> s) -- ^ A function to apply to the state to create the new local state
           -> Parser s t a -- ^ The parser to run in the local state
           -> Parser s t a
localState fun p = withState (\s -> putState (fun s) *> p <* putState s)
