{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser
  (Parser,
  runParser,
  next,
  look,
  lookPosition,
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
  (<*!>),
  (*!>),
  (<*!),
  failParse,
  localState,
  getState,
  putState,
  withState,
  modifyState)
  where

import Control.Applicative
import Parser.Internal
import SourcePos

infixl 4 <*!>, *!>, <*!

-- FIXME Document prettily :)
-- These three are like <*>, *> and <* except the right-hand side is a committed
-- parse (backtracking will fail instead of backtrack)

p <*!> q = p <*> commit q
p  *!> q = p *> commit q
p <*!  q = p <* commit q

{-# INLINE satisfyLookState #-}
-- | Check the next token and current state against a predicate, return the
-- token if the predicate returns True, fail the parse otherwise. Also fails if
-- end of stream is reached and does *not* consume the token.
satisfyLookState :: Show t => String -> (s -> t -> Parser s t a) -> Parser s t (Pos,a)
satisfyLookState msg p = do
  (pos,tok) <- look <|> failParse ("found EOF but expected "++msg)
  s <- getState
  ((,) pos <$> p s tok) <|> failParse ("found "++show tok++" but expected "++msg)

{-# INLINE satisfyLook #-}
-- | Like satisfyLookState but the predicate does not look at the state.
satisfyLook :: Show t => String -> (t -> Bool) -> Parser s t (Pos,t)
satisfyLook msg p = satisfyLookState msg (\_ t -> if p t then pure t else empty)
{-# INLINE satisfy #-}
-- | Like satisfyLook but also consumes the token.
satisfy :: Show t => String -> (t -> Bool) -> Parser s t (Pos,t)
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
match :: Eq t => Show t => [t] -> Parser s t [(Pos,t)]
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
