module Parser.Internal
  (Parser,
  runParser,
  commit,
  failParse,
  next,
  look,
  eof,
  getState,
  putState,
  withState,
  satisfyLookState
  )
  where

import Control.Applicative
import Control.Arrow (first)

import Debug.Trace

data Res a = Success a | Retry String | Error String

-- | Convenience alias for the internal result type.
type PTRes s t a = (Res (a,s), [t])

-- | The main parser type. This takes three arguments: the state, the token
-- type and the result type.
newtype Parser s t a = P (s -> [t] -> PTRes s t a)

{-# INLINE onP #-}
-- | Internal helper function: take a transformation on the internal result
-- type and use it to transform a Parser type.
onP :: (PTRes s t a -> PTRes s t b) -> Parser s t a -> Parser s t b
onP f (P p) = P $ \s ts -> f (p s ts)

-- | Run a parser. Takes a state and a list of tokens and returns the remaining
-- tokens along with either an error message or a tuple of state and value
-- result.
runParser :: Parser s t a -> s -> [t] -> (Either String (a,s), [t])
runParser (P p) s = first convert . p s
  where
    convert (Success x) = Right x
    convert (Retry e) = Left e
    convert (Error e) = Left e

instance Functor Res where
  fmap f (Success x) = Success (f x)
  fmap _ (Retry e) = Retry e
  fmap _ (Error e) = Error e
instance Applicative Res where
  pure = Success
  Success f <*> Success x = Success (f x)
  Success _ <*> Retry msg = Retry msg
  Success _ <*> Error msg = Error msg
  Retry msg <*> _ = Retry msg
  Error msg <*> _ = Error msg

instance Functor (Parser s t) where
  {-# INLINE fmap #-}
  fmap f p = first (fmap (first f)) `onP` p

instance Applicative (Parser s t) where
  {-# INLINE pure #-}
  pure x = P $ \s ts -> (Success (x,s), ts)
  pf <*> (P pa) = continue `onP` pf
    where
      continue (Error e, ts) = (Error e, ts)
      continue (Retry e, ts) = (Retry e, ts)
      continue (Success (f,s), ts) = first (fmap (first f)) $ pa s ts 

instance Alternative (Parser s t) where
  empty = failParse "Alternative.empty"
  {-# INLINE (<|>) #-}
  (P p) <|> (P q) = P $ \s ts -> case p s ts of (Retry _, _) -> q s ts; r -> r

instance Monad (Parser s t) where
  return = pure
  P x >>= y = P $ \s ts -> let (res,ts') = x s ts in case res of
    Success (x,s') -> let P p = y x in p s' ts'
    Retry msg -> (Retry msg,ts')
    Error msg -> (Error msg,ts')

-- | Make parse failure in 'p' a fatal parser error rather than a backtracking
-- error.
commit :: Parser s t a -> Parser s t a
commit (P p) = P $ \s ts -> case p s ts of
  (Retry e, ts) -> trace "Retry caught in commit" (Error ("commit: "++e), ts)
  other -> other

-- | Cause a parse failure with the given message.
failParse :: String -> Parser s t a
failParse e = {-trace ("failParse: "++e) $-} P $ \_ ts -> (Retry e, ts)

{-# INLINE next #-}
-- | Return and consume the next token. Fail the parse if at end of stream.
next :: Parser s t t
next = P f
  where
    -- guardM (gets (not . null)) "Ran out of input (EOF)" >> (gets head <* modify tail)
    f _ [] = (Retry "Ran out of input (EOF)", [])
    f s (t:ts) = (Success (t,s), ts)

-- | Return the next token but do not consume it. Fail the parse if at end of
-- stream.
look :: Parser s t t
look = P f
  where
    f _ [] = (Retry "Ran out of input (EOF)", [])
    f s ts@(t:_) = (Success (t,s), ts)

-- | Match only if at end of stream.
eof :: Parser s t ()
eof = P $ \s ts -> (if null ts then Success ((),s) else Retry "Expected end-of-file", ts)

{-# INLINE satisfyLookState #-}
satisfyLookState :: Show t => String -> (s -> t -> Parser s t a) -> Parser s t a
satisfyLookState msg p = (look <|> failParse ("Parser.satisfyLookState: expected "++msg++" instead of EOF")) >>= \t -> getState >>= \s -> (p s t <|> failParse ("Parser.satisfyLookState: expected "++msg++" but found "++show t))

-- | Return the current state.
getState :: Parser s t s
getState = P $ \s ts -> (Success (s,s), ts)
-- | Replace the state entirely. Note: when backtracking past this call, the
-- state will be discarded and parsing will continue with the previous state.
putState :: s -> Parser s t s
putState s = P $ \_ ts -> (Success (s,s), ts)

-- withState pure == getState
withState :: (s -> Parser s t a) -> Parser s t a
withState f = P $ \s ts -> let P p = f s in p s ts
