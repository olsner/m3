{-# LANGUAGE NoMonomorphismRestriction,GeneralizedNewtypeDeriving #-}

module Parser2
  (Parser,
  runParser,
  commit,
  commitAp,
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
  satisfyLook,
  lookNext,
  failParse,
  localState,
  getState,
  putState)
  where

import Control.Applicative
--import Control.Applicative.Backtrack hiding (commit)
--import qualified Control.Applicative.Backtrack as Backtrack
import Control.Applicative.Error
import Control.Applicative.Stream (StreamF,evalStreamF)
import qualified Control.Applicative.Stream as Stream
import Control.Applicative.State
import Control.Applicative.Trans
import Control.Arrow (first)
import Control.Monad.Identity
import Control.Monad.Error

import Debug.Trace

-- TODO: Maybe this could be refactored into separate distinct transformer steps
-- to make it clear what code is implementing which part of the behaviour.

-- * backtracking-error
-- * normal error (for committed parses, backtracking-error gets transformed into a normal error)
-- * stream-eater (basically the [t] -> (a, [t]) part, with a = normal-error-containing-backtracking-error-or-result)
-- * state

{-
instance Applicative Identity where
  pure = Identity
  (<*>) = ap

newtype ParserT s t f a = P { unP :: StreamF t (StateT s (ErrorF (ErrorF f))) a } deriving (Functor,Applicative,Alternative,Monad,AlternativeError,MonadError String)
runParserT :: Functor f => Monad f => ParserT s t f a -> s -> [t] -> f (Either String (a,s), [t])
runParserT (P p) s ts = fmap (\x -> (x,[])) $ runErrorF (runErrorF (runStateT (evalStreamF p ts) s) >>= either emptyError pure)
runParser p s ts = runIdentity (runParserT p s ts)

liftFatalError :: Functor f => Monad f => ErrorF f a -> ParserT s t f a
liftFatalError = liftError . liftAp
liftError :: Functor f => Monad f => ErrorF (ErrorF f) a -> ParserT s t f a
liftError = liftState . liftAp
liftState :: Monad f => StateT s (ErrorF (ErrorF f)) a -> ParserT s t f a
liftState = liftStream . liftAp
liftStream :: StreamF t (StateT s (ErrorF (ErrorF f))) a -> ParserT s t f a
liftStream = P

type Parser s t a = ParserT s t Identity a

data Res s a = Success s a | Retry String | Error String

-- | Convenience alias for the internal result type.
type PTRes s t a = (Res s a, [t])

-- | The main parser type. This takes three arguments: the state, the token
-- type and the result type.
-- The
newtype Parser s t a = P (s -> [t] -> PTRes s t a)

{-# INLINE onP #-}
-- | Internal helper function: take a transformation on the internal result
-- type and use it to transform a Parser type.
onP :: (PTRes s t a -> PTRes s t b) -> Parser s t a -> Parser s t b
onP f (P p) = P $ \s ts -> f (p s ts)

-- | Run a parser. Takes a state and a list of tokens and returns the remaining
-- tokens along with either an error message or a tuple of state and value
-- result.
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
-}

-- | Make parse failure in 'p' a fatal parser error rather than a backtracking
-- error.
commit :: Parser s t a -> Parser s t a
-- FIXME: should preserve the error from 'p'
commit p = catchError p (\e -> liftFatalError (emptyError ("failure in committed parser"++ e)))

-- | Slightly more intuitive interface for committing to parses - the first
-- parameter is something that (if matching) commits the parse, and the second
-- is something that must successfully parse if the first parser succeeds.
commitAp :: Parser s t (a -> b) -> Parser s t a -> Parser s t b
commitAp p q = p <*> commit q
-- could be called <*>! or something clever like that

-- | Cause a parse failure with the given message.
failParse :: String -> Parser s t a
failParse e = P (emptyError e)

{-# INLINE next #-}
-- | Return and consume the next token. Fail the parse if at end of stream.
next :: Parser s t t
next = liftStream (Stream.next)

-- | Return the next token but do not consume it. Fail the parse if at end of
-- stream.
lookNext :: Parser s t t
lookNext = liftStream (Stream.look)

{-# INLINE satisfyLookState #-}
-- | Check the next token and current state against a predicate, return the
-- token if the predicate returns True, fail the parse otherwise. Also fails if
-- end of stream is reached and does *not* consume the token.
satisfyLookState :: Show t => String -> (s -> t -> Bool) -> Parser s t t
satisfyLookState msg p = do
  t <- (next <|> emptyError ("satisfyLookState: expected "++msg++" instead of EOF"))
  b <- p <$> getState <*> pure t
  if b then pure t else emptyError ("Parser.satisfy: expected "++msg++", found "++show t)

{-# INLINE satisfyLook #-}
-- | Like satisfyLookState but the predicate does not look at the state.
satisfyLook :: Show t => String -> (t -> Bool) -> Parser s t t
satisfyLook msg p = satisfyLookState msg (const p)
{-# INLINE satisfy #-}
-- | Like satisfyLook but also consumes the token.
satisfy :: Show t => String -> (t -> Bool) -> Parser s t t
satisfy msg p = satisfyLook msg p <* next

-- | Match only if at end of stream.
eof :: Parser s t ()
eof = liftStream Stream.eof

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
localState fun p = getState >>= \oldState -> liftState (modify fun) *> p <* putState oldState

-- | Return the current state.
getState :: Parser s t s
getState = liftState get
-- | Replace the state entirely. Note: when backtracking past this call, the
-- state will be discarded and parsing will continue with the previous state.
putState :: s -> Parser s t ()
putState s = liftState (put s)

