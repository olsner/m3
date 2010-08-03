{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser.Internal
  (Parser,
  runParser,
  commit,
  failParse,
  next,
  lookNext,
  eof,
  getState,
  putState,
  withState,
  satisfyLookState
  )
  where

import Control.Applicative
--import Control.Applicative.Backtrack hiding (commit)
--import qualified Control.Applicative.Backtrack as Backtrack
import Control.Applicative.Error
import Control.Applicative.Stream (StreamF,runStreamF)
import qualified Control.Applicative.Stream as Stream
import Control.Applicative.State
import Control.Applicative.Trans
import Control.Monad.Identity
import Control.Monad.Error

instance Applicative Identity where
  pure = Identity
  (<*>) = ap

newtype ParserT s t f a = P { unP :: StateT s (ErrorF (ErrorF (StreamF t f))) a } deriving (Functor,Applicative,Alternative,Monad,AlternativeError,MonadError String)
runParserT :: Functor f => Monad f => ParserT s t f a -> s -> [t] -> f (Either String (a,s), [t])
runParserT (P p) s ts = runStreamF (runErrorF (runErrorF (runStateT p s) >>= either emptyError pure)) ts
runParser p s ts = runIdentity (runParserT p s ts)

-- m0 a = StateT [t] m a ~~> [t] -> m (Either String (a,s), [t])
-- m1 a = ErrorT String m0 a ~~> m0 (Either String (a,s))
-- m2 a = StateT s m1 a ~~> s -> m1 (a,s)

liftState = P
liftFatalError = liftState . liftAp
liftError = liftFatalError . liftAp
liftStream = liftError . liftAp

type Parser s t a = ParserT s t Identity a

-- | Make parse failure in 'p' a fatal parser error rather than a backtracking
-- error.
commit :: Parser s t a -> Parser s t a
-- FIXME: should preserve the error from 'p'
commit p = catchError p (\e -> liftFatalError (emptyError ("failure in committed parser"++ e)))

-- | Cause a parse failure with the given message.
failParse :: String -> Parser s t a
failParse e = P (emptyError e)

{-# INLINE next #-}
-- | Return and consume the next token. Fail the parse if at end of stream.
next :: Parser s t t
next = maybe (failParse "Parser.next: EOF") pure =<< liftStream Stream.next

-- | Return the next token but do not consume it. Fail the parse if at end of
-- stream.
lookNext :: Parser s t t
lookNext = maybe (failParse "Parser.lookNext: EOF") pure =<< liftStream Stream.look

-- | Match only if at end of stream.
eof :: Parser s t ()
eof = liftStream Stream.eof >>= \b -> if b then pure () else failParse "Parser.eof: not EOF"

-- | Return the current state.
getState :: Parser s t s
getState = liftState get
-- | Replace the state entirely. Note: when backtracking past this call, the
-- state will be discarded and parsing will continue with the previous state.
putState :: s -> Parser s t ()
putState s = liftState (put s)

-- | withState f == get >>= f, but without using monad functions (beyond the
-- magic obviously involved in the definition of this function)
withState :: (s -> Parser s t a) -> Parser s t a
withState fun = liftState get >>= fun

{-# INLINE satisfyLookState #-}
-- | Check the next token and current state against a predicate, return the
-- token if the predicate returns True, fail the parse otherwise. Also fails if
-- end of stream is reached and does *not* consume the token.
satisfyLookState :: Show t => String -> (s -> t -> Bool) -> Parser s t t
satisfyLookState msg p = do
  t <- (next <|> emptyError ("satisfyLookState: expected "++msg++" instead of EOF"))
  b <- p <$> getState <*> pure t
  if b then pure t else emptyError ("Parser.satisfy: expected "++msg++", found "++show t)

{-
import Control.Applicative
import Control.Arrow (first)
import Debug.Trace

data Res s a = Success s a | Retry String | Error String

-- | Convenience alias for the internal result type.
type PTRes s t a = (Res s a, [t])

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
    convert (Success s x) = Right (x,s)
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
    f s (t:ts) = (Success s t, ts)

-- | Return the next token but do not consume it. Fail the parse if at end of
-- stream.
lookNext :: Parser s t t
lookNext = P f
  where
    f _ [] = (Retry "Ran out of input (EOF)", [])
    f s ts@(t:_) = (Success s t, ts)

-- | Match only if at end of stream.
eof :: Parser s t ()
eof = P $ \s ts -> (if null ts then Success s () else Retry "Expected end-of-file", ts)

{-# INLINE satisfyLookState #-}
-- | Check the next token and current state against a predicate, return the
-- token if the predicate returns True, fail the parse otherwise. Also fails if
-- end of stream is reached and does *not* consume the token.
satisfyLookState :: Show t => String -> (s -> t -> Bool) -> Parser s t t
satisfyLookState msg p = P f
  where
    f _ [] = (Retry ("Parser.satisfy: expected "++msg++" instead of EOF"), [])
    f s ts@(t:_)
      | p s t = (Success s t, ts)
      | True  = (Retry ("Parser.satisfy: expected "++msg++", found "++show t), ts)

{-
-- Attempt at alternative implementation using only combinations of primitives
satisfyLookState msg p = withState (f p msg) <*> lookNext
  -- f :: (s -> t -> Bool) -> String -> s -> t -> Parser s t t
  where
    -- D'oh, not quite right: should be \s -> <parser>, but we need something
    -- like \s t -> <parser> to be able to factor in the token type.
    f p msg = \s t -> if p s t then pure t else failParse ("Parser.satisfyLookNext: expected "++msg++", found "++show t)
-}

-- | Return the current state.
getState :: Parser s t s
getState = P $ \s ts -> (Success s s, ts)
-- | Replace the state entirely. Note: when backtracking past this call, the
-- state will be discarded and parsing will continue with the previous state.
putState :: s -> Parser s t s
putState s = P $ \_ ts -> (Success s s, ts)

-- withState pure == getState
withState :: (s -> Parser s t a) -> Parser s t a
withState f = P $ \s ts -> let P p = f s in p s ts
-}
