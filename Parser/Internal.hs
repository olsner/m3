{-# LANGUAGE RankNTypes #-}

module Parser.Internal
  (Parser()
  ,runParser

  ,failParse
  ,commit

  ,next
  ,look
  ,eof

  ,getState
  ,putState
  ,modifyState
  ,withState
  ) where

import Control.Applicative

-- The three continuations: error (hard), retry (backtracking error), success.

-- The error continuation takes a list of tokens that were not consumed by the
-- parser. Since retries can become hard failures, the retry continuation must
-- be given as much information as the hard error continuation.
-- The success continuation simply takes the "value" (in the functor/
-- /applicative/monad) of the computation along with the updated state and
-- rest-of-stream values.

type Error t r = [t] -> String -> r
type Retry t r = Error t r
type Success s t a r = a -> s -> [t] -> r

-- | The main parser type. This takes three arguments: the state, the token
-- type and the result type.
newtype Parser s t a = P (forall r . s -> [t] -> Success s t a r -> Retry t r -> Error t r -> r)

-- | Run a parser. Takes a state and a list of tokens and returns the remaining
-- tokens along with either an error message or a tuple of state and value
-- result.
{-# INLINE runParser #-}
runParser :: Parser s t a -> s -> [t] -> (Either String (a,s), [t])
runParser (P p) s ts = p s ts suc err err
  where
    {-# INLINE suc #-}
    suc = \a s ts -> (Right (a,s),ts)
    {-# INLINE err #-}
    err = \ts msg -> (Left msg,ts)

instance Functor (Parser s t) where
  {-# INLINE fmap #-}
  fmap f (P pcf) = P $ \s ts suc -> pcf s ts (suc . f)
instance Applicative (Parser s t) where
  {-# INLINE pure #-}
  pure x = P $ \s ts suc _ _ -> suc x s ts
  {-# INLINE (<*>) #-}
  P ff <*> P fx = P $ \s ts suc retr err ->
    let
      suc' = \f s ts -> fx s ts (suc'' f) retr err 
      suc'' f = \x s ts -> suc (f x) s ts
    in ff s ts suc' retr err

instance Alternative (Parser s t) where
  {-# INLINE empty #-}
  -- Note: retry remembers its stream state - this allows a later soft-made-hard
  -- failure to provide the "rest of stream" to the parser user.
  empty = failParse "Alternative.empty"
  -- Run x with retry pointing to just running y from the current position.
  -- Error continuation remains the same (it will point outside of the parser)
  {-# INLINE (<|>) #-}
  P x <|> P y = P $ \s ts suc retr err ->
    x s ts suc (\_ _ -> y s ts suc retr err) err

instance Monad (Parser s t) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  P x >>= y = P $ \s ts suc retr err ->
    let
      suc' = \x s ts -> let P f = y x in f s ts suc retr err
    in x s ts suc' retr err

-- | Make parse failure in 'p' a fatal parser error rather than a backtracking
-- error.
{-# INLINE commit #-}
commit :: Parser s t a -> Parser s t a
commit (P p) = P $ \s ts suc _ err -> p s ts suc err err
-- i.e: simply replace the retry-continuation with the hard failure one.

{-# INLINE failParse #-}
-- | Cause a parse failure with the given message.
failParse :: String -> Parser s t a
failParse msg = P $ \_ ts _ retr _ -> retr ts msg

nextLook :: String -> ([t] -> [t]) -> Parser s t t
nextLook msg f = P $ \s ts suc retr _ -> case ts of
  [] -> retr ts (msg++": EOF")
  (t:_) -> suc t s (f ts)

{-# INLINE next #-}
-- | Return and consume the next token. Fail the parse if at end of stream.
next :: Parser s t t
next = nextLook "next" tail

-- | Return the next token but do not consume it. Fail the parse if at end of
-- stream.
look :: Parser s t t
look = nextLook "look" id

{-# INLINE eof #-}
-- | Match only if at end of stream.
eof :: Parser s t ()
eof = P $ \s ts suc retr _ -> case ts of
  [] -> suc () s ts
  _ -> retr ts ("eof: Not EOF")

-- | Return the current state.
getState :: Parser s t s
getState = P $ \s ts suc _ _ -> suc s s ts

-- | Replace the state entirely. Note: when backtracking past this call, the
-- state will be discarded and parsing will continue with the previous state.
putState :: s -> Parser s t s
putState s = P $ \_ ts suc _ _ -> suc s s ts

-- | Similar to (getState >>=).
withState :: (s -> Parser s t a) -> Parser s t a
withState f = P $ \s ts suc -> let P p = f s in p s ts suc

modifyState :: (s -> s) -> Parser s t ()
modifyState f = P $ \s ts suc _ _ -> suc () (f s) ts
