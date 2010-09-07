{-# LANGUAGE RankNTypes #-}

module Parser.Internal
  (Parser()
  ,runParser

  ,failParse
  ,commit

  ,next
  ,look
  ,lookPosition
  ,eof

  ,getState
  ,putState
  ,modifyState
  ,withState
  ) where

import Control.Applicative
import SourcePos

-- The three continuations: error (hard), retry (backtracking error), success.

-- The error continuation takes a list of tokens that were not consumed by the
-- parser. Since retries can become hard failures, the retry continuation must
-- be given as much information as the hard error continuation.
-- The success continuation simply takes the "value" (in the functor/
-- /applicative/monad) of the computation along with the updated state and
-- rest-of-stream values.

type Stream t = [(Pos,t)]

type Error t r = Stream t -> String -> r
type Retry t r = Error t r
type Success s t a r = a -> S s t -> r

data S s t = S !s !(Stream t)

-- | The main parser type. This takes three arguments: the state, the token
-- type and the result type.
newtype Parser s t a = P (forall r . S s t -> Success s t a r -> Retry t r -> Error t r -> r)

-- | Run a parser. Takes a state and a list of tokens and returns the remaining
-- tokens along with either an error message or a tuple of state and value
-- result.
{-# INLINE runParser #-}
runParser :: Parser s t a -> s -> Stream t -> (Either String (a,s), Stream t)
runParser (P p) s ts = p (S s ts) suc err err
  where
    {-# INLINE suc #-}
    suc = \a (S s ts) -> (Right (a,s),ts)
    {-# INLINE err #-}
    err = \ts msg -> (Left msg,ts)

instance Functor (Parser s t) where
  {-# INLINE fmap #-}
  fmap f (P pcf) = P $ \s suc -> pcf s (suc . f)
instance Applicative (Parser s t) where
  {-# INLINE pure #-}
  pure x = P $ \s suc _ _ -> suc x s
  {-# INLINE (<*>) #-}
  P ff <*> P fx = P $ \s suc retr err ->
    let
      suc' = \f s -> fx s (suc'' f) retr err 
      suc'' f = \x s -> suc (f x) s
    in ff s suc' retr err

instance Alternative (Parser s t) where
  {-# INLINE empty #-}
  -- Note: retry remembers its stream state - this allows a later soft-made-hard
  -- failure to provide the "rest of stream" to the parser user.
  empty = failParse "Alternative.empty"
  -- Run x with retry pointing to just running y from the current position.
  -- Error continuation remains the same (it will point outside of the parser)
  {-# INLINE (<|>) #-}
  P x <|> P y = P $ \s suc retr err ->
    x s suc (\_ _ -> y s suc retr err) err

instance Monad (Parser s t) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  P x >>= y = P $ \s suc retr err ->
    let
      suc' = \x s -> let P f = y x in f s suc retr err
    in x s suc' retr err

-- | Make parse failure in 'p' a fatal parser error rather than a backtracking
-- error.
{-# INLINE commit #-}
commit :: Parser s t a -> Parser s t a
commit (P p) = P $ \s suc _ err -> p s suc err err
-- i.e: simply replace the retry-continuation with the hard failure one.

{-# INLINE failParse #-}
-- | Cause a parse failure with the given message.
failParse :: String -> Parser s t a
failParse msg = P $ \(S _ ts) _ retr _ -> retr ts msg -- TODO When S has position, add it to the message.

nextLook :: String -> (Stream t -> Stream t) -> Parser s t (Pos,t)
nextLook msg f = P $ \(S s ts) suc retr _ -> case ts of
  [] -> retr ts (msg++": EOF")
  (t:_) -> suc t (S s (f ts))

{-# INLINE next #-}
-- | Return and consume the next token. Fail the parse if at end of stream.
next :: Parser s t (Pos,t)
next = nextLook "next" tail

-- | Return the next token but do not consume it. Fail the parse if at end of
-- stream.
look :: Parser s t (Pos,t)
look = nextLook "look" id

lookPosition = (fst <$> look) <|> pure endOfFilePos

{-# INLINE eof #-}
-- | Match only if at end of stream.
eof :: Parser s t ()
eof = P $ \s@(S _ ts) suc retr _ -> case ts of
  [] -> suc () s
  _ -> retr ts ("eof: Not EOF")

-- | Return the current state.
getState :: Parser s t s
getState = P $ \ss@(S s _) suc _ _ -> suc s ss

-- | Replace the state entirely. Note: when backtracking past this call, the
-- state will be discarded and parsing will continue with the previous state.
putState :: s -> Parser s t s
putState s = P $ \(S _ ts) suc _ _ -> suc s (S s ts)

-- | Pretty much (getState >>=).
withState :: (s -> Parser s t a) -> Parser s t a
withState f = P $ \ss@(S s ts) suc -> let P p = f s in p ss suc

modifyState :: (s -> s) -> Parser s t ()
modifyState f = P $ \(S s ts) suc _ _ -> suc () (S (f s) ts)
