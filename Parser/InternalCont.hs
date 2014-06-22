{-# LANGUAGE RankNTypes #-}

module Parser.InternalCont
  (Parser()
  ,Stream
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

type Stream t = [(Pos,t)]

-- Continuation-style version:
type Retry t r = Stream t -> String -> r
type Error t r = Stream t -> String -> r
type Success s t a r = a -> s -> Stream t -> r

newtype Parser s t a = P (forall r . s -> Stream t -> Success s t a r -> Retry t r -> Error t r -> r)

runParser (P p) s ts = p s ts suc err err
  where
    suc = \a s ts -> (Right (a,s),ts)
    err = \ts msg -> (Left msg,ts)

instance Functor (Parser s t) where
  fmap f (P pcf) = P $ \s ts suc -> pcf s ts (suc . f)
instance Applicative (Parser s t) where
  pure x = P $ \s ts suc _ _ -> suc x s ts
  P ff <*> P fx = P $ \s ts suc retr err ->
    let
      suc' = \f s ts -> fx s ts (suc'' f) retr err 
      suc'' f = \x s ts -> suc (f x) s ts
    in ff s ts suc' retr err

-- | Cause a parse failure with the given message.
failParse :: String -> Parser s t a
failParse msg = lookPosition >>= \pos -> P $ \_ ts _ retr _ ->
  retr ts (show pos ++": " ++ msg)

instance Alternative (Parser s t) where
  -- Note: retry remembers its stream state - this allows a later soft-made-hard
  -- failure to provide the "rest of stream" to the parser user.
  empty = failParse "Alternative.empty"
  -- Run x with retry pointing to just running y from the current position.
  -- Error continuation remains the same (it will point outside of the parser)
  P x <|> P y = P $ \s ts suc retr err ->
    x s ts suc (\_ _ -> y s ts suc retr err) err

instance Monad (Parser s t) where
  return = pure
  P x >>= y = P $ \s ts suc retr err ->
    let
      suc' = \x s ts -> let P f = y x in f s ts suc retr err
    in x s ts suc' retr err

nextLook :: String -> (Stream t -> Stream t) -> Parser s t (Pos, t)
nextLook msg f = P $ \s ts suc retr _ -> case ts of
  [] -> retr ts (msg++": EOF")
  (t:_) -> suc t s (f ts)

-- | Return and consume the next token. Fail the parse if at end of stream.
next :: Parser s t (Pos,t)
next = nextLook "next" tail

-- | Return the next token but do not consume it. Fail the parse if at end of
-- stream.
look :: Parser s t (Pos,t)
look = nextLook "look" id

lookPosition = (fst <$> look) <|> pure endOfFilePos

-- | Match only if at end of stream.
eof :: Show t => Parser s t ()
eof = P $ \s ts suc retr _ -> case ts of
  [] -> suc () s ts
  ((pos,t):_) -> retr ts (show pos ++": found "++show t++" but expected end of file")

-- | Make parse failure in 'p' a fatal parser error rather than a backtracking
-- error.
commit :: Parser s t a -> Parser s t a
commit (P p) = P $ \s ts suc _ err -> p s ts suc err err
-- i.e: simply replace the retry-continuation with the hard failure one.

-- | Return the current state.
getState :: Parser s t s
getState = P $ \s ts suc _ _ -> suc s s ts
-- | Replace the state entirely. Note: when backtracking past this call, the
-- state will be discarded and parsing will continue with the previous state.
putState :: s -> Parser s t ()
putState s = P $ \_ ts suc _ _ -> suc () s ts
-- | Similar to (getState >>=).
withState :: (s -> Parser s t a) -> Parser s t a
withState f = P $ \s ts suc -> let P p = f s in p s ts suc

modifyState :: (s -> s) -> Parser s t ()
modifyState f = P $ \s ts suc _ _ -> suc () (f s) ts
