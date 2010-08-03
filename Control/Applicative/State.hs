module Control.Applicative.State (module Control.Monad.State) where

import Control.Applicative
import Control.Applicative.Error
import Control.Applicative.Trans

import Control.Arrow (first)
import Control.Monad (ap)
import Control.Monad.State hiding (withState)

instance Monad m => Applicative (StateT s m) where
  pure = return
  (<*>) = ap

instance (Monad m, Alternative m) => Alternative (StateT s m) where
  empty = StateT $ const empty
  StateT xs <|> StateT ys = StateT $ \s -> (xs s <|> ys s)

instance ApplicativeTrans (StateT s) where
  liftAp a = StateT $ (\s -> fmap (\x -> (x,s)) a)

instance (Monad m, AlternativeError m) => AlternativeError (StateT s m) where
  emptyError = StateT . const . emptyError

{-
newtype StateF s f a = StateF { unStateF :: f (s -> (a,s)) }
instance Functor f => Functor (StateF s f) where
  fmap f (StateF g) = StateF $ fmap (\inf s -> first f (inf s)) g
instance Applicative f => Applicative (StateF s f) where
  pure = StateF . pure . idState
  StateF x <*> StateF y = StateF (f <$> x <*> y)
    where
      f :: (s -> (a->b,s)) -> (s -> (a,s)) -> (s -> (b,s))
      f x y s = case x s of (f,s') -> case y s' of (x,s'') -> (f x, s'')
instance Alternative f => Alternative (StateF s f) where
  empty = StateF empty
  StateF x <|> StateF y = StateF $ x <|> y
runStateF :: Functor f => StateF s f a -> s -> f (a,s)
runStateF (StateF f) s = fmap ($ s) f
localState :: Functor f
           => (s -> s) -- ^ A function to apply to the state to create the new local state
           -> StateF s f a -- ^ The parser to run in the local state
           -> StateF s f a
localState fun (StateF f) = StateF $ fmap (\k s -> (fst . k . fun $ s,s)) f
get :: Applicative f => StateF s f s
get = StateF $ pure (\s -> (s,s))
gets :: Applicative f => (s -> a) -> StateF s f a
gets f = StateF $ pure (\s -> (f s, s))
modify :: Applicative f => (s -> s) -> StateF s f s
modify f = StateF (pure (\s -> ((),f s))) *> get
put :: Applicative f => s -> StateF s f s
put s = StateF $ pure (\_ -> (s,s))

instance Monad f => Monad (StateF s f) where
  return = StateF . return . idState
  -- x :: f (s -> (a,s)), k :: (s -> (a,s)), y :: (a -> StateF s f b)
  -- y x :: StateF s f b, unStateF (y x) :: f (s -> (b,s))
  -- \s -> 
  -- x >>= y_, y_ :: ((s -> (a,s)) -> f (s -> (b,s))) -> ...
  StateF x >>= y_ = StateF (x >>= fun y)
    where
      -- y :: a -> f (s -> (b,s))
      y x = unStateF (y_ x)
      fun :: (a -> StateF s f b) -> (s -> (a,s)) -> f (f (s -> (b,s)))
      fun y k = return $ \s -> case k s of
        -- unstateF (y x) :: f (s -> (b,s))
        (x,s') -> y x >>= \k -> k s'

instance ApplicativeTrans (StateF s) where
  liftAp = StateF . fmap idState

idState x = \s -> (x,s)
-}

