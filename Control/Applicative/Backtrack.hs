
module Control.Applicative.Backtrack where

import Control.Applicative
import Control.Applicative.Error
import Control.Applicative.Trans

type BacktrackRes f a = Either (f String) (f a)
newtype BacktrackF f a = BacktrackF { unBacktrackF :: BacktrackRes f a }
runBacktrackF :: Functor f => BacktrackF f a -> f (Either String a)
runBacktrackF (BacktrackF f) = either (fmap Left) (fmap Right) f
instance Functor f => Functor (BacktrackF f) where
  fmap f (BacktrackF g) = BacktrackF (fmap (fmap f) g)
instance Applicative f => Applicative (BacktrackF f) where
  pure = BacktrackF . Right . pure
  BacktrackF x <*> BacktrackF y = BacktrackF $ case (x,y) of
    (Right f,Right x) -> Right (f <*> x)
    (Left err,_) -> Left err
    (Right _,Left err) -> Left err
instance Applicative f => Alternative (BacktrackF f) where
  empty = emptyError "Alternative.empty"
  BacktrackF x <|> BacktrackF y = BacktrackF $ case x of
    (Left _) -> y
    r -> r
instance Applicative f => AlternativeError (BacktrackF f) where
  emptyError = BacktrackF . Left . pure
instance ApplicativeTrans BacktrackF where
  liftAp = BacktrackF . Right

retry :: Applicative f => String -> BacktrackF f a
retry = BacktrackF . Left . pure
commit :: AlternativeError f => BacktrackF f a -> BacktrackF f a
commit f = f <|> BacktrackF (Right (emptyError "Backtracked into a commit"))
