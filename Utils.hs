module Utils where

import Control.Applicative
import Control.Monad.State
import System.IO

maybeM :: Applicative m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f (Just x) = Just <$> f x
maybeM _ Nothing = pure Nothing

traceIO str = liftIO (hPutStrLn stderr str)
