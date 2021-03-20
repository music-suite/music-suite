{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Log (MonadLog (..)) where

-- | A weaker form of 'MonadWriter' which also supports imperative logging.
class Monad m => MonadLog w m | m -> w where
  logger :: (a, w) -> m a
  logger ~(a, w) = do
    say w
    return a

  say :: w -> m ()
  say w = logger ((), w)
