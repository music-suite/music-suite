
{-# LANGUAGE DerivingStrategies, DerivingVia, FlexibleInstances #-}

module Control.Monad.Abort where

import Iso.Deriving
import Control.Monad.State
import Control.Monad.Except
import Data.Bifunctor

-- |
-- Abort is like 'State' but allow short-circuiting the computation.
--
-- @
-- t :: Abort Int ()
-- t = do
--   !x <- get
--   when (x > 10) abort
--   put $ x + 1
--   t
-- @
--
data Abort s a = Abort { runAbort :: s -> (Maybe a, s) }
  deriving (Functor)
  deriving (Applicative, Monad, MonadState s) via
    (ExceptT () (State s) `As1` Abort s)

-- | Abort the computation. The current state will be retained, but no
-- result will be returned.
abort :: Abort s a
abort = Abort $ \s -> (Nothing, s)

quit :: a -> Abort s a
quit x = Abort $ \s -> (Just x, s)

instance Inject (ExceptT () (State s) a) (Abort s a) where
  inj (ExceptT f) = Abort $ \s -> first eitherToMaybe $ runState f s

instance Project (ExceptT () (State s) a) (Abort s a) where
  prj (Abort f) = ExceptT $ StateT $ fmap (pure . first maybeToEither) f

instance Isomorphic (ExceptT () (State s) a) (Abort s a)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

maybeToEither :: Maybe a -> Either () a
maybeToEither Nothing = Left ()
maybeToEither (Just x) = Right x

