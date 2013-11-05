
{-# LANGUAGE
    StandaloneDeriving,
    DeriveFoldable,
    DeriveTraversable #-}

module Data.PairMonad where

import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Traversable

-- Equivalent to the Monad Writer instance.
instance Monoid o => Monad ((,) o) where
  return      = pure
  (o,a) >>= f = (o `mappend` o', a') where (o',a') = f a

deriving instance Foldable ((,) o)
deriving instance Traversable ((,) o)
