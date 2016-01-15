
{-# LANGUAGE
    CPP,
    StandaloneDeriving,
    DeriveFoldable,
    DeriveTraversable #-}

module Data.PairMonad where

import Control.Applicative

-- #ifdef __HAS_LENS__
import Control.Lens()
-- #endif __HAS_LENS__

import Data.Monoid
import Data.Foldable
import Data.Orphans ()
import Data.Traversable

-- Equivalent to the Monad Writer instance.
-- The following is provided by Data.Orphans
-- instance Monoid o => Monad ((,) o) where
--   return      = pure
--   (o,a) >>= f = (o `mappend` o', a') where (o',a') = f a

-- #ifndef __HAS_LENS__
-- deriving instance Foldable ((,) o)
-- deriving instance Traversable ((,) o)
-- #endif __HAS_LENS__
