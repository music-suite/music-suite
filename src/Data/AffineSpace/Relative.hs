
-- |
-- Utilities for working with time values.
--
-- TODO Move. Perhaps these could be added to vector-space-point?
--
module Data.AffineSpace.Relative (
        relative,
        relative2,
        relative3,
        reflectThrough
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace

-- | 
-- Apply a transformation relative to the given point.
-- 
relative :: AffineSpace p => p -> (Diff p -> Diff p) -> p -> p
relative p f = (p .+^) . f . (.-. p)

-- | 
-- Apply a transformation relative to the given point.
-- 
relative2 :: AffineSpace p => p -> (Diff p -> Diff p -> Diff p) -> p -> p -> p
relative2 p f x y = proj $ f (inj x) (inj y)
    where
        inj = (.-. p)
        proj = (p .+^)

-- | 
-- Apply a transformation relative to the given point.
-- 
relative3 :: AffineSpace p => p -> (Diff p -> Diff p -> Diff p -> Diff p) -> p -> p -> p -> p
relative3 p f x y z = proj $ f (inj x) (inj y) (inj z)
    where
        inj = (.-. p)
        proj = (p .+^)


-- |
-- Mirror a point through a given point.
--
reflectThrough :: AffineSpace p => p -> p -> p
reflectThrough o = relative o negateV

