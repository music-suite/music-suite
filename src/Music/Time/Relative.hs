
-- |
-- Utilities for working with time values.
--
-- TODO Move. Perhaps these could be added to vector-space-point?
--
module Music.Time.Relative where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point

-- | 
-- Apply a transformation relative to the given point.
-- 
relative :: AffineSpace p => p -> (Diff p -> Diff p) -> p -> p
relative p f = (p .+^) . f . (.-. p)

-- | 
-- Apply a transformation relative to the given point.
-- 
relative2 :: AffineSpace p => p -> (Diff p -> Diff p -> Diff p) -> p -> p -> p
relative2 p f x y = out $ f (in' x) (in' y)
    where
        in' = (.-. p)
        out = (p .+^)

-- |
-- Mirror a point around 'origin'.
--
mirror :: AdditiveGroup v => Point v -> Point v
mirror = relative origin negateV
