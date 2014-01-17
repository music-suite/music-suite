
-- |
-- Utilities for working with time values.
--
-- TODO Move. Perhaps these could be added to vector-space-point?
--
module Data.AffineSpace.Point.Relative (
        mirror,
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.AffineSpace.Relative

-- |
-- Mirror a point around 'origin'.
--
mirror :: AdditiveGroup v => Point v -> Point v
mirror = reflectThrough origin
