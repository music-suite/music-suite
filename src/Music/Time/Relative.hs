
module Music.Time.Relative where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point

-- | 
-- Apply a transformation around the given point.
-- 
relative :: AffineSpace p => p -> (Diff p -> Diff p) -> p -> p
relative p f = (p .+^) . f . (.-. p)
-- TODO Move. Perhaps this could be added to vector-space?
