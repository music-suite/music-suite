
module Data.AffineSpace.Point.Offsets (
      offsetPoints,
      pointOffsets,
      offsetVs,
      distanceVs,
      -- offsetted,
  ) where

import Data.AffineSpace
import Data.VectorSpace
import Data.List
import Data.AffineSpace.Point

-- | Lay out a series of vectors from a given point. Return all intermediate points.
--
-- > lenght xs + 1 == length (offsetPoints p xs)
--
-- >>> offsetPoints 0 [1,1,1] :: [Time]
-- [0,1,2,3]
offsetPoints :: AffineSpace p => p -> [Diff p] -> [p]
offsetPoints = scanl (.+^)

-- | Calculate the relative difference between vectors.
--
-- > lenght xs + 1 == length (offsetPoints p xs)
--
-- >>> offsetPoints 0 [1,1,1] :: [Time]
-- [0,1,2,3]
pointOffsets :: AffineSpace p => p -> [p] -> [Diff p]
pointOffsets or = (zeroV :) . snd . mapAccumL g or
  where
    g prev p = (p, p .-. prev)

-- How they should really have been defined

-- |
-- For all p
-- > offsetVs p . distanceVs p = id 
-- > distanceVs p . offsetVs p = id 
offsetVs :: AffineSpace p => p -> [Diff p] -> [p]
offsetVs = tail . offsetPoints

distanceVs :: AffineSpace p => p -> [p] -> [Diff p]
distanceVs = tail . pointOffsets

{-
offsetted :: AffineSpace p => p -> Iso' [Diff p] [p]
offsetted = iso distanceVs offsetVs
-}

