module Data.AffineSpace.Point.Offsets
  ( offsetPoints,
    offsetPointsS,
    pointOffsets,
    offsetVs,
    distanceVs,
  )
where

-- offsetted,

import Data.AffineSpace
import Data.AffineSpace.Point
import Data.List
import Data.Stream.Infinite (Stream)
import qualified Data.Stream.Infinite as Stream
import Data.VectorSpace

-- | Lay out a series of vectors from a given point. Return all intermediate points.
--
-- > lenght xs + 1 == length (offsetPoints p xs)
--
-- >>> offsetPoints 0 [1,1,1] :: [Integer]
-- [0,1,2,3]
offsetPoints :: AffineSpace p => p -> [Diff p] -> [p]
offsetPoints = scanl (.+^)

-- | Same as 'offsetPoints' but for infinite streams.
offsetPointsS :: AffineSpace p => p -> Stream (Diff p) -> Stream p
offsetPointsS = Stream.scanl (.+^)

-- | Calculate the relative difference between vectors.
--
-- > lenght xs + 1 == length (offsetPoints p xs)
--
-- >>> offsetPoints 0 [1,1,1] :: [Integer]
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
offsetVs p = tail . offsetPoints p

distanceVs :: AffineSpace p => p -> [p] -> [Diff p]
distanceVs p = tail . pointOffsets p
{-
offsetted :: AffineSpace p => p -> Iso' [Diff p] [p]
offsetted = iso distanceVs offsetVs
-}
