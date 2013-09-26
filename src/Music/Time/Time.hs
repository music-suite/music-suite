
{-# LANGUAGE
    TypeFamilies,
    GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Time.Time (
        -- * Time and duration types
        Time(..),
        Duration(..),
        -- $convert
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point

-- $convert
--
-- Note that you should use '.-.' and '.+^' to convert between time and
-- duration. To refer to time zero (the beginning of the music), use
-- 'origin'.
--

-- |
-- This type represents relative time in seconds.
--
newtype Duration = Duration { getDuration :: Rational }
    deriving (Eq, Ord, Show, Num, Enum, Fractional, Real, RealFrac, AdditiveGroup)

instance VectorSpace Duration where
    type Scalar Duration = Duration
    (Duration x) *^ (Duration y) = Duration (x *^ y)


-- |
-- This type represents absolute time in seconds since the start time. Note
-- that time can be negative, representing events occuring before the start time.
-- The start time is usually the the beginning of the musical performance.
--
-- Time forms an affine space with durations as the underlying vector space,
-- that is, we can add a time to a duration to get a new time using '.+^',
-- take the difference of two times to get a duration using '.-.'.
--
type Time = Point Duration

