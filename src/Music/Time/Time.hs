
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
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
        -- * The 'Time' and 'Duration' type functions
        Time(..),
        Duration(..),

        -- $converting

        -- * The 'Event' type functions
        Event(..),

        -- * Basic time and duration types
        DurationT(..),
        TimeT(..),
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point

-- $converting
--
-- Note that you should use '.-.' and '.+^' to convert between time and
-- duration. To refer to time zero (the beginning of the music), use
-- 'origin'.
--


-- |
-- This type function returns the duration type for a given type.
--
type family Duration (s :: *) :: *

-- |
-- This type function returns the duration type for a given type.
--
type Time a = Point (Duration a)

-- |
-- This type function returns the value type for a given type.
--
type family Event (s :: *) :: *

-- |
-- This type represents relative time in seconds.
--
type DurationT = Rational

-- |
-- This type represents absolute time in seconds since the start time. Note
-- that time can be negative, representing events occuring before the start time.
-- The start time is usually the the beginning of the musical performance.
--
-- Time forms an affine space with durations as the underlying vector space,
-- that is, we can add a time to a duration to get a new time using '.+^',
-- take the difference of two times to get a duration using '.-.'.
--
type TimeT = Point DurationT

