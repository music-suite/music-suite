
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

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
        -- $convert

        -- * Time type
        Time(..),

        -- * Duration type
        Duration,

        -- * Behavior class
        HasBehavior(..),

        -- ** Identities
        start,
        stop,
        unit,

        -- ** Combinators
        -- TODO
  ) where

import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Semigroup
import           Data.VectorSpace

import           Music.Score.Util       (showRatio)

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
    deriving (Eq, Ord, Num, Enum, Fractional, Real, RealFrac, AdditiveGroup)

instance Show Duration where
    show = showRatio . getDuration

instance VectorSpace Duration where
    type Scalar Duration = Duration
    (Duration x) *^ (Duration y) = Duration (x *^ y)

-- |
-- The unit duration.
--
unit :: Duration
unit = 1 -- TODO some overloaded unit value

-- |
-- This type represents absolute time in seconds since 'start'. Note that time can be
-- negative, representing events occuring before the start time.
--
-- Time forms an affine space with durations as the underlying vector space,
-- that is, we can add a time to a duration to get a new time using '.+^',
-- take the difference of two times to get a duration using '.-.'.
--
type Time = Point Duration

instance Num Time where
    (+)         = relative2 origin (+)
    (*)         = relative2 origin (*)
    negate      = mirror
    abs         = relative origin abs
    signum      = relative origin signum
    fromInteger = (origin .+^) . fromInteger
instance Fractional Time where
    recip        = relative origin recip
    fromRational = (origin .+^) . fromRational

-- |
-- The global start time, which usually means the the beginning of the musical performance.
--
-- This is a synonym for 'origin'.
--
start :: Time
start = origin

-- |
-- The global end time, defined as @start .+^ unit@.
--
stop :: Time
stop = origin .+^ unit


class HasBehavior f where
    (?) :: f a -> Time -> a

instance HasBehavior ((->) Time) where
    (?) = id

