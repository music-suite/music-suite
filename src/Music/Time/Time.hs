
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
        start,

        Duration,
        unit,

        Span,
        newSpan,
        getSpan,
        mapSpan,
        
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Music.Time.Relative

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

unit :: Duration
unit = 1

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

start :: Time
start = origin


-- A span represents two points in time or, equivalently, a time and a duration.
-- 
-- Instances:
--
--   * Semigroup: @(t1,d1) <> (t2,d2) = (t1+d1*t2,d1*d2)@
--
--   * Monoid: @mempty = (0,1)@
--
newtype Span = Span (Time, Duration)
    deriving (Eq, Ord, Show)

newSpan = curry Span
getSpan (Span x)     = x
mapSpan f = inSpan (uncurry f)

invSpan (Span (t,d)) = Span (negateP t, recip d)
inSpan f             = Span . f . getSpan 

negateP = relative origin negateV

revSpan = inSpan g
    where
        g (t,d) = (mirrorP (t .+^ d), d)
        mirrorP = relative origin negateV

-- TODO add "individual scaling" component, i.e. scale just duration not both time and duration
-- Useful for implementing separation in articulation etc

instance Semigroup Span where
    Span (t1, d1) <> Span (t2, d2) = Span (t1 .+^ (d1 *^ (t2.-.origin)), d1*^d2)

instance Monoid Span where
    mappend = (<>)
    mempty = Span (origin, unit)


{-
type TFun a = Time -> a
type BoundTFun a = (Span, TFun a)
type LinTFun a = (Span, (a, a))
type Event x = (Span, x)
-}



