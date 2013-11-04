
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
        Duration,
        -- $convert
        start,
        stop,
        unit,

        Span,
        inSpan,
        between,
        spanning,
        unitSpan,
        getSpanAbs,
        getSpanRel,
        mapSpanAbs,
        mapSpanRel,
        invertSpan,
  ) where

import Control.Arrow
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

-- |
-- The unit duration.
--
unit :: Duration
unit = 1

-- |
-- This type represents absolute time in seconds since 'start'. Note that time can be
-- negative, representing events occuring before the start time.
--
-- Time forms an affine space with durations as the underlying vector space,
-- that is, we can add a time to a duration to get a new time using '.+^',
-- take the difference of two times to get a duration using '.-.'.
--
type Time = Point Duration

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


-- |
-- A 'Span' represents two points in time referred to as its onset and offset respectively.
-- Equivalently, 'Span' a time and a duration, referred to as its onset and duration.
-- 
-- A third way of looking at 'Span' is that it represents a time transformation where
-- onset is translation and duration is scaling.
--
-- Instances:
--
--   * Semigroup: @(t1,d1) <> (t2,d2) = (t1+d1*t2,d1*d2)@
--
--   * Monoid: @mempty = (0,1)@
--
newtype Span = Span (Time, Duration)
    deriving (Eq, Ord, Show)

instance Semigroup Span where
    Span (t1, d1) <> Span (t2, d2) = Span (t1 .+^ (d1 *^ (t2.-.origin)), d1*^d2)

instance Monoid Span where
    mempty = Span (origin, unit)
    mappend = (<>)

inSpan f = Span . f . getSpanRel 

-- |
-- The default span, i.e. 'between' 'start' 'stop'.
-- 
unitSpan :: Span
unitSpan = mempty

-- | @between t u@ represents the span between @t@ and @u@.
between :: Time -> Time -> Span
between t u = t `spanning` (u .-. t)

-- | @spanning t d@ represents the span between @t@ and @t .+^ d@.
spanning :: Time -> Duration -> Span
spanning = curry Span

-- | Render a span as a time and duration.
getSpanRel :: Span -> (Time, Duration)
getSpanRel (Span x)     = x

-- | Render a span as a time pair.
getSpanAbs :: Span -> (Time, Time)
getSpanAbs = second (start .+^) . getSpanRel

-- | Map over the span as a time and duration.
mapSpanRel :: (Time -> Duration -> (Time, Duration)) -> Span -> Span
mapSpanRel f = inSpan (uncurry f)

-- | Map over the span as a time pair.
mapSpanAbs :: (Time -> Time -> (Time, Time)) -> Span -> Span
mapSpanAbs f = uncurry between . uncurry f . getSpanAbs

invertSpan :: Span -> Span
invertSpan (Span (t,d)) = Span (mirror t, recip d)

revSpan = inSpan g
    where
        g (t, d) = (mirror (t .+^ d), d)

-- TODO add "individual scaling" component, i.e. scale just duration not both time and duration
-- Useful for implementing separation in articulation etc



{-
type TFun a = Time -> a
type BoundTFun a = (Span, TFun a)
type LinTFun a = (Span, (a, a))
type Event x = (Span, x)
-}



