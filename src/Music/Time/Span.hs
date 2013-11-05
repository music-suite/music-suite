
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleContexts,
    ConstraintKinds,
    ViewPatterns,
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
-- Provides time spans.
--
-------------------------------------------------------------------------------------

module Music.Time.Span (
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
import Music.Time.Time
import Music.Time.Reverse
import Music.Time.Delayable
import Music.Time.Stretchable

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

instance Delayable Span where
    delay n = mapSpanRel $ curry $ delay n *** id

instance Stretchable Span where
    stretch n = mapSpanRel $ curry $ stretch n *** stretch n
        
instance Reversible Span where
    rev = inSpan g where g (t, d) = (mirror (t .+^ d), d)
    -- rev (getSpanAbs -> (x, y)) = mirror y `between` mirror x

instance Semigroup Span where
    -- Span (t1, d1) <> Span (t2, d2) = Span (t1 .+^ (d1 *^ (t2.-.origin)), d1*^d2)
    Span (t1, d1) <> Span (t2, d2) = Span (t1 `delayTime` (d1 `stretch` t2), d1 `stretch` d2)

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

-- TODO add "individual scaling" component, i.e. scale just duration not both time and duration
-- Useful for implementing separation in articulation etc



{-
type TFun a = Time -> a
type BoundTFun a = (Span, TFun a)
type LinTFun a = (Span, (a, a))
type Event x = (Span, x)
-}

                           