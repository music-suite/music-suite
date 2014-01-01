
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleContexts,
    ConstraintKinds,
    ViewPatterns,
    StandaloneDeriving,
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
        -- inSpan,

        -- ** Constructing spans
        (<->),
        (>->),
        era,
        
        -- ** Span as transformation
        sunit,
        sapp,
        sunder,
        sinvert,

        -- ** Deconstructing spans
        range,
        delta,
        mapRange,
        mapDelta,
  ) where

import Control.Arrow
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point

import Music.Time.Time
import Music.Time.Delayable
import Music.Time.Stretchable
import Music.Time.Onset
import Data.AffineSpace.Relative
import Music.Time.Reverse

-- |
-- A 'Span' represents two points in time @u@ and @v@ where @t <= u@ or, equivalently, 
-- a time @t@ and a duration @d@ where @d >= 0@.
-- 
-- A third way of looking at 'Span' is that it represents a time transformation where
-- onset is translation and duration is scaling.
--
newtype Span = Span (Time, Duration)
    deriving (Eq, Ord, Show)

normalizeSpan :: Span -> Span
normalizeSpan (Span (t,d))
    | d >= 0  =  t <-> (t .+^ d)
    | d <  0  =  (t .+^ d) <-> t

instance Delayable Span where
    delay n = mapDelta $ curry $ first (delay n)

instance Stretchable Span where
    stretch n = normalizeSpan . (mapDelta $ curry $ stretch n *** stretch n)

-- This instance would violate the first Reversible law
-- instance Reversible Span where
    -- rev = inSpan g where g (t, d) = (mirror (t .+^ d), d)

instance HasOnset Span where
    onset = fst . range

instance HasOffset Span where
    offset = snd . range

instance HasDuration Span where
    duration = snd . delta

instance Semigroup Span where
    -- Span (t1, d1) <> Span (t2, d2) = Span (t1 .+^ (d1 *^ (t2.-.origin)), d1*^d2)
    Span (t1, d1) <> Span (t2, d2) = normalizeSpan $ Span (t1 `delayTime` (d1 `stretch` t2), d1 `stretch` d2)

instance Monoid Span where
    mempty  = start <-> stop
    mappend = (<>)

inSpan f = Span . f . delta 

-- |
-- The default span, i.e. 'start' '<->' 'stop'.
-- 
sunit :: Span
sunit = mempty

-- | @t \<-\> u@ represents the span between @t@ and @u@.
(<->) :: Time -> Time -> Span
(<->) t u = t >-> (u .-. t)

-- | @t >-> d@ represents the span between @t@ and @t .+^ d@.
(>->) :: Time -> Duration -> Span
t >-> d
    | d > 0     = Span (t,d)
    | otherwise = error "Invalid span"

-- | Get the era (onset to offset) of a given value.
era :: (HasOnset a, HasOffset a) => a -> Span
era x = onset x <-> offset x

-- | 
-- Deconstruct a span as a time and duration.
--
-- Typically used with the @ViewPatterns@ extension, as in
--
-- > foo (delta -> (t,d)) = ...
--
delta :: Span -> (Time, Duration)
delta (Span x) = x

-- | 
-- Deconstruct a span as onset and offset.
--
-- Typically used with the @ViewPatterns@ extension, as in
--
-- > foo (range -> (u,v)) = ...
--
range :: Span -> (Time, Time)
range x = let (t, d) = delta x
    in (t, t .+^ d)

-- | Map over the span as onset and duration.
mapDelta :: (Time -> Duration -> (Time, Duration)) -> Span -> Span
mapDelta f = inSpan (uncurry f)

-- | Map over the span as a time pair.
mapRange :: (Time -> Time -> (Time, Time)) -> Span -> Span
mapRange f = uncurry (<->) . uncurry f . range

-- | Apply a span transformation.
sapp :: (Delayable a, Stretchable a) => Span -> a -> a
sapp (delta -> (t,d)) = delayTime t . stretch d

-- | Apply a function under a span transformation.
sunder :: (Delayable a, Stretchable a, Delayable b, Stretchable b) => Span -> (a -> b) -> a -> b
sunder s f = sapp (sinvert s) . f . sapp s

-- | The inversion of a span.
--                                    
-- > sinvert (sinvert s) = s
-- > sapp (sinvert s) . sapp s = id
--
sinvert :: Span -> Span
sinvert (Span (t,d)) = Span (mirror t, recip d)

-- TODO add "individual scaling" component, i.e. scale just duration not both time and duration
-- Useful for implementing separation in articulation etc


{-
type TFun a = Time -> a
type BoundTFun a = (Span, TFun a)
type LinTFun a = (Span, (a, a))
type Event x = (Span, x)
-}

-- TODO move
deriving instance Delayable a => Delayable (NoStretch a)
deriving instance HasOnset a => HasOnset (NoStretch a)
deriving instance HasOffset a => HasOffset (NoStretch a)
deriving instance HasDuration a => HasDuration (NoStretch a)

deriving instance Stretchable a => Stretchable (NoDelay a)
deriving instance HasOnset a => HasOnset (NoDelay a)
deriving instance HasOffset a => HasOffset (NoDelay a)
deriving instance HasDuration a => HasDuration (NoDelay a)


