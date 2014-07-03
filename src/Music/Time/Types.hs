
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Time.Types (

        -- * Duration
        Duration,
        toDuration,
        fromDuration,

        -- * Time points
        Time,
        toTime,
        fromTime,

        -- $convert
        offsetPoints,
        -- TODO name
        toAbs,

        -- * Time spans
        Span,
        -- *** Creating spans
        (<->),
        (>->),
        (<-<),
        -- *** Accessing spans
        range,
        delta,
        codelta,
        showRange,
        showDelta,
        showCodelta,

        -- ** Properties
        -- $forwardBackWardEmpty
        isForwardSpan,
        isBackwardSpan,
        isEmptySpan,

        -- ** Transformations
        reverseSpan,
        reflectSpan,
        normalizeSpan,
        
        -- ** Delay and stretch component
        delayComponent,
        stretchComponent,
        fixedOnsetSpan,
        fixedDurationSpan,

        -- ** Points in spans
        inside,

        -- ** Partial orders
        isProper,
        isBefore,
        encloses,
        overlaps,
        -- union
        -- intersection (alt name 'overlap')
        -- difference (would actually become a split)
  ) where

import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Semigroup
import           Data.Typeable
import           Data.VectorSpace
import           Data.List (mapAccumL)

import           Music.Time.Internal.Util (showRatio)


-- $convert
--
-- Note that you should use '.-.' and '.+^' to convert between time and
-- duration. To refer to time zero (the beginning of the music), use
-- 'origin'.
--

-- |
-- Internal time representation. Can be anything with instances
-- for 'Fractional' and 'RealFrac'.
--
type TimeBase = Rational
-- type TimeBase = Fixed E12

-- instance HasResolution a => AdditiveGroup (Fixed a) where
--   zeroV = 0
--   negateV = negate
--   (^+^) = (+)

-- Can be enabled for experimental time representation
-- deriving instance Floating Time
-- deriving instance Floating Duration


-- |
-- Duration, corresponding to note values in standard notation.
-- The standard names can be used: @1\/2@ for half note @1\/4@ for a quarter note and so on.
--
-- Duration is a one-dimensional 'VectorSpace', and is the associated vector space of time points.
-- It is a also an 'AdditiveGroup' (and hence also 'Monoid' and 'Semigroup') under addition.
--
-- 'Duration' is invariant under translation so 'delay' has no effect on it.
--
-- The semantics are given by
--
-- @
-- type Duration = R
-- @
--
newtype Duration = Duration { getDuration :: TimeBase }
  deriving (Eq, Ord, Num, Enum, Fractional, Real, RealFrac, Typeable)

instance Show Duration where
  show = showRatio . realToFrac

instance InnerSpace Duration where
  (<.>) = (*)

instance AdditiveGroup Duration where
  zeroV = 0
  (^+^) = (+)
  negateV = negate

instance VectorSpace Duration where
  type Scalar Duration = Duration
  (*^) = (*)

instance Semigroup Duration where
  (<>) = (*^)

instance Monoid Duration where
  mempty  = 1
  mappend = (*^)
 -- TODO use some notion of norm rather than 1

-- |
-- Convert a value to a duration.
--
toDuration :: Real a => a -> Duration
toDuration = realToFrac

-- |
-- Convert a value to a duration.
--
fromDuration :: Fractional a => Duration -> a
fromDuration = realToFrac


-- |
-- Time points, representing duration since some known reference time, typically the start
-- of the music. Note that time can be negative, representing values occuring before the
-- reference time.
--
-- Time forms an affine space with durations as the underlying vector space, that is, we
-- can add a time to a duration to get a new time using '.+^', take the difference of two
-- times to get a duration using '.-.'. 'Time' forms an 'AffineSpace' with 'Duration' as
-- difference space.
--
-- The semantics are given by
--
-- @
-- type Time = R
-- @
--
newtype Time = Time { getTime :: TimeBase }
  deriving (Eq, Ord, Num, Enum, Fractional, Real, RealFrac, Typeable)

instance Show Time where
  show = showRatio . realToFrac

deriving instance AdditiveGroup Time

instance VectorSpace Time where
  type Scalar Time = Duration
  Duration x *^ Time y = Time (x * y)

instance AffineSpace Time where
  type Diff Time = Duration
  Time x .-. Time y   = Duration (x - y)
  Time x .+^ Duration y = Time   (x + y)

instance Semigroup Time where
  (<>) = (^+^)

instance Monoid Time where
  mempty  = zeroV
  mappend = (^+^)
  mconcat = sumV

-- |
-- Convert a value to a duration.
--
toTime :: Real a => a -> Time
toTime = realToFrac

-- |
-- Convert a value to a duration.
--
fromTime :: Fractional a => Time -> a
fromTime = realToFrac


-- @length (offsetPoints x xs) = length xs + 1
-- >>> offsetPoints 0 [1,2,1]
-- [0,1,2,1]
-- offsetPoints :: AffineSpace a => Time -> [Duration] -> [Time]
offsetPoints :: AffineSpace a => a -> [Diff a] -> [a]
offsetPoints = scanl (.+^)

toAbs :: [Duration] -> [Time]
toAbs = snd . Data.List.mapAccumL g 0 where g now d = (now .+^ d, now .+^ d)
-- TODO use State instead




-- |
-- A 'Span' represents an onset and offset in time (or equivalently: an onset and a
-- duration, /or/ a duration and an offset, /or/ a duration and a middle point).
--
-- Pattern matching over span is possible (with @ViewPatterns@):
--
-- @
-- foo ('view' 'range'   -> (t1, t2)) = ...
-- foo ('view' 'delta'   -> (t1, d))  = ...
-- foo ('view' 'codelta' -> (d,  t2)) = ...
-- @
--
-- Another way of looking at 'Span' is that it represents a time transformation where
-- onset is translation and duration is scaling.
--
-- TODO How to use with 'transform', 'whilst' etc.
--
-- The semantics are given by
--
-- @
-- type Span = Time x Time
-- @
--
newtype Span = Delta { _delta :: (Time, Duration) }
  deriving (Eq, Ord, Typeable)

-- You can create a span using the constructors '<->', '<-<' and '>->'. Note that:
--
-- > a >-> b = a         <-> (a .+^ b)
-- > a <-< b = (b .-^ a) <-> b
-- > a <-> b = a         >-> (b .-. a)
--
-- To create and destruct a span (in any of its incarnations), use the provided isomorphisms:
--
-- 'Span' is a 'Semigroup', 'Monoid' and 'AdditiveGroup':
--
-- - To convert a span to a pair, use @s^.'range'@.
--
-- - To construct a span from a pair, use @(t, u)^.'from' 'range'@.
--

--
-- $musicTimeSpanIsos
--
-- >>> (2 <-> 3)^.range
-- > (2, 3)
-- >
-- >>> hs> (2 <-> 3)^.delta
-- > (2, 1)
-- >
-- >>> hs> (10 >-> 5)^.range
-- > (10, 15)
-- >
-- >>> hs> (10 >-> 5)^.delta
-- > (10, 5)
--

instance Show Span where
  -- show = showDelta
  show = showRange
  -- Which form should we use?

-- |
-- 'zeroV' or 'mempty' represents the /unit interval/ @0 \<-\> 1@, which also happens to
-- be the identity transformation.
--
instance Semigroup Span where
  (<>) = (^+^)

-- |
-- '<>' or '^+^' composes transformations, i.e. both time and duration is stretched,
-- and then time is added.
--
instance Monoid Span where
  mempty  = zeroV
  mappend = (^+^)

-- |
-- 'negateV' returns the inverse of a given transformation.
--
instance AdditiveGroup Span where
  zeroV   = 0 <-> 1
  Delta (t1, d1) ^+^ Delta (t2, d2) = Delta (t1 ^+^ d1 *^ t2, d1*d2)
  negateV (Delta (t, d)) = Delta (-t ^/ d, recip d)

--
-- a >-> b = a         <-> (a .+^ b)
-- a <-< b = (b .-^ a) <-> b
-- a <-> b = a         >-> (b .-. a)
-- (b .-^ a) <-> b = a <-< b
--

-- |
-- @t \<-\> u@ represents the span between @t@ and @u@.
--
(<->) :: Time -> Time -> Span
t <-> u = t >-> (u .-. t)

-- |
-- @t >-> d@ represents the span between @t@ and @t .+^ d@.
--
(>->) :: Time -> Duration -> Span
(>->) = curry Delta

-- |
-- @d \<-\> t@ represents the span between @t .-^ d@ and @t@.
--
(<-<) :: Duration -> Time -> Span
a <-< b = (b .-^ a) <-> b


-- > (<->) = curry $ view $ from range
-- > (>->) = curry $ view $ from delta

-- |
-- View a span as pair of onset and offset.
--
range :: Iso' Span (Time, Time)
range = iso _range $ uncurry (<->)
  where
    _range x = let (t, d) = _delta x in (t, t .+^ d)

-- |
-- View a span as a pair of onset and duration.
--
delta :: Iso' Span (Time, Duration)
delta = iso _delta Delta

-- |
-- View a span as a pair of duration and offset.
--
codelta :: Iso' Span (Duration, Time)
codelta = iso _codelta $ uncurry (<-<)
  where
    _codelta x = let (t, d) = _delta x in (d, t .+^ d)

-- |
-- Show a span in range notation, i.e. @t1 \<-\> t2@.
--
showRange :: Span -> String
showRange (view range -> (t,u)) = show t ++ " <-> " ++ show u

-- |
-- Show a span in delta notation, i.e. @t >-> d@.
--
showDelta :: Span -> String
showDelta (view delta -> (t,d)) = show t ++ " >-> " ++ show d

-- |
-- Show a span in codelta notation, i.e. @t <-< d@.
--
showCodelta :: Span -> String
showCodelta (view codelta -> (d,u)) = show d ++ " <-< " ++ show u

-- |
-- Access the delay component in a span.
--
delayComponent :: Span -> Time
delayComponent x = x ^. delta . _1

-- |
-- Access the stretch component in a span.
--
stretchComponent :: Span -> Duration
stretchComponent x = x ^. delta . _2

-- |
-- A prism to the subset of 'Span' that performs a delay but no stretch.
--
fixedDurationSpan :: Prism' Span Time
fixedDurationSpan = prism' (\t -> view (from delta) (t, 1)) $ \x -> case view delta x of
  (t, 1) -> Just t
  _      -> Nothing

-- |
-- A prism to the subset of 'Span' that performs a stretch but no delay.
--
fixedOnsetSpan :: Prism' Span Duration
fixedOnsetSpan = prism' (\d -> view (from delta) (0, d)) $ \x -> case view delta x of
  (0, d) -> Just d
  _      -> Nothing

--
-- $forwardBackWardEmpty
--
-- A span is either /forward/, /backward/ or /empty/.
--
-- >>> any id [isForwardSpan x, isBackwardSpan x, isEmptySpan x]
-- True
--
-- >>> all not [isForwardSpan x, isBackwardSpan x, isEmptySpan x]
-- False
--

-- |
-- Whether the given span has a positive duration, i.e. whether its 'onset' is before its 'offset'.
--
isForwardSpan :: Span -> Bool
isForwardSpan = (> 0) . signum . _durationS

-- |
-- Whether the given span has a negative duration, i.e. whether its 'offset' is before its 'onset'.
--
isBackwardSpan :: Span -> Bool
isBackwardSpan = (< 0) . signum . _durationS

-- |
-- Whether the given span is empty, i.e. whether its 'onset' and 'offset' are equivalent.
--
isEmptySpan :: Span -> Bool
isEmptySpan = (== 0) . signum . _durationS


-- |
-- Reflect a span about its midpoint.
--
reverseSpan :: Span -> Span
reverseSpan s = reflectSpan (_middleS s) s

-- |
-- Reflect a span about an arbitrary point.
--
reflectSpan :: Time -> Span -> Span
reflectSpan p = over (range . both) (reflectThrough p)

-- |
-- Normalize a span, i.e. reverse it if negative, and do nothing otherwise.
--
normalizeSpan :: Span -> Span
normalizeSpan s = if isForwardSpan s then s else reverseSpan s
-- TODO Duplicate as normalizeNoteSpan

-- |
-- Whether this is a proper span, i.e. whether @'_onset' x '<' '_offset' x@.
--
isProper :: Span -> Bool
isProper (view range -> (t, u)) = t < u
{-# DEPRECATED isProper "Use 'isForwardSpan'" #-}

-- |
-- Whether the given point falls inside the given span (inclusively).
--
-- Designed to be used infix, for example
--
-- @
-- 0.5 ``inside`` (1 '<->' 2)
-- @
--
inside :: Time -> Span -> Bool
inside x (view range -> (t, u)) = t <= x && x <= u

-- |
-- Whether the first given span encloses the second span.
--
encloses :: Span -> Span -> Bool
a `encloses` b = _onsetS b `inside` a && _offsetS b `inside` a

-- |
-- Whether the given span overlaps.
--
overlaps :: Span -> Span -> Bool
a `overlaps` b = not (a `isBefore` b) && not (b `isBefore` a)

-- |
-- Whether the first given span occurs before the second span.
--
isBefore :: Span -> Span -> Bool
a `isBefore` b = (_onsetS a `max` _offsetS a) <= (_onsetS b `min` _offsetS b)


-- Same as (onset, offset), defined here for bootstrapping reasons
_onsetS    (view range -> (t1, t2)) = t1
_offsetS   (view range -> (t1, t2)) = t2
_middleS  s = _onsetS s .+^ _durationS s / 2
_durationS s = _offsetS s .-. _onsetS s

