
{-# LANGUAGE OverloadedStrings          #-}
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

        -- * Basic types
        Time,
        Duration,

        -- ** Convert between time and duration
        -- $convert
        offsetPoints,
        toAbsoluteTime,
        toRelativeTime,
        toRelativeTimeN,
        toRelativeTimeN', -- TODO Fairbairn threshold

        -- * Time spans
        Span,

        -- ** Constructing spans
        (<->),
        (>->),
        (<-<),

        delta,
        range,
        codelta,

        fixedDurationSpan,
        fixedOnsetSpan,

        -- ** Transformations
        normalizeSpan,
        reverseSpan,
        reflectSpan,
        
        -- ** Properties
        isEmptySpan,
        isForwardSpan,
        isBackwardSpan,        

        -- delayComponent,
        -- stretchComponent,

        -- ** Points in spans
        inside,

        -- ** Partial orders
        encloses,
        properlyEncloses,
        overlaps,

        -- *** etc.
        isBefore,
        afterOnset,
        strictlyAfterOnset,
        beforeOnset,
        strictlyBeforeOnset,
        afterOffset,
        strictlyAfterOffset,
        beforeOffset,
        strictlyBeforeOffset,
        
        startsWhenStarts,
        startsWhenStops,
        stopsWhenStops,
        stopsWhenStarts,
        
        startsBefore,
        startsLater,
        stopsAtTheSameTime,
        stopsBefore,
        stopsLater,
        
        -- union
        -- intersection (alt name 'overlap')
        -- difference (would actually become a split)

        -- ** Read/Show
        showRange,
        showDelta,
        showCodelta,
  ) where

import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))
--
import           Control.Applicative.Backwards
import           Control.Monad.State.Lazy
--
import           Data.Aeson (ToJSON(..))
import qualified Data.Aeson as JSON

import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Semigroup
import           Data.Typeable
import           Data.VectorSpace
import           Data.List (mapAccumL, mapAccumR)
import           Data.Ratio

import           Music.Time.Internal.Util (showRatio)
-- import           Data.Fixed

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
{-
type TimeBase = Fixed E12

instance HasResolution a => AdditiveGroup (Fixed a) where
  zeroV = 0
  negateV = negate
  (^+^) = (+)

-- Can be enabled for experimental time representation
instance Floating TimeBase where
deriving instance Floating Time
deriving instance Floating Duration
-}


-- |
-- Duration, corresponding to note values in standard notation.
-- The standard names can be used: @1\/2@ for half note @1\/4@ for a quarter note and so on.
--
newtype Duration = Duration { getDuration :: TimeBase }
  deriving (Eq, Ord, Num, Enum, Fractional, Real, RealFrac, Typeable)

-- Duration is a one-dimensional 'VectorSpace', and is the associated vector space of time points.
-- It is a also an 'AdditiveGroup' (and hence also 'Monoid' and 'Semigroup') under addition.
--
-- 'Duration' is invariant under translation so 'delay' has no effect on it.
--

-- $semantics Duration
--
-- type Duration = R
--

instance Show Duration where
  show = showRatio . realToFrac

instance ToJSON Duration where
  toJSON = JSON.Number . realToFrac

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
newtype Time = Time { getTime :: TimeBase }
  deriving (Eq, Ord, Num, Enum, Fractional, Real, RealFrac, Typeable)

-- Time forms an affine space with durations as the underlying vector space, that is, we
-- can add a time to a duration to get a new time using '.+^', take the difference of two
-- times to get a duration using '.-.'. 'Time' forms an 'AffineSpace' with 'Duration' as
-- difference space.
--

-- $semantics Time
--
-- type Time = R
--

instance Show Time where
  show = showRatio . realToFrac

instance ToJSON Time where
  toJSON = JSON.Number . realToFrac

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



-- TODO terminology
-- Return the "accumulative sum" of the given vecors

-- |
-- @length (offsetPoints x xs) = length xs + 1@
--
-- >>> offsetPoints (0 ::Double) [1,2,1]
-- [0.0,1.0,3.0,4.0]
--
-- @
-- offsetPoints :: 'AffineSpace' a => 'Time' -> ['Duration'] -> ['Time']
-- @
--
offsetPoints :: AffineSpace a => a -> [Diff a] -> [a]
offsetPoints = scanl (.+^)

-- | Convert to delta (time to wait before this note)
toRelativeTime :: [Time] -> [Duration]
toRelativeTime = snd . mapAccumL g 0 where g prev t = (t, t .-. prev)
-- toRelativeTime xs = fst $ mapAccumL2 g xs 0 where g t prev = (t .-. prev, t)

-- | Convert to delta (time to wait before next note)
toRelativeTimeN :: [Time] -> [Duration]
toRelativeTimeN [] = []
toRelativeTimeN xs = toRelativeTimeN' (last xs) xs

-- | Convert to delta (time to wait before next note)
toRelativeTimeN' :: Time -> [Time] -> [Duration]
toRelativeTimeN' end xs = snd $ mapAccumR g end xs where g prev t = (t, prev .-. t)

{-
TODO consolidate with this beat (used in Midi export)

toRelative = snd . List.mapAccumL g 0
    where
        g now (t,d,x) = (t, (0 .+^ (t .-. now),d,x))

-}
-- 0 x,1 x,1 x,1 x
  -- x 1,x 1,x 1,x 0

-- | Convert from delta (time to wait before this note)
toAbsoluteTime :: [Duration] -> [Time]
toAbsoluteTime = tail . offsetPoints 0


-- -- TODO use State instead
-- 
-- -- mapAccumL                 ::                   (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
-- -- \f -> mapM (runState . f) :: MonadState s m => (a -> s -> (b, s)) -> [a] -> s -> ([b], s)
-- 
-- -- mapAccumL :: (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
-- mapAccumL2   :: (a -> s -> (b, s)) -> [a] -> s -> ([b], s)
-- mapAccumL2 f = runState . mapM (state . f)







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
-- @
-- a '<->' b = (a, b)^.'from' 'range'
-- a '>->' b = (a, b)^.'from' 'delta'
-- a '<-<' b = (a, b)^.'from' 'codelta'
-- @
--
newtype Span = Delta { _delta :: (Time, Duration) }
  deriving (Eq, Ord, Typeable)

-- $semantics
--
-- type Span = Time x Time
--

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
-- (2,3)
--
-- >>> (2 <-> 3)^.delta
-- (2,1)
--
-- >>> (10 >-> 5)^.range
-- (10,15)
--
-- >>> (10 >-> 5)^.delta
-- (10,5)
--

instance Show Span where
  -- show = showDelta
  show = showRange
  -- Which form should we use?

instance ToJSON Span where
  toJSON (view range -> (a,b)) = JSON.object [ ("onset", toJSON a), ("offset", toJSON b) ]


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

infixl 6 <->
infixl 6 >->
infixl 6 <-<

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
-- @any id [isForwardSpan x, isBackwardSpan x, isEmptySpan x] == True@
-- @all not [isForwardSpan x, isBackwardSpan x, isEmptySpan x] == False@
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
-- Reflect a span through its midpoint.
--
reverseSpan :: Span -> Span
reverseSpan s = reflectSpan (_midpointS s) s

-- |
-- Reflect a span through an arbitrary point.
--
reflectSpan :: Time -> Span -> Span
reflectSpan p = over (range . both) (reflectThrough p)

-- |
-- Normalize a span, i.e. reverse it if negative, and do nothing otherwise.
--
-- @
-- _duration s = _duration (normalizeSpan s)
-- _midpoint s = _midpoint (normalizeSpan s)
-- @
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

infixl 5 `inside`
infixl 5 `encloses`
infixl 5 `properlyEncloses`
infixl 5 `overlaps`
-- infixl 5 `encloses`
-- infixl 5 `encloses`
-- infixl 5 `encloses`

-- |
-- Whether the given point falls inside the given span (inclusively).
--
-- Designed to be used infix, for example
--
-- >>> 0.5 `inside` 1 <-> 2
-- False
--
-- >>> 1.5 `inside` 1 <-> 2
-- True
--
-- >>> 1 `inside` 1 <-> 2
-- True
--
inside :: Time -> Span -> Bool
inside x (view range -> (t, u)) = t <= x && x <= u

-- |
-- Whether the first given span encloses the second span.
--
-- >>> 0 <-> 3 `encloses` 1 <-> 2
-- True
--
-- >>> 0 <-> 2 `encloses` 1 <-> 2
-- True
--
-- >>> 1 <-> 3 `encloses` 1 <-> 2
-- True
--
-- >>> 1 <-> 2 `encloses` 1 <-> 2
-- True
--
encloses :: Span -> Span -> Bool
a `encloses` b = _onsetS b `inside` a && _offsetS b `inside` a

-- |
-- Whether the first given span encloses the second span.
--
-- >>> 0 <-> 3 `properlyEncloses` 1 <-> 2
-- True
--
-- >>> 0 <-> 2 `properlyEncloses` 1 <-> 2
-- True
--
-- >>> 1 <-> 3 `properlyEncloses` 1 <-> 2
-- True
--
-- >>> 1 <-> 2 `properlyEncloses` 1 <-> 2
-- False
--
properlyEncloses :: Span -> Span -> Bool
a `properlyEncloses` b = a `encloses` b && a /= b



-- TODO more intuitive param order

afterOnset :: Time -> Span -> Bool
t `afterOnset` s = t >= _onsetS s

strictlyAfterOnset :: Time -> Span -> Bool
t `strictlyAfterOnset` s = t > _onsetS s

beforeOnset :: Time -> Span -> Bool
t `beforeOnset` s = t <= _onsetS s

strictlyBeforeOnset :: Time -> Span -> Bool
t `strictlyBeforeOnset` s = t < _onsetS s

afterOffset :: Time -> Span -> Bool
t `afterOffset` s = t >= _offsetS s

strictlyAfterOffset :: Time -> Span -> Bool
t `strictlyAfterOffset` s = t > _offsetS s

beforeOffset :: Time -> Span -> Bool
t `beforeOffset` s = t <= _offsetS s

strictlyBeforeOffset :: Time -> Span -> Bool
t `strictlyBeforeOffset` s = t < _offsetS s


-- Param order OK

-- Name?
startsWhenStarts :: Span -> Span -> Bool
a `startsWhenStarts` b = _onsetS a == _onsetS b

-- Name?
startsWhenStops :: Span -> Span -> Bool
a `startsWhenStops` b = _onsetS a == _offsetS b

-- Name?
stopsWhenStops :: Span -> Span -> Bool
a `stopsWhenStops` b = _offsetS a == _offsetS b

-- Name?
stopsWhenStarts :: Span -> Span -> Bool
a `stopsWhenStarts` b = _offsetS a == _onsetS b


startsBefore :: Span -> Span -> Bool
a `startsBefore` b = _onsetS a < _onsetS b

startsLater :: Span -> Span -> Bool
a `startsLater` b = _onsetS a > _onsetS b

stopsAtTheSameTime :: Span -> Span -> Bool
a `stopsAtTheSameTime` b = _offsetS a == _offsetS b

stopsBefore :: Span -> Span -> Bool
a `stopsBefore` b = _offsetS a < _offsetS b

stopsLater :: Span -> Span -> Bool
a `stopsLater` b = _offsetS a > _offsetS b

{-
contains
curtails
delays
happensDuring
intersects
trisects
isCongruentTo
overlapsAllOf
overlapsOnlyOnsetOf
overlapsOnlyOffsetOf
overlapsOnsetOf
overlapsOffsetOf



-}

-- timespantools.timespan_2_starts_during_timespan_1
-- timespantools.timespan_2_starts_when_timespan_1_starts
-- timespantools.timespan_2_starts_when_timespan_1_stops
-- timespantools.timespan_2_stops_after_timespan_1_starts
-- timespantools.timespan_2_stops_after_timespan_1_stops
-- timespantools.timespan_2_stops_before_timespan_1_starts
-- timespantools.timespan_2_stops_before_timespan_1_stops
-- timespantools.timespan_2_stops_during_timespan_1
-- timespantools.timespan_2_stops_when_timespan_1_starts
-- timespantools.timespan_2_stops_when_timespan_1_stops
-- timespantools.timespan_2_trisects_timespan_1     



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


-- TODO resolve this so we can use actual onset/offset etc in the above definitions
-- Same as (onset, offset), defined here for bootstrapping reasons
_onsetS    (view range -> (t1, t2)) = t1
_offsetS   (view range -> (t1, t2)) = t2
_midpointS  s = _onsetS s .+^ _durationS s / 2
_durationS s = _offsetS s .-. _onsetS s

{-
Two alternative definitions for midpoint:

midpoint x = onset x + duration x / 2
midpoint x = (onset x + offset x) / 2

Both equivalent. Proof:

  let d = b - a
  (a + b)/2 = a + d/2
  (a + b)/2 = a + (b - a)/2
  a + b     = 2a + (b - a)
  a + b     = a + b
-}



