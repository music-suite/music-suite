{-# OPTIONS_GHC
  -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports
  #-}
module Music.Time.Types
  ( -- * Basic types
    Time,
    Duration,
    Alignment,
    LocalDuration,

    -- ** Convert between time and duration
    -- $convert
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
    onsetAndOffset,
    onsetAndDuration,
    durationAndOffset,
    stretchComponent,
    delayComponent,
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
    strictlyInside,
    closestPointInside,

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
  )
where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens hiding
  ( (<|),
    Indexable,
    Level,
    below,
    index,
    inside,
    parts,
    reversed,
    transform,
    (|>),
  )
import Control.Monad.State.Lazy
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.AffineSpace.Point.Offsets
import Data.List (mapAccumL, mapAccumR)
import Data.Ratio
import Data.Semigroup
import Data.Typeable
import Data.VectorSpace
import Music.Score.Internal.Util (unRatio)
import Music.Time.Internal.Util (showRatio)

-- import           Data.Fixed

-- $convert
--
-- Note that you should use '.-.' and '.+^' to convert between time and
-- duration. To refer to time zero (the beginning of the music), use
-- 'origin'.

-- |
-- Internal time representation. Can be anything with instances
-- for 'Fractional' and 'RealFrac'.
type TimeBase = Rational

{-
type TimeBase = Fixed E12

instance HasResolution a => AdditiveGroup (Fixed a) where
  zeroV = 0
  negateV = negate
  (^+^) = (+)

instance Floating TimeBase where
deriving instance Floating Time
deriving instance Floating Duration
-}

type Alignment = Duration

type LocalDuration = Alignment

{-# DEPRECATED LocalDuration "Use 'Alignment'" #-}

-- |
-- Duration, corresponding to note values in standard notation.
-- The standard names can be used: @1\/2@ for half note @1\/4@ for a quarter note and so on.
newtype Duration = Duration { _getDuration :: TimeBase }
  deriving (Eq, Ord, Typeable, Enum, Num, Fractional, Real, RealFrac)

-- Duration is a one-dimensional 'VectorSpace', and is the associated vector space of time points.
-- It is a also an 'AdditiveGroup' (and hence also 'Monoid' and 'Semigroup') under addition.
--
-- 'Duration' is invariant under translation so 'delay' has no effect on it.
--

instance Show Duration where
  show = showRatio . toRational

instance ToJSON Duration where
  -- toJSON = JSON.Number . realToFrac
  toJSON x = let (a, b) = unRatio (toRational x) in toJSON [a, b]

instance FromJSON Duration where
  parseJSON x = do
    [a, b] <- parseJSON x
    return $ realToFrac $ (a :: Integer) Data.Ratio.% b

instance Semigroup Duration where
  (<>) = (*^)

instance Monoid Duration where

  mempty = 1


instance AdditiveGroup Duration where

  zeroV = 0

  (^+^) = (+)

  negateV = negate

instance VectorSpace Duration where

  type Scalar Duration = Duration

  (*^) = (*)

instance InnerSpace Duration where
  (<.>) = (*)

-- | 'Time' represents points in time space. The difference between two time points
-- is a 'Duration', for example in a bar of duration 4/4 (that is 1), the difference
-- between the first and third beat 1/2.
--
-- Time has an origin (zero) which usually represents the beginning of the musical
-- performance, but this may not always be the case, as the modelled music may be
-- infinite, or contain a musical pickup. Hence 'Time' values can be negative.
newtype Time = Time {_getTime :: TimeBase}
  deriving (Eq, Ord, Typeable, Enum, Num, Fractional, Real, RealFrac)

-- Time forms an affine space with durations as the underlying vector space, that is, we
-- can add a time to a duration to get a new time using '.+^', take the difference of two
-- times to get a duration using '.-.'. 'Time' forms an 'AffineSpace' with 'Duration' as
-- difference space.

instance Show Time where
  show = showRatio . toRational

instance ToJSON Time where
  toJSON x = toJSON $ (x .-. 0)

instance FromJSON Time where
  parseJSON = fmap (0 .+^) . parseJSON

instance Semigroup Time where
  (<>) = (+)

instance Monoid Time where

  mempty = 0

instance AdditiveGroup Time where

  zeroV = 0

  (^+^) = (+)

  negateV = negate

instance VectorSpace Time where

  type Scalar Time = LocalDuration

  Duration x *^ Time y = Time (x * y)

instance AffineSpace Time where

  type Diff Time = LocalDuration

  Time x .-. Time y = Duration (x - y)

  Time x .+^ Duration y = Time (x + y)

-- | Interpret as durations from 0.
--
-- > toAbsoluteTime (toRelativeTime xs) == xs
--
-- > lenght xs == length (toRelativeTime xs)
--
-- >>> toAbsoluteTime [1,1,1] :: [Time]
-- [1,2,3]
toAbsoluteTime :: [Duration] -> [Time]
toAbsoluteTime = tail . offsetPoints 0

-- | Duration between 0 and first value and so on until the last.
--
-- > toAbsoluteTime (toRelativeTime xs) == xs
--
-- > lenght xs == length (toRelativeTime xs)
--
-- >>> toRelativeTime [1,2,3]
-- [1,1,1]
toRelativeTime :: [Time] -> [Duration]
toRelativeTime = tail . pointOffsets 0

-- TODO rename these two...

-- | Duration between values until the last, then up to the given final value.
-- > lenght xs == length (toRelativeTime xs)
toRelativeTimeN' :: Time -> [Time] -> [Duration]
toRelativeTimeN' or = snd . Data.List.mapAccumR g or
  where
    g prev p = (p, prev .-. p)

-- Same as toRelativeTimeN' but always returns 0 as the last value...
-- TODO remove
toRelativeTimeN :: [Time] -> [Duration]
toRelativeTimeN [] = []
toRelativeTimeN xs = toRelativeTimeN' (last xs) xs

-- |
-- A 'Span' represents a specific time interval.
--
-- Another way of looking at 'Span' is that it represents a time transformation where
-- onset is translation and duration is scaling.
--
-- This type is known as 'Arc' in Tidal and as 'Era' in the active package.
newtype Span = Span {getSpan :: (Time, Duration)}
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
-- >>> (2 <-> 3)^.onsetAndOffset
-- (2,3)
--
-- >>> (2 <-> 3)^.onsetAndDuration
-- (2,1)
--
-- >>> (10 >-> 5)^.onsetAndOffset
-- (10,15)
--
-- >>> (10 >-> 5)^.onsetAndDuration
-- (10,5)

instance Show Span where
  show = showRange

-- Which form should we use?

instance ToJSON Span where
  toJSON (view onsetAndOffset -> (a, b)) = JSON.object [("onset", toJSON a), ("offset", toJSON b)]

instance FromJSON Span where
  parseJSON (JSON.Object x) = liftA2 (<->) onset offset
    where
      onset = x JSON..: "onset"
      offset = x JSON..: "offset"
  parseJSON _ = mzero

instance Semigroup Span where
  (<>) = (^+^)

instance Monoid Span where

  mempty = zeroV

instance AdditiveGroup Span where

  zeroV = 0 <-> 1

  Span (t1, d1) ^+^ Span (t2, d2) = Span (t1 ^+^ d1 *^ t2, d1 * d2)

  negateV (Span (t, d)) = Span (- t ^/ d, recip d)

instance VectorSpace Span where

  type Scalar Span = Duration

  x *^ Span (t, d) = Span (x *^ t, x *^ d)

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
(<->) :: Time -> Time -> Span
t <-> u = t >-> (u .-. t)

-- |
-- @t >-> d@ represents the span between @t@ and @t .+^ d@.
(>->) :: Time -> Duration -> Span
(>->) = curry Span

-- |
-- @d \<-\> t@ represents the span between @t .-^ d@ and @t@.
(<-<) :: Duration -> Time -> Span
a <-< b = (b .-^ a) <-> b

-- |
-- View a span as pair of onset and offset.
onsetAndOffset :: Iso' Span (Time, Time)
onsetAndOffset = iso (\x -> let (t, d) = getSpan x in (t, t .+^ d)) (uncurry (<->))

-- |
-- View a span as a pair of onset and duration.
onsetAndDuration :: Iso' Span (Time, Duration)
onsetAndDuration = iso getSpan Span

-- |
-- View a span as a pair of duration and offset.
durationAndOffset :: Iso' Span (Duration, Time)
durationAndOffset = iso (\x -> let (t, d) = getSpan x in (d, t .+^ d)) (uncurry (<-<))

-- |
-- View a span as pair of onset and offset.
range :: Iso' Span (Time, Time)
range = onsetAndOffset
{-# DEPRECATED range "Use onsetAndOffset" #-}

-- |
-- View a span as a pair of onset and duration.
delta :: Iso' Span (Time, Duration)
delta = onsetAndDuration
{-# DEPRECATED delta "Use onsetAndDuration" #-}

-- |
-- View a span as a pair of duration and offset.
codelta :: Iso' Span (Duration, Time)
codelta = durationAndOffset
{-# DEPRECATED codelta "Use durationAndOffset" #-}

-- |
-- Show a span in range notation, i.e. @t1 \<-\> t2@.
showRange :: Span -> String
showRange (view onsetAndOffset -> (t, u)) = show t ++ " <-> " ++ show u

-- |
-- Show a span in delta notation, i.e. @t >-> d@.
showDelta :: Span -> String
showDelta (view onsetAndDuration -> (t, d)) = show t ++ " >-> " ++ show d

-- |
-- Show a span in codelta notation, i.e. @t <-< d@.
showCodelta :: Span -> String
showCodelta (view durationAndOffset -> (d, u)) = show d ++ " <-< " ++ show u

-- |
-- Access the delay component in a span.
delayComponent :: Span -> Time
delayComponent x = x ^. onsetAndDuration . _1

-- |
-- Access the stretch component in a span.
stretchComponent :: Span -> Duration
stretchComponent x = x ^. onsetAndDuration . _2

-- |
-- A prism to the subset of 'Span' that performs a delay but no stretch.
fixedDurationSpan :: Prism' Span Time
fixedDurationSpan = prism' (\t -> view (from onsetAndDuration) (t, 1)) $ \x -> case view onsetAndDuration x of
  (t, 1) -> Just t
  _ -> Nothing

-- |
-- A prism to the subset of 'Span' that performs a stretch but no delay.
fixedOnsetSpan :: Prism' Span Duration
fixedOnsetSpan = prism' (\d -> view (from onsetAndDuration) (0, d)) $ \x -> case view onsetAndDuration x of
  (0, d) -> Just d
  _ -> Nothing

--

-- $forwardBackWardEmpty
--
-- A span is either /forward/, /backward/ or /empty/.
--
-- @any id [isForwardSpan x, isBackwardSpan x, isEmptySpan x] == True@
-- @all not [isForwardSpan x, isBackwardSpan x, isEmptySpan x] == False@

-- |
-- Whether the given span has a positive duration, i.e. whether its 'onset' is before its 'offset'.
isForwardSpan :: Span -> Bool
isForwardSpan = (> 0) . signum . _durationS

-- |
-- Whether the given span has a negative duration, i.e. whether its 'offset' is before its 'onset'.
isBackwardSpan :: Span -> Bool
isBackwardSpan = (< 0) . signum . _durationS

-- |
-- Whether the given span is empty, i.e. whether its 'onset' and 'offset' are equivalent.
isEmptySpan :: Span -> Bool
isEmptySpan = (== 0) . signum . _durationS

-- |
-- Reflect a span through its midpoint.
reverseSpan :: Span -> Span
reverseSpan s = reflectSpan (_midpointS s) s

-- |
-- Reflect a span through an arbitrary point.
reflectSpan :: Time -> Span -> Span
reflectSpan p = over (onsetAndOffset . both) (reflectThrough p)

-- |
-- Normalize a span, i.e. reverse it if negative, and do nothing otherwise.
--
-- @
-- abs $ s^.'duration' = abs $ ('normalizeSpan' s)^.'duration'
-- s^.'midpoint' = ('normalizeSpan' s)^.'midpoint'
-- @
normalizeSpan :: Span -> Span
normalizeSpan s = if isForwardSpan s then s else reverseSpan s

-- TODO Duplicate as normalizeNoteSpan


infixl 5 `inside`

infixl 5 `encloses`

infixl 5 `properlyEncloses`

infixl 5 `overlaps`

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
inside :: Time -> Span -> Bool
inside x (view onsetAndOffset -> (t, u)) = t <= x && x <= u

-- >>> 2 `inside` (3<->4)
-- False
-- >>> 3 `inside` (3<->4)
-- True
--
-- >>> 2 `strictlyInside` (3<->4)
-- False
-- >>> 3 `strictlyInside` (3<->4)
-- False
-- >>> 3.5 `strictlyInside` (3<->4)
-- True
--
strictlyInside :: Time -> Span -> Bool
strictlyInside x (view onsetAndOffset -> (t, u)) = t < x && x < u

-- | If the given time is outside the given span, return the closest point inside.
--   Otherwise return the given time.
--
-- >>> closestPointInside (1 <-> 3) 0
-- 1
-- >>> closestPointInside (1 <-> 3) 55
-- 3
-- >>> closestPointInside (1 <-> 3) 2
-- 2
closestPointInside :: Span -> Time -> Time
closestPointInside ((^. onsetAndOffset) -> (m, n)) t
  | t < m = m
  | t > n = n
  | otherwise = t

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
overlaps :: Span -> Span -> Bool
a `overlaps` b = not (a `isBefore` b) && not (b `isBefore` a)

-- |
-- Whether the first given span occurs before the second span.
isBefore :: Span -> Span -> Bool
a `isBefore` b = (_onsetS a `max` _offsetS a) <= (_onsetS b `min` _offsetS b)

-- TODO resolve this so we can use actual onset/offset etc in the above definitions
-- Same as (onset, offset), defined here for bootstrapping reasons
_onsetS (view onsetAndOffset -> (t1, t2)) = t1

_offsetS (view onsetAndOffset -> (t1, t2)) = t2

_midpointS s = _onsetS s .+^ _durationS s / 2

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

_onsetS :: Span -> Time

_offsetS :: Span -> Time

_midpointS :: Span -> Time

_durationS :: Span -> LocalDuration
