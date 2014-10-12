
{-# LANGUAGE TypeOperators #-}
import Data.Semigroup hiding (Product, Sum)
import Data.Functor.Product
import Data.Functor.Sum

----------
data Hertz
-- Augmentable, Alterable
-- HasNumber?, HasQuality (using Lens?)
-- Number, Quality
-- isStep/isLeap/isCompound
-- separate/simple/octaves/invert

-- Common
data Chromatic
data Diatonic
type Interval = (Diatonic, Chromatic)
-- Vector with basis {d2,A1} {(1,0),(0,1)}

----------
-- Point in time space. 0 is beginning of music.
data Time
-- Vector in time space.
data Duration
-- "A time range"
-- Two points in time space (onset, duration).
-- Also a linear transformation in time space (translation, scaling). 
type Span = (Time, Duration) -- = (Time, Time) = (Duration, Time)
-- Vector with basis (assuming Time/Duration repr.) {delay 1,stretch 1} {(1,0),(0,1)}

-- A value starting at a certain time
-- Ordered on time, so we can use sorted lists/sets/maps
type Future a = (Min Time, a)
-- A value stopping at a certain time
type Past a = (Max Time, a)

-- There is a related structure InterpolatingMap (or whatever)
-- Similar to map, but can look up "in between" incices, using some interpoation function (const, flip const, average etc.)

-- A discrete time function
type Reactive a = (a, [Future a]) -- = ([Past a], a)

-- A continous time function
type Behavior a = Time -> a

-- A continous time function of unspecified range
type Spline a = Time -> a -- where {t : Time, 0 <= t <= 1}
-- A continous, finite time function (analogous to a finite list)
type Segment a = Voice (Spline a) -- = Note (Spline a)

newtype Rest a = C1 Duration
newtype Note a = C2 (Duration, a)
newtype Chord a = C3 () -- ?

type (:*:) = Product
type (:+:) = Sum
infixr 4 :+:
-- These are not placed, i.e in parallel composition there is no pickup/putdown
type NoteRestChord = Rest :+: Note :+: Chord
-- Functor, Foldable, Traversable

fromRest :: Rest a -> NoteRestChord a
fromRest = InL

fromNote :: Note a -> NoteRestChord a
fromNote = InR . InL

fromChord :: Chord a -> NoteRestChord a
fromChord = InR . InR

type Delayed a = (Time, a) -- call offset/pickup etc.?
--  "a value that is active within its own span"
type Event a = (Span, a)

type ~[a] = UnorderedList a
type #[a] = OrderedList a

type Voice a = [Note a]  = #[(Duration, a)] = Duration -> Int -> [a] -- ?
type Track a = [Event a] = ~[(Span, a)]     = Span -> [a]
-- Unclear: In the Voice case we care about order, in the track case we don't
-- Why? Duration is a "vector", so we can add them to get several "offsets" (i.e. sequential composition)
-- Span is a "point"/"transformation" so we can not add them.







-- This is a Tidal pattern:
-- It is in fact (semantically) equivalent to a track, except it allows for leak-free infinite structures
-- by placing it in a Span reader monad (i.e. we know what the system is currently rendering)

type Pattern a = Span -> Track a
-- Note that if we use the (Span -> [a]) formulation, we get a double reader monad:
type Pattern a = Span -> Span -> [a]
-- Which can of course be reduced to (Span -> [a])

