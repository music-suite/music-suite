
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
{-# LANGUAGE CPP                        #-}

module TimeTypes (
        (!),
        tabulated,

        -- * Music.Time.Transform
        -- ** The Transformable class
        Transformable(..),
        -- *** Apply under a transformation

        under,      -- :: (Transformable a, Transformable b) => Span -> (a -> b) -> a -> b
        underM,
        underW,
        -- underL,
        -- underDelay,
        -- underStretch,
        -- conjugate,  -- :: Span -> Span -> Span

        -- *** Specific transformations
        delay,
        -- delay',
        undelay,
        stretch,
        compress,
        delaying,
        undelaying,
        stretching,
        compressing,

        -- * Music.Time.Duration
        -- ** The HasDuration class
        HasDuration(..),
        -- ** Stretching to absolute duration
        duration,
        stretchTo,

        -- * Music.Time.Position
        -- ** The HasPosition class
        HasPosition(..),
        -- ** Inspecting position
        era,
        -- _era,

        -- ** Specific positions
        onset,          -- :: HasPosition a => a -> Time
        offset,         -- :: HasPosition a => a -> Time
        -- _onset,
        -- _offset,
        preOnset,       -- :: HasPosition a => a -> Time
        postOnset,      -- :: HasPosition a => a -> Time
        postOffset,     -- :: HasPosition a => a -> Time
        
        -- ** Moving to absolute positions
        startAt,        -- :: (Transformable a, HasPosition a) => Time -> a -> a
        stopAt,         -- :: (Transformable a, HasPosition a) => Time -> a -> a
        placeAt,        -- :: (Transformable a, HasPosition a) => Duration -> Time -> a -> a
        -- pinned,        -- :: (HasPosition a, HasPosition b, Transformable b) => (a -> b) -> a -> b

        -- * Music.Time.Reverse
        -- ** The Reversible class
        Reversible(..),
        reversed,

        -- * Music.Time.Split
        -- ** The Splittable class
        Splittable(..),

        -- * Music.Time.Combinators
        -- ** Align without composition
        lead,           -- :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
        follow,         -- :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
        -- ** Align and compose
        after,
        before,
        during,
        sustain,
        -- ** Composition operators
        Sequential(..),
        scat,
        pcat,
        times,

        -- * Music.Time.Types
        -- ** Duration
        Duration,
        -- ** Time points
        Time,
        -- ** Time spans
        Span,
        -- *** Constructing time spans
        (<->),
        (>->),
        -- *** Deconstructing time spans
        range,
        delta,
        -- *** Points in spans
        inside,
        -- *** Important values (XXX)
        start,
        stop,
        unit,

        -- * Music.Time.Stretched
        Stretched,
        stretched,
        stretchedValue,
        -- runStretched,

        -- * Music.Time.Delayed
        Delayed,
        delayed,
        delayedValue,
        -- runDelayed,

        -- * Music.Time.Note
        Note,
        note,
        noteValue,
        -- runNote,
        -- reifyNote,
        -- noteValue',
        -- mapNote,

        -- * Music.Time.Bounds
        Bounds,
        bounds,
        trim,
        -- trimG,
        bounded,

        -- * Music.Time.Segment
        Segment,
        fromSegment,
        fromSegment2,

        -- * Music.Time.Behavior
        Behavior,
        (!^),
        behavior',
        behavior,

        -- ** Special behaviors
        time,
        ui,
        sine,
        cosine,
        sawtooth,
        dirac,
        turnOn,
        turnOff,

        -- ** Combinators
        switch,
        switch',
        splice,
        concatBehavior,

        -- * Music.Time.Voice
        Voice,
        voice,
        zipVoice,
        zipVoiceWith,
        dzipVoiceWith,
        mergeEqualNotes,

        -- * Music.Time.Voices
        Divide,
        voiceList,
        Voices,
        voiceMap,
        concatSubVoices,

        -- * Music.Time.Reactive
        Reactive,

        -- * Music.Time.Score
        Score,
        mapWithSpan,
        filterWithSpan,
        mapFilterWithSpan,
        mapEvents,
        filterEvents,
        mapFilterEvents,

        -- * Music.Score.Pitch
        Pitch,
        Interval,
        SetPitch,
        HasPitch(..),
        HasPitches(..),
        Transposable,
        pitch',
        pitches',
        up,
        down,
        above,
        below,
        inv,
        -- octavesUp,
        -- octavesDown,
        -- octavesAbove,
        -- octavesBelow,

        -- * Music.Score.Dynamic
        Dynamic,
        SetDynamic,
        HasDynamic(..),
        HasDynamics(..),
        dynamic',
        dynamics',

        -- * Music.Score.Articulation
        Articulation,
        SetArticulation,
        HasArticulation(..),
        HasArticulations(..),
        articulation',
        articulations',

        -- * Music.Score.Part
        Part,
        SetPart,
        HasPart(..),
        HasParts(..),
        part',
        parts',

  ) where

import qualified Data.ByteString.Lazy         as ByteString
import           Data.Default
import qualified Diagrams.Backend.SVG         as SVG
import           Diagrams.Prelude             hiding (Duration, Dynamic,
                                               Segment, Time, Transformable,
                                               after, atTime, duration, during,
                                               _era, interval, offset, place,
                                               position, start, stretch, inv,
                                               stretchTo, transform, trim, era,
                                               under, value, view, (<->), (|>), ui)
import           System.Process               (system)
import           Text.Blaze.Svg.Renderer.Utf8 (renderSvg)


import           Control.Applicative
import           Control.Arrow                (first, second, (***))
import qualified Control.Category
import           Control.Comonad
import           Control.Comonad.Env
import           Control.Lens                 hiding (Indexable, index, parts, above, below,
                                               reversed, transform, under, (|>), inside)
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Distributive
import           Data.Foldable                (Foldable)
import qualified Data.Foldable                as Foldable
import           Data.Functor.Rep
import           Data.List.NonEmpty           (NonEmpty)
import           Data.Maybe
import           Data.NumInstances
import           Data.Semigroup
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Traversable             (Traversable)
import qualified Data.Traversable             as T
import           Data.Typeable
import           Data.VectorSpace
import           Music.Pitch.Literal

import           Data.Int
import           Test.SmallCheck.Series       hiding ((><), NonEmpty)
import           Test.Tasty
import           Test.Tasty.SmallCheck

import qualified Data.Ratio                   as Util_Ratio


-- Misc instances

instance Monoid b => Monad ((,) b) where
  return = pure
  (>>=) = flip (=<<)
    where
      (=<<) = (join .) . fmap
      join (b, (b', a)) = (b `mappend` b', a)

-- These are already in 'lens'
-- deriving instance Foldable ((,) o)
-- deriving instance Traversable ((,) o)


{-
  TODO

  - Use graphing and verify that timed fmap (i.e. reactive's apply) works.

  - New representations for Score, Voice and Reactive


-}




-- Types etc

{-
  Law Semigroup
    a <> (b <> c)  = (a <> b) <> c
    a    <> mempty = a
    mempty <> a    = a

  Law AdditiveGroup
    a ^+^ (b ^+^ c)  = (a ^+^ b) ^+^ c
    a    ^+^ zeroV   = a
    zeroV  ^+^ a     = a
    a    ^+^ negateV a = zeroV
    negateV a ^+^ a    = zeroV
    a ^+^ b        = b ^+^ a

  Law Functor
  Law Eq
  Law Ord
  Law
-}

-- | A value in the unit interval /(0,1)/.
newtype Normalized a = Normalized { getNormalized :: a }
  deriving (Eq, Ord, Show, Functor)

instance Num a => Bounded (Normalized a) where
  minBound = Normalized 0
  maxBound = Normalized 1
instance (Num a, Ord a) => Num (Normalized a) where
  fromInteger = toNorm . fromInteger
instance (Num a, Ord a, Fractional a) => Fractional (Normalized a) where
  fromRational = toNorm . fromRational

toNorm   = fromMaybe (error "Outside 0-1") . (^? normalize)
fromNorm = (^. unnormalize)


zipNormalizedWith :: (Num a, Ord a, Num b, Ord b, Num c, Ord c) => (a -> b -> c) -> Normalized a -> Normalized b -> Maybe (Normalized c)
zipNormalizedWith f a b = ((a^.unnormalize) `f` (b^.unnormalize))^? normalize

normalize' = Normalized
addLim = zipNormalizedWith (+)

normalize :: (Num a, Ord a) => Prism' a (Normalized a)
normalize = prism getNormalized (\x -> if 0 <= x && x <= 1 then Right (Normalized x) else Left x)

unnormalize :: (Num a, Ord a) => Getter (Normalized a) a
unnormalize = re normalize









-- |
-- Index a representable functor.
--
-- Infix version of 'index'.
--
(!) :: Representable f => f a -> Rep f -> a
(!) = index

-- | Index a behavior, specification of '!' (and 'index').
(!^) :: Behavior a -> Time -> a
(!^) = (!)

tabulated :: Representable f => Iso (Rep f -> a) (Rep f -> b) (f a) (f b)
tabulated = iso tabulate index



















-- Moving and scaling things

-- FIXME compare with diagrams variant
-- translation vs linear etc

-- |
-- Class of values that can be transformed (i.e. scaled and moved) in time.
--
-- In theory this could be generalized to arbitrary affine transformations.
--
-- Law
--
-- > transform s . transform t = transform (s <> t)
--
-- Law
--
-- > onset (delay n a)      = n ^+. onset a
-- > offset (delay n a)     = n ^+. offset a
-- > duration (stretch n a) = n ^* (duration a)
--
-- Lemma
--
-- > duration a = duration (delay n a)
--
class Transformable a where
  transform :: Span -> a -> a

-- TODO remove this one (or at least implement Score without it)
-- maybe change it to transform both components?
instance Transformable a => Transformable (a, b) where
  transform t (s,a) = (transform t s, a)

-- FIXME strange
-- transformInv (view delta -> (t,d)) = stretch (recip d) . delay' (reflectThrough 0 t)

-- FIXME get rid of this
delay' :: Transformable a => Time -> a -> a
delay' t   = delay (t .-. 0)


-- |
-- A transformation that moves a value forward in time.
--
delaying :: Duration -> Span
delaying x = (0 .+^ x) >-> 1

-- |
-- A transformation that stretches (augments) a value by the given factor.
--
stretching :: Duration -> Span
stretching x = 0 >-> x

-- |
-- A transformation that moves a value backward in time.
--
undelaying :: Duration -> Span
undelaying x = delaying (negate x)

-- |
-- A transformation that compresses (diminishes) a value by the given factor.
--
compressing :: Duration -> Span
compressing x = stretching (recip x)

-- |
-- Moves a value forward in time.
--
-- > onset (delay n x)  = n ^+. onset x
-- > offset (delay n x) = n ^+. offset x
--
-- > delay n b ! t == b ! (t .-^ n)
--
delay :: Transformable a => Duration -> a -> a
delay = transform . delaying

-- |
-- Moves a value backward in time. Equivalent to @'stretch' . 'negate'@.
--
-- > onset (undelay n x) = n - onset x
-- > offset (undelay n x) = n - offset x
--
-- > undelay n b ! t == b ! (t .+^ n)
--
undelay :: Transformable a => Duration -> a -> a
undelay = transform . undelaying

-- |
-- Stretches (augments) a value by the given factor.
--
-- > duration (stretch n a) = n * (duration a)
--
stretch :: Transformable a => Duration -> a -> a
stretch = transform . stretching

-- |
-- Compresses (diminishes) a score. Equivalent to @'stretch' . 'recip'@.
--
-- > duration (compress n a) = (duration a) / n
--
compress :: Transformable a => Duration -> a -> a
compress = transform . compressing

-- Fitting things

-- Things with a duration

-- |
-- Class of values that have a duration.
--
-- Law Duration
--
-- > duration x = (offset x .-. onset x)
--
class HasDuration a where
  _duration :: a -> Duration

-- |
-- Access the duration.
--
duration :: (Transformable a, HasDuration a) => Lens' a Duration
duration = lens _duration (flip stretchTo)

-- |
-- Stretch a value to have the given duration.
--
stretchTo :: (Transformable a, HasDuration a) => Duration -> a -> a
stretchTo d x = (d ^/ _duration x) `stretch` x

-- |
-- Access the duration.
--
normalizeDuration = stretchTo 1

-- stretchNorm :: (Transformable a, HasDuration a, InnerSpace Duration) => a -> a
-- stretchNorm x = stretchTo (normalized $ duration x) x


-- Placing things

-- |
-- Class of values that have a position in time.
--
-- Many values such as notes, envelopes etc can in fact have many positions such as
-- onset, maxPoint, offset, decay time etc. Rather than having separate classes for
-- a discrete set of cases, this class provides an interpolation from a /local/
-- position to a /global/ position. While the local position goes from 0 to 1,
-- the global position goes from 'onset' to 'offset'.
--
-- For instantaneous values, a suitable instance is:
-- 
-- @
-- '_position' x = 'const' t
-- @
--
-- For values with an onset and offset you can use 'alerp':
--
-- @
-- '_position' x = 'alerp' 'onset' 'offset'
-- @
--
class HasPosition a where
  _position :: a -> {-Scalar-} Duration -> Time

_era :: HasPosition a => a -> Span
_era x = _onset x <-> _offset x

-- |
-- Return the onset of the given value.
--
-- In an 'Envelope', this is the value between the attack and decay phases.
--
_onset :: (HasPosition a{-, Fractional s, s ~ (Scalar (Duration))-}) => a -> Time
_onset     = (`_position` 0)

-- |
-- Return the offset of the given value.
--
-- In an 'Envelope', this is the value between the sustain and release phases.
--
_offset :: (HasPosition a{-, Fractional s, s ~ (Scalar (Duration))-}) => a -> Time
_offset    = (`_position` 1.0)

-- |
-- Onset of the given value.
--
onset :: (HasPosition a, Transformable a) => Lens' a Time
onset = lens (`_position` 0) (flip $ placeAt 0)

-- |
-- Onset of the given value.
--
offset :: (HasPosition a, Transformable a) => Lens' a Time
offset = lens (`_position` 1) (flip $ placeAt 1)

-- |
-- Onset of the given value.
--
preOnset :: (HasPosition a, Transformable a) => Lens' a Time
preOnset = lens _preOnset (flip $ placeAt 1)

-- |
-- Onset of the given value.
--
postOnset :: (HasPosition a, Transformable a) => Lens' a Time
postOnset = lens _postOnset (flip $ placeAt 1)

-- |
-- Onset of the given value.
--
postOffset :: (HasPosition a, Transformable a) => Lens' a Time
postOffset = lens _postOffset (flip $ placeAt 1)


-- |
-- Return the pre-onset of the given value.
--
-- In an 'Envelope', this is the value right before the attack phase.
--
_preOnset :: (HasPosition a{-, Fractional s, s ~ (Scalar (Duration))-}) => a -> Time
_preOnset  = (`_position` (-0.5))

-- |
-- Return the post-onset of the given value.
--
-- In an 'Envelope', this is the value between the decay and sustain phases.
--
_postOnset :: (HasPosition a{-, Fractional s, s ~ (Scalar (Duration))-}) => a -> Time
_postOnset   = (`_position` 0.5)

-- |
-- Return the post-offset of the given value.
--
-- In an 'Envelope', this is the value right after the release phase.
--
_postOffset :: (HasPosition a{-, Fractional s, s ~ (Scalar (Duration))-}) => a -> Time
_postOffset  = (`_position` 1.5)



-- |
-- Move a value forward in time.
--
startAt :: (Transformable a, HasPosition a) => Time -> a -> a
startAt t x   = (t .-. _onset x) `delay` x

-- |
-- Move a value forward in time.
--
stopAt  :: (Transformable a, HasPosition a) => Time -> a -> a
stopAt t  x   = (t .-. _offset x) `delay` x

-- |
-- Align a value to a given position.
--
-- @placeAt p t@ places the given thing so that its position p is at time t
--
-- @
-- 'placeAt' 0 == 'startAt'
-- 'placeAt' 1 == 'stopAt'
-- @
--
placeAt :: (Transformable a, HasPosition a) => Duration -> Time -> a -> a
placeAt p t x = (t .-. x `_position` p) `delay` x

-- |
-- Place a value over the given span.
--
-- @placeAt s t@ places the given thing so that @x^.place == s@
--
_placeAt :: (HasPosition a, Transformable a) => Span -> a -> a
_placeAt s x = transform (s ^-^ (view era) x) x

-- |
-- A lens to the position
--
era :: (HasPosition a, Transformable a) => Lens' a Span
era = lens _era (flip _placeAt)

-- *TimeTypes> (transform ((3 <-> 4) ^-^ (4 <-> 4.5)) (4 <-> 4.5))^.range
-- (3,4)


-- |
-- @a \`lead\` b@  moves a so that @offset a' == onset b@
--
lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
a `lead` b   = placeAt 1 (b `_position` 0) a

-- |
-- @a \`follow\` b@  moves b so that @offset a  == onset b'@
--
follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
a `follow` b = placeAt 0 (a `_position` 1) b

-- |
--
after :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `after` b =  a <> (a `follow` b)

-- |
--
before :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `before` b =  (a `lead` b) <> b

pinned :: (HasPosition a, Transformable a) => (a -> a) -> a -> a
pinned f x = startAt (_onset x) (f x)

-- |
-- Compose a list of sequential objects, with onset and offset tangent to one another.
--
-- For non-positioned types, this is the often same as 'mconcat'
-- For positioned types, this is the same as 'afterAnother'
--
scat = Prelude.foldr (|>) mempty

-- |
-- Compose a list of parallel objects, so that their local origins align.
--
-- This not possible for non-positioned types, as they have no notion of an origin.
-- For positioned types this is the same as 'mconcat'.
--
pcat = Prelude.foldr (<>) mempty

during :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
y `during`  x = _placeAt (_era x) y
x `sustain` y   = x <> y `during` x

times n   = scat . replicate n

-- |
-- Class of values that can be split.
--
-- For non-positioned values such as 'Stretched', split cuts a value into pieces a piece of the given duration and the rest.
--
-- For positioned values succh as 'Note', split cuts a value relative to its onset.
--
-- XXX what about Behavior (infinite span)
--
-- Law
--
-- > let (a, b) = split x in duration a + duration b = duration x
--
class HasDuration a => Splittable a where
  split  :: Duration -> a -> (a, a)



takeM, dropM :: Splittable a => Duration -> a -> a

takeM t = fst . split t
dropM t = snd . split t



-- |
-- Class of values that can be reversed.
--
class Reversible a where

  -- | Reverse the given value.
  rev :: a -> a

instance Reversible () where
  rev = id

instance Reversible Int where
  rev = id

instance Reversible Double where
  rev = id

instance Reversible Integer where
  rev = id

instance Reversible a => Reversible [a] where
  rev = reverse . fmap rev

instance Reversible Duration where
  rev = stretch (-1)

instance Reversible Time where
  rev = stretch (-1)

instance Reversible Span where
  rev = stretch (-1)

instance Reversible a => Reversible (a, b) where
  rev (s,a) = (rev s, a)

-- |
-- View the reverse of a value.
--
reversed :: Reversible a => Iso' a a
reversed = iso rev rev



class Sequential a where
  (|>) :: a -> a -> a
  (<|) :: a -> a -> a

instance Sequential (Voice a) where
  (|>) = (<>)
  (<|) = flip (<>)

instance Sequential (Voices a) where
  (|>) = (<>)
  (<|) = flip (<>)

instance Sequential (Score a) where
  (|>) = after
  (<|) = before





















-- |
-- Internal time representation. Can be anything with Fractional and RealFrac instances.
--
type TimeBase = Rational

-- |
-- Duration, corresponding to note values in standard notation.
-- The standard names can be used: @1\/2@ for half note @1\/4@ for a quarter note and so on.
--
-- Duration is a one-dimensional 'VectorSpace', and is the associated vector space of time points.
-- It is a also an 'AdditiveGroup' (and hence also 'Monoid' and 'Semigroup') under addition.
--
-- 'Duration' is invariant under translation so 'delay' has no effect on it.
--
newtype Duration = Duration { getDuration :: TimeBase }
  deriving (Eq, Ord, Num, Enum, Fractional, Real, RealFrac, Typeable)

instance Show Duration where
  show = showRatio . realToFrac

instance InnerSpace Duration

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
  mempty  = 1 -- TODO use some notion of norm
  mappend = (*^)

instance Transformable Duration where
  Span (_, d1) `transform` d2 = d1 * d2

instance HasDuration Duration where
  _duration = id

-- |
-- Time points, representing duration since some known reference time, typically the start
-- of the music. Note that time can be negative, representing events occuring before the
-- reference time.
--
-- Time forms an affine space with durations as the underlying vector space, that is, we
-- can add a time to a duration to get a new time using '.+^', take the difference of two
-- times to get a duration using '.-.'. 'Time' forms an 'AffineSpace' with 'Duration' as
-- difference space.
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

instance Transformable Time where
  Span (t1, d1) `transform` t2 = t1 ^+^ d1 *^ t2

instance HasPosition Time where
  _position = const


-- |
-- A 'Span' represents two points in time @u@ and @v@ or, equivalently, a time @t@ and a
-- duration @d@. A third way of looking at 'Span' is that it represents a time
-- transformation where onset is translation and duration is scaling.
--
-- You can create a span using the '<->' and '>->' constructors. Note that:
--
-- > t <-> u = t >-> (u .-. t)
-- > t >-> d = t <-> t .+^ d
--
-- To create and destruct a span (in any of its incarnations), use the provided isomorphisms:
--
-- 'Span' is a 'Semigroup', 'Monoid' and 'AdditiveGroup':
--
-- - To convert a span to a pair, use @s^.'range'@.
--
-- - To construct a span from a pair, use @(t, u)^.'from' 'range'@.
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
-- With the @ViewPatterns@ extension you can pattern match over spans using
-- 
-- @
-- foo (view range -> (u,v)) = ...
-- @
--
newtype Span = Span { getDelta :: (Time, Duration) }
  deriving (Eq, Ord, Typeable)


instance Show Span where
  -- show (view range -> (t,u)) = show t ++ "<->" ++ show u
  show (view delta -> (t,d)) = show t ++ ">->" ++ show d

instance HasPosition Span where
  _position (view range -> (t1, t2)) = alerp t1 t2

instance HasDuration Span where
  _duration = snd . view delta

instance Transformable Span where
  transform = (<>)

instance Splittable Span where
  -- XXX

-- |
-- 'zeroV' or 'mempty' represents the /unit interval/ @0 \<-\> 1@, which also happens to
-- be the identity transformation.
--
instance Semigroup Span where
  (<>) = (^+^)

-- |
-- '<>' or '^+^' composes transformations, that is the scaling component is composed
-- using the '<>' instance for 'Duration', and the translation component is composed
-- using the '<>' instance for 'Time'.
--
instance Monoid Span where
  mempty  = zeroV
  mappend = (^+^)

-- |
-- 'negateV' negates time and duration using their respective 'negateV' instances.
--
instance AdditiveGroup Span where
  zeroV   = 0 <-> 1
  Span (t1, d1) ^+^ Span (t2, d2) = Span (t1 ^+^ d1 *^ t2, d1*d2)
  negateV (Span (t, d)) = Span (-t ^/ d, recip d)

durationToSpan d = (0 >-> d)
timeToSpan t = (t >-> 1)

-- |
-- @t \<-\> u@ represents the span between @t@ and @u@.
--
(<->) :: Time -> Time -> Span
t <-> u = t >-> (u .-. t)

-- |
-- @t >-> d@ represents the span between @t@ and @t .+^ d@.
--
(>->) :: Time -> Duration -> Span
t >-> d = Span (t, d)

-- > (<->) = curry $ view $ from range
-- > (>->) = curry $ view $ from delta

-- |
-- View a span as pair of onset and offset.
--
range :: Iso' Span (Time, Time)
range = iso getRange $ uncurry (<->) where getRange x = let (t, d) = getDelta x in (t, t .+^ d)

-- |
-- View a span as a pair of onset and duration.
--
delta :: Iso' Span (Time, Duration)
delta = iso getDelta $ uncurry (>->)

-- - To convert a span to a pair, use @s^.'delta'@.
--
-- - To construct a span from a pair, use @(t, d)^.'from' 'delta'@.
--


-- > forall s . id `under` s = id
-- > forall s . return `underM` s = return
-- > forall s . extract `underW` s = extract

-- |
-- Apply a function under transformation.
--
-- Designed to be used infix, as in
-- 
-- @
-- 'stretch' 2 ``under`` 'delaying' 2
-- @
--
under :: (Transformable a, Transformable b) => (a -> b) -> Span -> a -> b
f `under` s = transform s . f . transform (negateV s)

instance (Transformable a, Transformable b) => Transformable (a -> b) where
    transform = flip under

-- |
-- Apply a morphism under transformation (monadic version).
--
-- 
-- @
-- ('stretch' 2 . 'pure') ``underM`` 'delaying' 2
-- @
--
underM :: (Functor f, Transformable a, Transformable b) => (a -> f b) -> Span -> a -> f b
f `underM` s = fmap (transform s) . f . transform (negateV s)

-- |
-- Apply a morphism under transformation (co-monadic version).
--
-- 
-- @
-- ('extract' . 'stretch' 2) ``underW`` 'delaying' 2
-- @
--
underW :: (Functor f, Transformable a, Transformable b) => (f a -> b) -> Span -> f a -> b
f `underW` s = transform s . f . fmap (transform (negateV s))

-- |
-- Apply a function under transformation.
--
underDelay :: (Transformable a, Transformable b) => (a -> b) -> Time -> a -> b
underDelay     = flip (flip under . delaying . (.-. 0))

-- |
-- Apply a function under transformation.
--
underStretch :: (Transformable a, Transformable b) => (a -> b) -> Duration -> a -> b
underStretch = flip (flip under . stretching)

underL :: (Transformable a, Transformable b) => Traversal s t a b -> Traversal (Span,s) (Span,t) a b
underL l f (s,a) = (s,) <$> (l $ f `underM` negateV s) a

conjugate :: Span -> Span -> Span
conjugate t1 t2  = negateV t1 <> t2 <> t1


-- |
-- Whether the given point falls inside the given span.
--
-- Designed to be used infix, as in
--
-- @
-- t `inside`` (1 '<->' 2)
-- @
--
inside :: Time -> Span -> Bool
inside x (view range -> (t, u)) = t <= x && x <= u

-- |
-- Global start time.
--
start :: Time
start = 0

-- |
-- Global end time.
--
stop :: Time
stop  = 1

-- |
-- Global start time.
--
unit :: Duration
unit  = 1


{-
  (\x -> (transl x, matrixRep x)) ((inv $ translation 2 <> scaling 2) :: Transformation Double)
-}

{-
  -- | Invert a transformation.
  inv :: HasLinearMap v => Transformation v -> Transformation v
  inv (Transformation t t' v) = Transformation (linv t) (linv t')
                         (negateV (lapp (linv t) v))

  -- | Get the transpose of a transformation (ignoring the translation
  --   component).
  transp :: Transformation v -> (v :-: v)
  transp (Transformation _ t' _) = t'

  -- | Get the translational component of a transformation.
  transl :: Transformation v -> v
  transl (Transformation _ _ v) = v

  -- | Transformations are closed under composition; @t1 <> t2@ is the
  --   transformation which performs first @t2@, then @t1@.
  instance HasLinearMap v => Semigroup (Transformation v) where
    Transformation t1 t1' v1 <> Transformation t2 t2' v2
    = Transformation (t1 <> t2) (t2' <> t1') (v1 ^+^ lapp t1 v2)
-}

-- |
-- Pitch type.
--
type family Pitch             (s :: *) :: * -- Pitch s   = a

-- |
-- Pitch type.
--
type family SetPitch (b :: *) (s :: *) :: * -- Pitch b s = t

-- |
-- Associated interval type.
--
type Interval a = Diff (Pitch a)

-- class Has s t a b |
--   s -> a,
--   -- t -> b,
--   s b -> t,
--   -- t a -> s

-- type Lens      s t a b = forall f. Functor f     => (a -> f b) -> s -> f t
-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- |
-- Class of types that provide a single pitch.
--
class (Transformable (Pitch s), Transformable (Pitch t), 
      SetPitch (Pitch t) s ~ t) => HasPitch s t where

  -- |
  -- Pitch type.
  --
  pitch :: Lens s t (Pitch s) (Pitch t)

-- |
-- Pitch type.
--
pitch' :: (HasPitch s t, s ~ t) => Lens' s (Pitch s)
pitch' = pitch

-- |
-- Class of types that provide a pitch traversal.
--
class (Transformable (Pitch s), Transformable (Pitch t), SetPitch (Pitch t) s ~ t) => HasPitches s t where

  -- |
  -- Pitch type.
  --
  pitches :: Traversal s t (Pitch s) (Pitch t)

-- |
-- Pitch type.
--
pitches' :: (HasPitches s t, s ~ t) => Traversal' s (Pitch s)
pitches' = pitches

type instance Pitch Bool = Bool
type instance SetPitch a Bool = a
instance HasPitch Bool Bool where
  pitch = ($)
instance HasPitches Bool Bool where
  pitches = ($)

type instance Pitch Int = Int
type instance SetPitch a Int = a
instance (Transformable a, a ~ Pitch a) => HasPitch Int a where
  pitch = ($)
instance HasPitches Int Int where
  pitches = ($)

type instance Pitch Integer = Integer
type instance SetPitch a Integer = a
instance HasPitch Integer Integer where
  pitch = ($)
instance HasPitches Integer Integer where
  pitches = ($)

type instance Pitch Float = Float
type instance SetPitch a Float = a
instance (Transformable a, a ~ Pitch a) => HasPitch Float a where
  pitch = ($)
instance (Transformable a, a ~ Pitch a) => HasPitches Float a where
  pitches = ($)

type instance Pitch Double = Double
type instance SetPitch a Double = a
instance HasPitch Double Double where
  pitch = ($)
instance HasPitches Double Double where
  pitches = ($)


type instance Pitch (c,a) = Pitch a
type instance SetPitch b (c,a) = (c,SetPitch b a)

instance HasPitch a b => HasPitch (c, a) (c, b) where
  pitch = _2 . pitch

instance HasPitches a b => HasPitches (c, a) (c, b) where
  pitches = traverse . pitches


type instance Pitch [a] = Pitch a

type instance SetPitch b [a] = [SetPitch b a]

instance HasPitches a b => HasPitches [a] [b] where
  pitches = traverse . pitches


type instance Pitch (Note a) = Pitch a
type instance SetPitch g (Note a) = Note (SetPitch g a)

type instance Pitch (Note a) = Pitch a

instance (HasPitch a b) => HasPitch (Note a) (Note b) where
  pitch = _Wrapped . pl
    where
      pl f (s,a) = (s,) <$> (pitch $ f `underM` negateV s) a

instance (HasPitches a b) => HasPitches (Note a) (Note b) where
  pitches = _Wrapped . pl
    where
      pl f (s,a) = (s,) <$> (pitches $ f `underM` negateV s) a


-- |
-- Class of types that can be transposed.
--
type Transposable a = (HasPitches a a, VectorSpace (Interval a), AffineSpace (Pitch a), IsInterval (Interval a), IsPitch (Pitch a))

-- |
-- Transpose up.
--
up :: Transposable a => Interval a -> a -> a
up a = pitches %~ (.+^ a)

-- |
-- Transpose down.
--
down :: Transposable a => Interval a -> a -> a
down a = pitches %~ (.-^ a)

-- |
-- Add the given interval above.
--
above :: (Semigroup a, Transposable a) => Interval a -> a -> a
above a x = x <> up a x

-- |
-- Add the given interval below.
--
below :: (Semigroup a, Transposable a) => Interval a -> a -> a
below a x = x <> down a x

-- |
-- Invert pitches.
--
inv :: Transposable a => Pitch a -> a -> a
inv p = pitches %~ (reflectThrough p)

-- |
-- Transpose up by the given number of octaves.
--
octavesUp :: Transposable a => Scalar (Interval a) -> a -> a
octavesUp a     = up (_P8^*a)

-- |
-- Transpose down by the given number of octaves.
--
octavesDown :: Transposable a => Scalar (Interval a) -> a -> a
octavesDown a   = down (_P8^*a)













-- |
-- Dynamics type.
--
type family Dynamic             (s :: *) :: * -- Dynamic s   = a

-- |
-- Dynamic type.
--
type family SetDynamic (b :: *) (s :: *) :: * -- Dynamic b s = t


-- class Has s t a b |
--   s -> a,
--   -- t -> b,
--   s b -> t,
--   -- t a -> s

-- type Lens      s t a b = forall f. Functor f     => (a -> f b) -> s -> f t
-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- |
-- Class of types that provide a single dynamic.
--
class (Transformable (Dynamic s), Transformable (Dynamic t), SetDynamic (Dynamic t) s ~ t) => HasDynamic s t where

  -- |
  -- Dynamic type.
  --
  dynamic :: Lens s t (Dynamic s) (Dynamic t)

-- |
-- Dynamic type.
--
dynamic' :: (HasDynamic s t, s ~ t) => Lens' s (Dynamic s)
dynamic' = dynamic

-- |
-- Class of types that provide a dynamic traversal.
--
class (Transformable (Dynamic s), Transformable (Dynamic t), SetDynamic (Dynamic t) s ~ t) => HasDynamics s t where

  -- |
  -- Dynamic type.
  --
  dynamics :: Traversal s t (Dynamic s) (Dynamic t)

-- XXX we should give the lens in the class another name
-- and use the good name for an alias like this to get
-- better :info prints in GHCI
dynamics2 :: HasDynamics s t => Traversal s t (Dynamic s) (Dynamic t)
dynamics2 = dynamics

-- |
-- Dynamic type.
--
dynamics' :: (HasDynamics s t, s ~ t) => Traversal' s (Dynamic s)
dynamics' = dynamics

type instance Dynamic Bool = Bool
type instance SetDynamic a Bool = a

instance HasDynamic Bool Bool where
  dynamic = ($)

instance HasDynamics Bool Bool where
  dynamics = ($)


type instance Dynamic Int = Int
type instance SetDynamic a Int = a

instance HasDynamic Int Int where
  dynamic = ($)

instance HasDynamics Int Int where
  dynamics = ($)


type instance Dynamic Integer = Integer
type instance SetDynamic a Integer = a

instance HasDynamic Integer Integer where
  dynamic = ($)

instance HasDynamics Integer Integer where
  dynamics = ($)

type instance Dynamic Float = Float
type instance SetDynamic a Float = a

instance HasDynamic Float Float where
  dynamic = ($)

instance HasDynamics Float Float where
  dynamics = ($)


type instance Dynamic (c,a) = Dynamic a
type instance SetDynamic b (c,a) = (c,SetDynamic b a)

instance HasDynamic a b => HasDynamic (c, a) (c, b) where
  dynamic = _2 . dynamic

instance HasDynamics a b => HasDynamics (c, a) (c, b) where
  dynamics = traverse . dynamics


type instance Dynamic [a] = Dynamic a
type instance SetDynamic b [a] = [SetDynamic b a]

instance HasDynamics a b => HasDynamics [a] [b] where
  dynamics = traverse . dynamics


type instance Dynamic (Note a) = Dynamic a
type instance SetDynamic g (Note a) = Note (SetDynamic g a)

type instance Dynamic (Note a) = Dynamic a

instance HasDynamic a b => HasDynamic (Note a) (Note b) where
  dynamic = _Wrapped . pl
    where
      pl f (s,a) = (s,) <$> (dynamic $ f `underM` negateV s) a

instance HasDynamics a b => HasDynamics (Note a) (Note b) where
  dynamics = _Wrapped . pl
    where
      pl f (s,a) = (s,) <$> (dynamics $ f `underM` negateV s) a












-- |
-- Articulations type.
--
type family Articulation             (s :: *) :: * -- Articulation s   = a

-- |
-- Articulation type.
--
type family SetArticulation (b :: *) (s :: *) :: * -- Articulation b s = t


-- class Has s t a b |
--   s -> a,
--   -- t -> b,
--   s b -> t,
--   -- t a -> s

-- type Lens      s t a b = forall f. Functor f     => (a -> f b) -> s -> f t
-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- |
-- Class of types that provide a single articulation.
--
class (Transformable (Articulation s), Transformable (Articulation t), SetArticulation (Articulation t) s ~ t) => HasArticulation s t where

  -- |
  -- Articulation type.
  --
  articulation :: Lens s t (Articulation s) (Articulation t)

-- |
-- Articulation type.
--
articulation' :: (HasArticulation s t, s ~ t) => Lens' s (Articulation s)
articulation' = articulation

-- |
-- Class of types that provide a articulation traversal.
--
class (Transformable (Articulation s), Transformable (Articulation t), SetArticulation (Articulation t) s ~ t) => HasArticulations s t where

  -- |
  -- Articulation type.
  --
  articulations :: Traversal s t (Articulation s) (Articulation t)

-- |
-- Articulation type.
--
articulations' :: (HasArticulations s t, s ~ t) => Traversal' s (Articulation s)
articulations' = articulations

type instance Articulation Bool = Bool
type instance SetArticulation a Bool = a
instance HasArticulation Bool Bool where
  articulation = ($)
instance HasArticulations Bool Bool where
  articulations = ($)

type instance Articulation Int = Int
type instance SetArticulation a Int = a
instance HasArticulation Int Int where
  articulation = ($)
instance HasArticulations Int Int where
  articulations = ($)

type instance Articulation Integer = Integer
type instance SetArticulation a Integer = a
instance HasArticulation Integer Integer where
  articulation = ($)
instance HasArticulations Integer Integer where
  articulations = ($)

type instance Articulation Float = Float
type instance SetArticulation a Float = a
instance HasArticulation Float Float where
  articulation = ($)
instance HasArticulations Float Float where
  articulations = ($)


type instance Articulation (c,a) = Articulation a
type instance SetArticulation b (c,a) = (c,SetArticulation b a)

instance HasArticulation a b => HasArticulation (c, a) (c, b) where
  articulation = _2 . articulation

instance HasArticulations a b => HasArticulations (c, a) (c, b) where
  articulations = traverse . articulations


type instance Articulation [a] = Articulation a
type instance SetArticulation b [a] = [SetArticulation b a]

instance HasArticulations a b => HasArticulations [a] [b] where
  articulations = traverse . articulations


type instance Articulation (Note a) = Articulation a
type instance SetArticulation g (Note a) = Note (SetArticulation g a)

instance (HasArticulation a b) => HasArticulation (Note a) (Note b) where
  articulation = _Wrapped . pl
    where
      pl f (s,a) = (s,) <$> (articulation $ f `underM` negateV s) a

instance (HasArticulations a b) => HasArticulations (Note a) (Note b) where
  articulations = _Wrapped . pl
    where
      pl f (s,a) = (s,) <$> (articulations $ f `underM` negateV s) a














-- |
-- Parts type.
--
type family Part             (s :: *) :: * -- Part s   = a

-- |
-- Part type.
--
type family SetPart (b :: *) (s :: *) :: * -- Part b s = t


-- class Has s t a b |
--   s -> a,
--   -- t -> b,
--   s b -> t,
--   -- t a -> s

-- type Lens      s t a b = forall f. Functor f     => (a -> f b) -> s -> f t
-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- |
-- Class of types that provide a single part.
--
class (Transformable (Part s), Transformable (Part t), SetPart (Part t) s ~ t) => HasPart s t where

  -- |
  -- Part type.
  --
  part :: Lens s t (Part s) (Part t)

-- |
-- Part type.
--
part' :: (HasPart s t, s ~ t) => Lens' s (Part s)
part' = part

-- |
-- Class of types that provide a part traversal.
--
class (Transformable (Part s), Transformable (Part t), SetPart (Part t) s ~ t) => HasParts s t where

  -- |
  -- Part type.
  --
  parts :: Traversal s t (Part s) (Part t)

-- |
-- Part type.
--
parts' :: (HasParts s t, s ~ t) => Traversal' s (Part s)
parts' = parts

type instance Part Bool = Bool
type instance SetPart a Bool = a
instance HasPart Bool Bool where
  part = ($)
instance HasParts Bool Bool where
  parts = ($)

type instance Part Int = Int
type instance SetPart a Int = a
instance HasPart Int Int where
  part = ($)
instance HasParts Int Int where
  parts = ($)

type instance Part Integer = Integer
type instance SetPart a Integer = a
instance HasPart Integer Integer where
  part = ($)
instance HasParts Integer Integer where
  parts = ($)

type instance Part Float = Float
type instance SetPart a Float = a
instance HasPart Float Float where
  part = ($)
instance HasParts Float Float where
  parts = ($)

type instance Part (c,a) = Part a
type instance SetPart b (c,a) = (c,SetPart b a)

instance HasPart a b => HasPart (c, a) (c, b) where
  part = _2 . part

instance HasParts a b => HasParts (c, a) (c, b) where
  parts = traverse . parts


type instance Part [a] = Part a
type instance SetPart b [a] = [SetPart b a]

instance HasParts a b => HasParts [a] [b] where
  parts = traverse . parts


type instance Part (Note a) = Part a
type instance SetPart g (Note a) = Note (SetPart g a)

instance (HasPart a b) => HasPart (Note a) (Note b) where
  part = _Wrapped . pl
    where
      pl f (s,a) = (s,) <$> (part $ f `underM` negateV s) a

instance (HasParts a b) => HasParts (Note a) (Note b) where
  parts = _Wrapped . pl
    where
      pl f (s,a) = (s,) <$> (parts $ f `underM` negateV s) a

















through :: Applicative f => Lens' s a -> Lens s t a b -> Lens (f s) (f t) (f a) (f b)
through lens1 lens2 =
  lens getBP (flip setBP)
  -- (\sa sbt afb s -> sbt s <$> afb (sa s)) getBP (flip setBP)
  -- (\sbt afb s -> sbt s <$> afb (getBP s)) (flip setBP)
  -- (\afb s -> (flip setBP) s <$> afb (getBP s))
  -- \afb s -> (flip setBP) s <$> afb (getBP s)
  -- \afb s -> (flip $ \x a -> liftA2 (lens2 .~) x a) s <$> afb (getBP s)
  -- \afb s -> (\a x -> liftA2 (lens2 .~) x a) s <$> afb (getBP s)
  -- \afb s -> (\x -> liftA2 (lens2 .~) x s) <$> afb (getBP s)
  -- \afb s -> (\x -> liftA2 (lens2 .~) x s) <$> afb ((^. lens1) <$> s)
  -- \afb s -> (\x -> liftA2 (lens2 .~) x s) <$> afb ((\s -> s ^. lens1) <$> s)
  -- \afb s -> (\x -> liftA2 (lens2 .~) x s) <$> afb ((\s -> getConst (lens1 Const s)) <$> s)
  -- \afb s -> (\x -> liftA2 (\s -> set lens2 s) x s) <$> afb ((\s -> getConst (lens1 Const s)) <$> s)
  -- \afb s -> (\x -> liftA2 (\b ->  runIdentity . lens2 (\_ -> Identity b)) x s) <$> afb ((\s -> getConst (lens1 Const s)) <$> s)

  -- \afb s -> (\x -> liftA2 (\b ->  runIdentity . lens2 (\_ -> Identity b)) x s)
  --       <$>
  --       afb ((\s -> getConst (lens1 Const s)) <$> s)

  -- \f s -> (\x -> (\b ->  runIdentity . lens2 (const $ Identity b)) <$> x <*> s)
  --       <$>
  --       f ((\s -> getConst (lens1 Const s)) <$> s)

  -- \f s -> (\x -> liftA2 (\a b -> runIdentity $ (lens2 . const . Identity $ b) a) s x)
  --       <$>
  --       f ((getConst . lens1 Const) <$> s)

  -- \f s -> liftA2 ( \a b -> runIdentity (lens2 (const (Identity b)) a) ) s <$> (f ((getConst . lens1 Const) <$> s))
  -- \f s -> liftA2 ( \a -> runIdentity . flip lens2 a . const . Identity ) s <$> (f ((getConst . lens1 Const) <$> s))
  -- \f s -> liftA2 (\a -> runIdentity . (`lens2` a) . const . Identity) s <$> f (getConst <$> lens1 Const <$> s)
  where
    getBP a = (^. lens1) <$> a
    setBP x a = liftA2 (lens2 .~) x a

instance (Show a, Transformable a) => Show (Note a) where
  show x = show (x^.from note) ++ "^.note"
deriving instance Show a => Show (Delayed a)
deriving instance Show a => Show (Stretched a)

instance Transformable () where
  transform _ = id
instance Transformable Int where
  transform _ = id
instance Transformable Int8 where
  transform _ = id
instance Transformable Bool where
  transform _ = id
instance Transformable Float where
  transform _ = id
instance Transformable Double where
  transform _ = id
instance Transformable Integer where
  transform _ = id

-- |
-- Segments are /invariant/ under transformation.
--
-- To transform a timve varying value, use fromSegment.
--
instance Transformable (Segment a) where
  transform _ = id

-- | XXX name
fromSegment :: Monoid a => Iso (Segment a) (Segment b) (Behavior a) (Behavior b)
fromSegment = undefined

fromSegment2 :: Monoid a => Iso (Segment a) (Segment b) (Bounds (Behavior a)) (Bounds (Behavior b))
fromSegment2 = undefined








-- |
-- A 'Note' is a value with a known 'era'.
--
-- There is a morphism from 'runNote' to 'transform':
--
-- @
-- 'runNote' . 'transform' s = 'transform' s . 'runNote'
-- @
--
newtype Note a    = Note    { getNote :: (Span, a)   }

deriving instance Eq a => Eq (Note a)
deriving instance Functor Note
deriving instance Typeable1 Note
deriving instance Foldable Note
deriving instance Traversable Note
deriving instance Applicative Note
deriving instance Comonad Note

-- |
-- Note is a 'Monad' and 'Applicative' in the style of pair, with 'return' placing a value
-- at the default span 'mempty' and 'join' composing time transformations.
deriving instance Monad Note

-- |
-- A 'Delayed' value has a known 'position', but no duration.
--
newtype Delayed a   = Delayed   { getDelayed :: (Time, a)   }
  deriving (Eq, {-Ord, -}{-Show, -}Functor, Applicative, Monad, Comonad, Foldable, Traversable)

deriving instance Typeable1 Delayed

mapDelayed f (Delayed (t,x)) = Delayed (t, (f `underDelay` negateV t) x)



-- |
-- View the value in the note.
--
delayedValue :: (Transformable a, Transformable b) => Lens (Delayed a) (Delayed b) a b
delayedValue = lens runDelayed (flip $ mapDelayed . const)

-- |
-- A 'Stretched' value has a known 'position', but no duration.
--
newtype Stretched a = Stretched { getStretched :: (Duration, a) }
  deriving (Eq, {-Ord, -}{-Show, -}Functor, Applicative, Monad, Comonad, Foldable, Traversable)

deriving instance Typeable1 Stretched

mapStretched f (Stretched (d,x)) = Stretched (d, (f `underStretch` negateV d) x)

-- |
-- View the value in the note.
--
stretchedValue :: (Transformable a, Transformable b) => Lens (Stretched a) (Stretched b) a b
stretchedValue = lens runStretched (flip $ mapStretched . const)

instance Reversible (Note a) where
  rev = stretch (-1)

instance Splittable a => Splittable (Note a) where

instance Reversible (Delayed a) where

instance Reversible (Stretched a) where
  rev = stretch (-1)

instance Splittable a => Splittable (Stretched a) where


instance Wrapped (Note a) where { type Unwrapped (Note a) = (Span, a) ; _Wrapped' = iso getNote Note }
instance Wrapped (Delayed a) where { type Unwrapped (Delayed a) = (Time, a) ; _Wrapped' = iso getDelayed Delayed }
instance Wrapped (Stretched a) where { type Unwrapped (Stretched a) = (Duration, a) ; _Wrapped' = iso getStretched Stretched }
instance Rewrapped (Note a) (Note b)
instance Rewrapped (Delayed a) (Delayed b)
instance Rewrapped (Stretched a) (Stretched b)

instance Transformable (Note a) where transform t = unwrapped $ first (transform t)
instance Transformable (Delayed a) where transform t = unwrapped $ first (transform t)
instance Transformable (Stretched a) where transform t = unwrapped $ first (transform t)

instance HasDuration (Note a) where _duration = _duration . ask . unwr
instance HasDuration (Stretched a) where _duration = _duration . ask . unwr

instance HasPosition (Note a) where x `_position` p = ask (unwr x) `_position` p
instance HasPosition (Delayed a) where x `_position` p = ask (unwr x)`_position` p

-- |
-- View a note as a pair of the original value and the transformation.
--
note :: Transformable a => Iso (Span, a) (Span, b) (Note a) (Note b)
note = _Unwrapped

-- |
-- Extract the transformed value.
--
runNote :: Transformable a => Note a -> a
runNote = uncurry transform . unwr

-- |
-- Extract the transformed value.
--
-- reifyNote :: Transformable a => Note a -> Note (Span, a)
-- reifyNote = fmap (view $ from note) . duplicate

mapNote f (Note (s,x)) = Note (s, under f s x)

-- |
-- View the value in the note.
--
noteValue :: (Transformable a, Transformable b) => Lens (Note a) (Note b) a b
noteValue = lens runNote (flip $ mapNote . const)


-- |
-- View a delayed value as a pair of a the original value and a delay time.
--
delayed :: Iso' (Time, a) (Delayed a)
delayed = _Unwrapped'

-- |
-- View a stretched value as a pair of the original value and a stretch factor.
--
stretched :: Iso (Duration, a) (Duration, b) (Stretched a) (Stretched b)
stretched = _Unwrapped
-- |
-- Extract the delayed value.
--
runDelayed :: Transformable a => Delayed a -> a
runDelayed = uncurry delay' . unwr

-- |
-- Extract the stretched value.
--
runStretched :: Transformable a => Stretched a -> a
runStretched = uncurry stretch . unwr

-- instance HasPosition (Note a) where position n







-- |
-- 'Bounds' restricts the start and stop time of a value.
--
newtype Bounds a    = Bounds    { getBounds :: (Span, a)   }
  deriving (Functor, Foldable, Traversable)

-- |
-- Add bounds.
--
bounds :: Time -> Time -> a -> Bounds a
bounds t u x = Bounds (t <-> u, x)

-- trim :: (Monoid b, Keyed f, Key f ~ Time) => Bounds (f b) -> Bounds (f b)
-- trim (Bounds (s, x)) = Bounds (s, mapWithKey (\t x -> if t `inside` s then x else mempty) x)

-- |
-- Add bounds.
--
trim :: Monoid b => Bounds (Behavior b) -> Behavior b
trim = trimG

trimG :: (Applicative f, Monoid b, Representable f, Rep f ~ Time) => Bounds (f b) -> f b
trimG (Bounds (s, x)) = (tabulate $ \t x -> if t `inside` s then x else mempty) <*> x


-- mapPitch4' :: (HasPitch s s, Pitch s ~ a) => Behavior (a -> a) -> s -> s
-- mapPitch4' = mapPitch4

pureB :: a -> Behavior a
pureB = pure

mapPitch4 :: (HasPitch s t, Pitch s ~ Behavior a, Pitch t ~ Behavior b) => Behavior (a -> b) -> s -> t
mapPitch4 f = (pitch %~ (f <*>))

fix :: Transformable a => Note (Behavior a) -> Note a
-- fix = from note %~ (\(s,x) -> (s,x ! 0))
fix = fmap (! 0)

-- TODO instead of pureB and fix use

fixPitch :: (HasPitch s t, Pitch s ~ Behavior (Pitch t)) => s -> t
fixPitch = pitch %~ (!^ 0)

purePitch :: (HasPitch s t, Pitch t ~ Behavior (Pitch s)) => s -> t
purePitch = pitch %~ pure

{-
mapPitch5 :: (
  (Pitch (SetPitch (Behavior b) (SetPitch (Behavior a) s)) ~ Behavior b),
  (Pitch (SetPitch (Behavior a) s) ~ Behavior a),
  HasPitch s t, Pitch s ~ a, Pitch t ~ b) => Behavior (a -> b) -> s -> t
mapPitch5 f = fixPitch . (pitch %~ (f <*>)) . purePitch
-}



mapPitch3 :: (HasPitch s t, Pitch s ~ Behavior a, Pitch t ~ Behavior b) => Behavior (a -> b) -> s -> t
mapPitch3 f = pitch %~ (f <*>)

mapPitch2 :: (HasPitch s t, Pitch s ~ Behavior a, Pitch t ~ Behavior b) => (Behavior a -> Behavior b) -> s -> t
mapPitch2 f = pitch %~ f

mapPitch1 :: HasPitch s t => (Pitch s -> Pitch t) -> s -> t
mapPitch1 f = pitch %~ f




-- |
-- Add bounds.
--
bounded :: Lens' (Bounds (Behavior a)) (Note (Segment a))
bounded = undefined


instance Reversible (Bounds a) where
  rev = stretch (-1)
instance Splittable a => Splittable (Bounds a) where
instance Wrapped (Bounds a) where { type Unwrapped (Bounds a) = (Span, a) ; _Wrapped' = iso getBounds Bounds }
instance Rewrapped (Bounds a) (Bounds b)
instance Transformable (Bounds a) where transform t = unwrapped $ first (transform t)
instance HasDuration (Bounds a) where _duration = _duration . ask . unwr
instance HasPosition (Bounds a) where x `_position` p = ask (unwr x) `_position` p

-- TODO Compare Diagram's Trail and Located (and see the conal blog post)

-- |
--
-- A 'Segment' is a value varying over some unknown time span, semantically
--
-- > type Segment a => Duration -> a
--
-- To place a segment in a particular time span, use 'Note' 'Segment'.
--
newtype Segment a = Segment (Normalized Duration -> a) deriving (Functor, Applicative, Monad{-, Comonad-})
-- Defined 0-1

instance Semigroup a => Semigroup (Segment a) where
  (<>) = undefined

instance Monoid a => Monoid (Segment a) where

-- type instance Key Segment = Duration
--
-- instance Lookup Segment where
--   t `lookup` Segment b = b <$> t ^? normalize
--
-- instance Indexable Segment where
--   Segment b `index` t = b $ t ^?! normalize

-- Segment is a 'Monad' and 'Applicative' functor, similar to the function instance:
--
-- > pure s ! t == s
--
-- > fs <*> xs ! t == (fs ! t) (xs ! t)
--
-- > join s ! t == (s ! t) ! t
--

deriving instance Distributive Segment

instance Representable Segment where
  type Rep Segment = Duration
  -- tabulate = Behavior
  -- index (Behavior x) = x
  tabulate = error "No Representable Segment"
  index    = error "No Representable Segment"


type instance Pitch                 (Segment a) = Segment (Pitch a)
type instance SetPitch (Segment g)  (Segment a) = Segment (SetPitch g a)

instance (HasPitch a a, HasPitch a b) => HasPitch (Segment a) (Segment b) where
  pitch = through pitch pitch


-- Behavior is 'Representable':
--
-- > ask = realToFrac <$> time
-- > localRep (- t) = delay t
-- > localRep (/ t) = stretch t

-- |
--
-- A 'Behavior' is an infitately varying value, semantically
-- 
--
-- > type Behavior a => Time -> a
--
-- While a 'Behavior' can not be placed (as it has no endpoints), we can focus on a
-- certain part of a behavior by placing it inside 'Bounds'.
--
newtype Behavior a  = Behavior { getBehavior :: Time -> a }   deriving (Functor, Applicative, Monad, Comonad)
-- Defined throughout, "focused" on 0-1

deriving instance Typeable1 Behavior
deriving instance Distributive Behavior
deriving instance Semigroup a => Semigroup (Behavior a)
deriving instance Monoid a => Monoid (Behavior a)
deriving instance AdditiveGroup a => AdditiveGroup (Behavior a)
instance VectorSpace a => VectorSpace (Behavior a) where
  type Scalar (Behavior a) = Behavior (Scalar a)
  (*^) = liftA2 (*^)
instance AffineSpace a => AffineSpace (Behavior a) where
  type Diff (Behavior a) = Behavior (Diff a)
  (.-.) = liftA2 (.-.)
  (.+^) = liftA2 (.+^)
instance IsPitch a => IsPitch (Behavior a) where
  fromPitch = pure . fromPitch
instance IsInterval a => IsInterval (Behavior a) where
  fromInterval = pure . fromInterval
 
deriving instance Num a => Num (Behavior a)
deriving instance Fractional a => Fractional (Behavior a)
-- deriving instance RealFrac a => RealFrac (Behavior a)
deriving instance Floating a => Floating (Behavior a)

-- FOO3
-- TODO move
instance Eq a => Eq (b -> a) where
instance Ord a => Ord (b -> a) where
  min = liftA2 min
  max = liftA2 max
instance Real a => Real (b -> a) where
  toRational = error "No toRational for funtions"
-- instance RealFrac a => RealFrac (b -> a) where
  -- truncate = fmap truncate
  -- round = fmap round
  -- ceiling = fmap ceiling
  -- floor = fmap floor

instance Eq a => Eq (Behavior a) where
  (==) = error "No fun"

instance Ord a => Ord (Behavior a) where
  (<) = error "No fun"
  max = liftA2 max
  min = liftA2 min

instance Real a => Real (Behavior a) where
  toRational = toRational . (`index` 0)


-- FOO1

deriving instance Transformable a => Transformable (Behavior a)

instance Representable Behavior where
  type Rep Behavior = Time
  tabulate = Behavior
  index (Behavior x) = x


type instance Pitch                 (Behavior a) = Behavior (Pitch a)
type instance SetPitch (Behavior g) (Behavior a) = Behavior (SetPitch g a)

instance (HasPitch a a, HasPitch a b) => HasPitch (Behavior a) (Behavior b) where
  pitch = through pitch pitch


-- XXX is this correct?
instance (HasPitch a a, HasPitch a b) => HasPitches (Behavior a) (Behavior b) where
  pitches = through pitch pitch


type instance Dynamic                 (Behavior a) = Behavior (Dynamic a)
type instance SetDynamic (Behavior g) (Behavior a) = Behavior (SetDynamic g a)

instance (HasDynamic a a, HasDynamic a b) => HasDynamic (Behavior a) (Behavior b) where
  dynamic = through dynamic dynamic


type instance Articulation                 (Behavior a) = Behavior (Articulation a)
type instance SetArticulation (Behavior g) (Behavior a) = Behavior (SetArticulation g a)

instance (HasArticulation a a, HasArticulation a b) => HasArticulation (Behavior a) (Behavior b) where
  articulation = through articulation articulation


type instance Part                 (Behavior a) = Behavior (Part a)
type instance SetPart (Behavior g) (Behavior a) = Behavior (SetPart g a)

instance (HasPart a a, HasPart a b) => HasPart (Behavior a) (Behavior b) where
  part = through part part



-- |
-- View a behavior as a time function and vice versa. Specification of 'tabulated'.
--
behavior' :: Iso' (Time -> a) (Behavior a)
behavior' = tabulated

-- |
-- View a behavior as a time function and vice versa. Specification of 'tabulated'.
--
behavior :: Iso (Time -> a) (Time -> b) (Behavior a) (Behavior b)
behavior = tabulated

-- |
-- A behavior that
--
time' :: Behavior Time
time' = id^.behavior

-- |
-- A behavior that acts
--
time :: Fractional a => Behavior a
time = realToFrac^.behavior

-- |
-- A behavior that does something
--
ui :: Fractional a => Behavior a
ui = switch 0 0 (switch 1 time 1)

-- |
-- A behavior that
--
interval :: (Fractional a, Transformable a) => Time -> Time -> Note (Behavior a)
interval t u = (t <-> u, time)^.note

-- |
-- A behavior that
--
sine :: Floating a => Behavior a
sine = sin (time*tau)

-- |
-- A behavior that
--
cosine :: Floating a => Behavior a
cosine = cos (time*tau)

-- |
-- A behavior that
--
sawtooth :: RealFrac a => Behavior a
sawtooth = time - fmap floor' time

-- |
-- A behavior that
--
dirac :: (Num a, Bounded a) => Behavior a
dirac = switch' 0 0 maxBound 0

-- |
-- A behavior that
-- XXX name
--
turnOn  = switch 0 0 1

-- |
-- A behavior that
-- XXX name
--
turnOff = switch 0 1 0

floor' :: RealFrac a => a -> a
floor' = fromIntegral . floor
  
{-
-- | Specification of 'index'.
atTime :: Behavior a -> Time -> a
atTime = index                 
-}


deriving instance Bounded a => Bounded (Behavior a)

-- TODO move to NumInstances
instance Bounded a => Bounded (b -> a) where
  minBound = pure minBound
  maxBound = pure maxBound



-- |
-- @
-- switch t a b ! t == b
-- @
--
switch :: Time -> Behavior a -> Behavior a -> Behavior a
switch t (Behavior rx) (Behavior ry) = Behavior (\u -> if u < t then rx u else ry u)

switch' :: Time -> Behavior a -> Behavior a -> Behavior a -> Behavior a
switch' t (Behavior rx) (Behavior ry) (Behavior rz) = Behavior $ \u -> case u `compare` t of
    LT -> rx u
    EQ -> ry u
    GT -> rz u

splice :: Behavior a -> Bounds (Behavior a) -> Behavior a
splice c n = fmap (getLast . fromMaybe undefined . getOption) $ fmap (Option . Just . Last) c <> (trim . (fmap.fmap) (Option . Just . Last)) n




concatBehavior :: Monoid a => Score (Behavior a) -> Behavior a
concatBehavior = undefined

-- switch :: Time -> Reactive a -> Reactive a -> Reactive a
-- trim :: Monoid a => Span -> Reactive a -> Reactive a








type ScoreNote a = Note a

newtype Score a = Score { getScore :: [ScoreNote a] }
  deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Semigroup, Monoid)

instance Wrapped (Score a) where
  type Unwrapped (Score a) = [ScoreNote a]
  _Wrapped' = iso getScore Score

instance Rewrapped (Score a) (Score b)

instance Applicative Score where
  pure  = return
  (<*>) = ap

instance Monad Score where
  return = (^. _Unwrapped') . return . return
  xs >>= f = (^. _Unwrapped') $ mbind ((^. _Wrapped') . f) ((^. _Wrapped') xs)

instance Alternative Score where
  empty = mempty
  (<|>) = mappend

instance MonadPlus Score where
  mzero = mempty
  mplus = mappend

instance Transformable (Score a) where
  transform t (Score xs) = Score (fmap (transform t) xs)

instance Reversible a => Reversible (Score a) where
  rev (Score xs) = Score (fmap rev xs)

instance HasPosition (Score a) where
instance HasDuration (Score a) where
instance Splittable a => Splittable (Score a) where

type instance Pitch (Score a) = Pitch a
type instance SetPitch g (Score a) = Score (SetPitch g a)

type instance Pitch (Score a) = Pitch a
instance (HasPitches a b) => HasPitches (Score a) (Score b) where
  pitches = _Wrapped . traverse . from _Unwrapped . underL pitches



type instance Part (Score a) = Part a
type instance SetPart g (Score a) = Score (SetPart g a)

instance (HasParts a b) => HasParts (Score a) (Score b) where
  parts = _Wrapped . traverse . from _Unwrapped . underL parts

type instance Dynamic (Score a) = Dynamic a
type instance SetDynamic g (Score a) = Score (SetDynamic g a)

instance HasDynamics a b => HasDynamics (Score a) (Score b) where
  dynamics = _Wrapped . traverse . from _Unwrapped . underL dynamics

type instance Articulation (Score a) = Articulation a
type instance SetArticulation g (Score a) = Score (SetArticulation g a)

instance (HasArticulations a b) => HasArticulations (Score a) (Score b) where
  articulations = _Wrapped . traverse . from _Unwrapped . underL articulations


-- | XXX indexed traversal?
score :: Traversal (Score a) (Score b) (Note a) (Note b)
score = _Wrapped . traverse

-- | Map over the events in a score.
mapWithSpan :: (Span -> a -> b) -> Score a -> Score b
mapWithSpan f = undefined

-- | Filter the events in a score.
filterWithSpan :: (Span -> a -> Bool) -> Score a -> Score a
filterWithSpan f = undefined

-- | Efficient combination of 'mapEvents' and 'filterEvents'.
mapFilterWithSpan :: (Span -> a -> Maybe b) -> Score a -> Score b
mapFilterWithSpan f = undefined

-- | Map over the events in a score.
mapEvents :: (Time -> Duration -> a -> b) -> Score a -> Score b
mapEvents f = mapWithSpan (uncurry f . view delta)

-- | Filter the events in a score.
filterEvents   :: (Time -> Duration -> a -> Bool) -> Score a -> Score a
filterEvents f = undefined

-- | Efficient combination of 'mapEvents' and 'filterEvents'.
mapFilterEvents :: (Time -> Duration -> a -> Maybe b) -> Score a -> Score b
mapFilterEvents f = undefined


-- |
-- A 'Voice' is a sequence of stretched values.
--
newtype Voice a   = Voice    { getVoice :: Seq (Stretched a)   } deriving ({-Eq, -}{-Ord, -}{-Show, -}
  Functor, Foldable, Traversable, Semigroup, Monoid)

instance Applicative Voice where
  pure  = return
  (<*>) = ap

instance Monad Voice where
  return = (^. _Unwrapped') . return . return
  -- TODO

instance Transformable (Voice a) where

instance Reversible a => Reversible (Voice a) where

instance HasDuration (Voice a) where

instance Splittable a => Splittable (Voice a) where

instance Wrapped (Voice a) where
  type Unwrapped (Voice a) = (Seq (Stretched a))
  _Wrapped' = iso getVoice Voice

instance Rewrapped (Voice a) (Voice b)

-- |
-- Voice
--
voice :: Traversal (Voice a) (Voice b) (Stretched a) (Stretched b)
voice = _Wrapped . traverse

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice :: Voice a -> Voice b -> Voice (a, b)
zipVoice = zipVoiceWith (,)

-- |
-- Join the given voices by multiplying durations and combining values using the given function.
--
zipVoiceWith :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith  = undefined

-- |
-- Join the given voices by combining durations and values using the given function.
--
dzipVoiceWith :: (Duration -> Duration -> a -> b -> (Duration, c)) -> Voice a -> Voice b -> Voice c
dzipVoiceWith = undefined

-- |
-- Merge consecutive equal note.
--
mergeEqualNotes :: Eq a => Voice a -> Voice a
mergeEqualNotes = undefined


{-
instance Cons (Voice a) (Voice b) (Stretched a) (Stretched b) where
  _Cons = prism (\(s,v) -> stretchedToVoice s <> v) $ \v -> case uncons (unwr v) of
    Just (x,xs) -> Right (x,wr xs)
    Nothing   -> Left mempty

type instance Index (Voice a) = Int
type instance IxValue (Voice a) = Stretched a
instance Ixed (Voice a) where
  ix n = _Wrapped' . ix n
-}

stretchedToVoice :: Stretched a -> Voice a
stretchedToVoice x = Voice (return x)


-- |
-- The 'Divide' and 'Voices' types represent a sequence of voices and sub-voices with possibly infinite division.
--
newtype Divide a = Divide (NonEmpty (Voice a)) deriving (Functor, Foldable, Traversable)

instance Applicative Divide where
  pure  = return
  (<*>) = ap

instance Monad Divide where
  -- TODO

instance Transformable (Divide a) where

instance Reversible a => Reversible (Divide a) where

instance HasDuration (Divide a) where

instance Splittable a => Splittable (Divide a) where

voiceList :: Iso (Divide a) (Divide b) (NonEmpty (Voices a)) (NonEmpty (Voices b))
voiceList = undefined

newtype Voices a   = Voices    { getVoices :: Seq (Stretched a)   } deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Semigroup, Monoid)

instance Applicative Voices where
  pure  = return
  (<*>) = ap

instance Monad Voices where
  -- TODO

instance Transformable (Voices a) where

instance Reversible a => Reversible (Voices a) where

instance HasDuration (Voices a) where

instance Splittable a => Splittable (Voices a) where

-- | XXX
voiceMap :: Traversal (Voices a) (Voices b) (Stretched (Either a (Divide a))) (Stretched (Either a (Divide a)))
voiceMap = undefined


concatSubVoices :: Monoid a => Voices a -> Voice a
concatSubVoices = undefined

-- | XXX only defined positively
-- Need to use alternative to voice similar to a zipper etc
type Reactive a = Voice (Segment a)

-- newinstance Functor Behavior
-- -- Distributive?
-- -- XXX potentially slow, improve by memoization/const optimization
-- instance Representable Behavior where
--   type Rep = Time
-- instance Applicative Behavior
-- instance Monad Behavior
-- instance Monoid a => Monoid (Behavior a)
--
-- newtype Track a = [(Time, a)]
--   -- XXX Start time, laziness
--   -- Functor, Monad
-- newtype Score a = [(Span, a)]
-- -- XXX Start time, laziness
--   -- Functor, Monad
--
--
-- newtype Reactive a = (Time -> (a, Duration^2))
-- -- XXX Start time, laziness
-- -- Distributive?
-- instance Representable Reactive where
--   type Rep = Time
-- instance Applicative Reactive
-- instance Monad Reactive
-- instance Monoid a => Monoid (Reactive a)

{-

-- Fre monad of ?
{-
data Score s a
  = SOne a
  | SPlus s [Score a]
-}
newtype Trans s a = Trans (s, [a]) deriving (Functor)
instance Monoid s => Monad (Trans s) where
  return = Trans . return . return
  -- TODO the usual >>=

type Score s a = Free (Trans s) a

viewScore :: Monoid s => Score s a -> [(s, a)]
viewScore x = case retract x of
  Trans (s,as) -> zip (repeat s) as


-- Free monad of (a,a)
{-
data Tree a
  = One a
  | Plus (Tree a) (Tree a)
-}
data Pair a = Pair a a deriving (Functor)
newtype MaybePair a = MaybePair (Maybe (Pair a)) deriving (Functor) -- Use compose
type Tree a = Free MaybePair a

-- CPS-version of Tree
newtype Search a = Search { getSearch :: forall r . (a -> Tree r) -> Tree r }
   -}









-- -- Monoid/Semigroup
--
--
-- -- Has... Pitch Dynamics Articulation Part Chord?? Clef Slide Tremolo Text Harmonic Meta
-- -- Has+Is ... Midi/MusicXml
-- -- Is ... Pitch Interval Dynamic
--
--
--
-- reverse
-- split
--   take
--   drop
-- duration
-- position
--   onset
--   offset
-- transform
--   delay
--   stretch
-- scat
-- pcat
--
-- -- a `lead`   b  moves a so that (offset a' == onset b)
-- -- a `follow` b  moves b so that (offset a  == onset b')
-- lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
-- follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
--


                                             















-- TODO these are examples...

-- TODO compose segments etc
adsr :: Behavior Duration
adsr = time <&> \t ->
  if t `inside` (0  <-> 0.15) then lerp 0   1   ((t .-. 0)^/0.15)   else
  if t `inside` (0.15 <-> 0.3)  then lerp 1   0.3 ((t .-. 0.15)^/0.15) else
  if t `inside` (0.3  <-> 0.65) then lerp 0.3 0.2 ((t .-. 0.3)^/0.35) else
  if t `inside` (0.65 <-> 1.0)  then lerp 0.2 0   ((t .-. 0.65)^/0.35) else
  0

toFloat :: Real a => a -> Float
toFloat = realToFrac

modulate :: Floating (Pitch a) => Behavior (Pitch a -> Pitch a)
modulate = (\t x -> x * sin (t*2*pi)) <$> time



test = openG $ drawBehavior (r*5) <> lc blue (drawBehavior (c1*5)) <> drawNote (fmap (fmap snd) nc)
  where
    -- c = 1
c1 = (sin (time/20*2*pi))


newtype PD = PD { getPD :: (Behavior Float, Behavior Float) }
instance Wrapped PD where
  type Unwrapped PD = (Behavior Float, Behavior Float)
  _Wrapped' = iso getPD PD
instance Rewrapped PD PD
instance Transformable PD where
  transform _ = id
type instance Pitch PD = Behavior Float
type instance SetPitch g PD = PD
type instance Dynamic PD = Behavior Float
type instance SetDynamic g PD = PD
instance HasPitch PD PD where
  pitch = _Wrapped . _2
instance HasDynamic PD PD where
  dynamic = _Wrapped . _1
pd :: PD
pd = PD (time, time)

drawPD pd = (lc red $ drawBehavior $ pd^.dynamic) <> (lc blue $ drawBehavior $ pd^.pitch)



a :: Behavior Float
a = time



c2 :: Behavior Float -> Behavior Float
c2  = liftA2 (*) c1

nc :: Note (Behavior (Int, Float))
nc = transform (3 >-> 5) $ return $ fmap (0,) $ fmap toFloat adsr

r :: Behavior Float
r  = fmap snd $ runNote (nc & pitch %~ c2)

drawNote :: (Real a, Renderable (Path R2) b) => Note a -> Diagram b R2
drawNote n = let
  (t,d) = view delta $ n^.era
  a = n ^?! traverse
  in drawNote' (t,d,a)
























-- Utility

showRatio :: (Integral a, Show a) => Util_Ratio.Ratio a -> String
showRatio (realToFrac -> (unRatio -> (x, 1))) = show x
showRatio (realToFrac -> (unRatio -> (x, y))) = "(" ++ show x ++ "/" ++ show y ++ ")"

unRatio :: Integral a => Util_Ratio.Ratio a -> (a, a)
unRatio x = (Util_Ratio.numerator x, Util_Ratio.denominator x)

mjoin :: (Monad m, Monad n, Functor m, Traversable n) => m (n (m (n a))) -> m (n a)
mjoin = fmap join . join . fmap T.sequence

mbind :: (Monad m, Monad n, Functor m, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
mbind = (join .) . fmap . (fmap join .) . T.mapM

-- Same as @flip const@, useful to fix the type of the first argument.
assuming :: a -> b -> b
assuming = flip const

sameType :: a -> a -> ()
sameType = undefined

-- TODO must be a better way to do this
unwrapped f = wr . f . unwr
wr   = (^. _Unwrapped')
unwr = (^. _Wrapped')



{-
-- Tests

-- sc_semigroup :: (Semigroup a, Typeable a, Eq a, Serial IO a) => a -> TestTree
-- sc_semigroup x = testGroup ("Semigroup " ++ show (typeOf x)) [
  -- testProperty "mempty <> a == a" $ \a -> mempty <> a == (a :: a)
  -- ]

newtype BadMonoid a = BadMonoid [a]
  deriving (Eq, Ord, Show, Typeable)
instance Monoid (BadMonoid a) where
  BadMonoid x `mappend` BadMonoid y = BadMonoid (y `mappend` reverse x) -- lawless
  mempty = BadMonoid []
instance Functor BadMonoid where
  fmap f (BadMonoid xs) = BadMonoid (fmap f $ reverse $ xs) -- lawless

data BadFunctor a = BF1 | BF2
  deriving (Eq, Ord, Show, Typeable)

instance Functor BadFunctor where
  fmap f BF1 = BF2 -- lawless
  fmap f BF2 = BF2

instance Monad m => CoSerial m Time where
  coseries = liftM const -- TODO?
instance Monad m => Serial m Time where
  series = msum $ fmap return [-1,0,1,2,2.13222,10]
instance Monad m => Serial m Duration where
  series = msum $ fmap return [-1,0,1,2,1.51232,10]
instance Monad m => Serial m Span where
  series = newtypeCons Span
instance (Monad m, Serial m a) =>  Serial m (BadFunctor a) where
  series = cons0 BF1 \/ cons0 BF2
instance (Monad m, Serial m a) => Serial m (BadMonoid a) where
  series = newtypeCons BadMonoid
instance Monad m => Serial m Int8 where
  series = msum $ fmap return [0..2]
instance (Monad m, Serial m a) => Serial m (Note a) where
  series = newtypeCons Note
instance (Monad m, Serial m a) => Serial m (Delayed a) where
  series = newtypeCons Delayed
instance (Monad m, Serial m a) => Serial m (Stretched a) where
  series = newtypeCons Stretched
instance (Monad m, Serial m a) => Serial m (Behavior a) where
  series = newtypeCons Behavior


-- > onset (delay n a)      = n ^+. onset a
-- > offset (delay n a)     = n ^+. offset a
-- > duration (stretch n a) = n ^* (duration a)
--
-- Lemma
--
-- > duration a = duration (delay n a)

constDurLaw :: (Show a, Typeable a, Serial IO a, HasDuration a, Transformable a) => a -> TestTree
constDurLaw typ = testGroup ("Delay and duration" ++ show (typeOf typ)) $ [
  testProperty "duration a == duration (delay n a)" $ \(n :: Duration) a -> assuming (sameType typ a) $
                _duration a == _duration (delay n a)
  ]

-- FOO2

delayBehLaw typ = testGroup ("Delay behavior" ++ show (typeOf typ)) $ [
  testProperty "delay n b ! t == b ! (t .-^ n)" $ \(n :: Duration) (t :: Time) b -> assuming (sameType typ b) $
                delay n b ! t == b ! (t .-^ n)
  ]


{-
  (t<->u) `transform` b ! t           == b ! 0
  (t<->u) `transform` b ! ((u-t)/2+t) == b ! 0.5
  (t<->u) `transform` b ! u           == b ! 1
-}

transformUi typ = testGroup ("Transform UI" ++ show (typeOf typ)) $ [
  testProperty "(t<->u) `transform` b ! t          == b ! 0" $ \(t :: Time) (u2 :: Time) -> let b = (ui::Behavior Double); u = notEqualTo t u2 in
                (t<->u) `transform` b ! t          == b ! 0,

  testProperty "(t<->u) `transform` b ! ((u-t)/2+t) == b ! 0.5" $ \(t :: Time) (u2 :: Time) -> let b = (ui::Behavior Double); u = notEqualTo t u2 in
                (t<->u) `transform` b ! ((u-t)/2+t) == b ! 0.5,

  testProperty "(t<->u) `transform` b ! u           == b ! 1" $ \(t :: Time) (u2 :: Time) -> let b = (ui::Behavior Double); u = notEqualTo t u2 in
                (t<->u) `transform` b ! u           == b ! 1

  ]
notEqualTo x y 
  | x == y    = y + 1
  | otherwise = y


-- DEBUG
instance Show (Behavior a) where
  show _ = "<<Behavior>>"

monoid :: (Monoid t, Eq t, Show t, Typeable t, Serial IO t) => t -> TestTree
monoid typ = testGroup ("instance Monoid " ++ show (typeOf typ)) $ [
  testProperty "x <> (y <> z) == (x <> y) <> z" $ \x y z -> assuming (sameType typ x)
          x <> (y <> z) == (x <> y) <> z,

  testProperty "mempty <> x == x"         $ \x   -> assuming (sameType typ x)
          mempty <> x == x,

  testProperty "x <> mempty == x"         $ \x   -> assuming (sameType typ x)
         (x <> mempty == x)
  ]
  where
    (<>) = mappend

-- functor :: (Functor f, Eq (f b), Show (f b), Typeable b, Typeable1 f, Serial IO (f b)) => f b -> TestTree
-- functor typ = testGroup ("instance Functor " ++ show (typeOf typ)) $ [
--   testProperty "fmap id = id" $ \x -> assuming (sameType typ x)
--          (fmap id x == id x)
--   ]

-- applicative :: (Applicative f, Eq (f b), Show (f b), Typeable b, Typeable1 f, Serial IO (f b)) => f b -> TestTree
-- applicative typ = testGroup ("instance Applicative " ++ show (typeOf typ)) $ [
--
--   testProperty "pure id <*> v = v" $ \x -> assuming (sameType typ x) ((pure id <*> x) == x),
--
--   testProperty "pure (.) <*> u <*> v <*> w = u <*> (v <*> w)"
--     $ \u v w -> assuming (sameType typ w)
--       ((pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w)))
--
--   ]


ap2 u v w = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

main = defaultMain $ testGroup "" $ [
  testProperty "rev . rev == id" $ \(x :: ())  -> rev (rev x) == x,
  testProperty "rev . rev == id" $ \(x :: [Int]) -> rev (rev x) == x,

  monoid (undefined :: ()),
  monoid (undefined :: Maybe ()),
  monoid (undefined :: [()]),
  -- monoid (undefined :: Behavior ()), -- too slow!

  monoid (undefined :: Time),
  monoid (undefined :: Duration),
  monoid (undefined :: Span),

  constDurLaw (undefined :: Note ()),
  constDurLaw (undefined :: Stretched ()),
  delayBehLaw (undefined :: Behavior Int8),

  transformUi (undefined :: Behavior Int8)
  -- functor (undefined :: BadFunctor Int8),
  -- functor (undefined :: BadMonoid Int8)

  ]



-}




drawScore' :: (Renderable (Path R2) b, Real a) =>     [[(Time, Duration, a)]] -> Diagram b R2
drawScore' = vcat' (def & sep .~ 2) . fmap drawPart'

drawPart' :: (Renderable (Path R2) b, Real a) =>    [(Time, Duration, a)] -> Diagram b R2
drawPart' = mconcat . fmap drawNote'

drawNote' :: (Renderable (Path R2) b, Real a) => (Time, Duration, a) -> Diagram b R2
drawNote' (realToFrac -> t, realToFrac -> d, realToFrac -> y) = translateY y $ translateX t $ scaleX d $ noteShape
  where
  noteShape = {-showOr $-} lcA transparent $ fcA (blue `withOpacity` 0.5) $ strokeLoop $ closeLine $ fromOffsets [r2 (1,0), r2 (-0.8,0.2), r2 (-0.2,0.8)]

drawBehavior :: (Renderable (Path R2) b, Real a) =>  Behavior a -> Diagram b R2
drawBehavior = drawBehavior' 50

drawSegment :: (Renderable (Path R2) b, Real a) =>  Segment a -> Diagram b R2
drawSegment = drawBehavior' 50

drawBehavior' count b = cubicSpline False points & lw 0.05
  where
    points = take (samplesPerCell*count) $ fmap (\x -> p2 (x, realToFrac $ b `index` realToFrac x)) [0,1/samplesPerCell..]
    samplesPerCell = 40

grid = grid' 20 <> fc lightblue (circle 0.1)
gridX = gridX' 20
gridY = gridY' 20

grid' ds = {-showOr $ -}moveOriginTo (p2 (realToFrac ds*(1/20),-(realToFrac ds/2))) $ (gridX <> gridY & lc lightblue)

gridY' :: (Renderable (Path R2) b) => Int -> Diagram b R2
gridY' ds = alignTL $ hcat' (def & sep .~ 1) $ replicate (ds+1) $ vrule (realToFrac ds)

gridX' :: (Renderable (Path R2) b) => Int -> Diagram b R2
gridX' ds = alignTL $ vcat' (def & sep .~ 1) $ replicate (ds+1) $ hrule (realToFrac ds)

writeG :: (a ~ SVG.SVG) => FilePath -> Diagram a R2 -> IO ()
writeG path dia = do
  let svg = renderDia SVG.SVG (SVG.SVGOptions (Height 300) Nothing) dia
  let bs  = renderSvg svg
  ByteString.writeFile path bs

openG :: (a ~ SVG.SVG) => Diagram a R2 -> IO ()
openG = openG' . (<> grid)

openG' dia = do
  writeG "test.svg" $ dia --
  -- FIXME find best reader
  system "echo '<img src=\"test.svg\"></img>' > test.html"
  -- system "open -a 'Firefox' test.html"
  system "osascript -e 'tell application \"Google Chrome\" to tell the active tab of its first window' -e 'reload' -e 'end tell'"
  return ()


