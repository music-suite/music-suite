
{-# LANGUAGE CPP                        #-}
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

#ifndef __HADDOCK__
#define INCLUDE_TESTS
#define INCLUDE_LIFTED
#endif

{-
  Music.Time.Transform
  Music.Time.Duration
  Music.Time.Position
  Music.Time.Split
  Music.Time.Reverse
  Music.Time.Juxtapose

  Music.Time.Time      \
  Music.Time.Duration   * (merge as M.T.Types?) 
  Music.Time.Span      /

  Music.Time.Stretched
  Music.Time.Delayed
  Music.Time.Note
  Music.Time.Voice
  Music.Time.Chord
  Music.Time.Score
  Music.Time.Segment
  Music.Time.Linear   ?
  Music.Time.Spline   ?
  Music.Time.Behavior
  Music.Time.Reactive
  
  Music.Score.Meta
  Music.Score.Pitch
  Music.Score.Dynamics
  Music.Score.Articulation
  Music.Score.Part
    
  Music.Score.GraceNotes
  Music.Score.Ornaments
  Music.Score.Tremolo
  Music.Score.Text
  Music.Score.Harmonics
  Music.Score.Slide
  Music.Score.Clef
  Music.Score.Repeats
-}

#ifndef __HADDOCK__
module Main (
        main,
#else
module TimeTypes (
#endif

        -- -- * Data.Clipped
        -- Clipped,
        -- -- unsafeToClipped,
        -- -- fromClipped,
        -- clipped,
        -- unclipped,

        -- * Data.Functor.Rep.Lens
        -- $dataFunctorRepLens
        (!),
        tabulated,


        -- * Music.Time.Meta
        Meta,
        HasMeta(..),

        -- * Music.Time.Transform
        -- * The Transformable class
        Transformable(..),
        -- ** Apply under a transformation
        whilst,
        on,     
        conjugate,

        -- ** Specific transformations
        delay,
        undelay,
        stretch,
        compress,
        -- *** Applied transformations
        delaying,
        undelaying,
        stretching,
        compressing,
        -- *** Utility
        delayTime,

        -- * Music.Time.Duration
        -- * The HasDuration class
        HasDuration(..),
        -- * Stretching to absolute duration
        duration,
        stretchTo,

        -- * Music.Time.Position
        -- * The HasPosition class
        HasPosition(..),
        -- * Inspecting position
        era,
        position,

        -- * Specific positions
        onset,
        offset,
        preOnset,
        postOnset,
        postOffset,

        -- * Moving to absolute positions
        startAt,
        stopAt,
        placeAt,

        -- * Music.Time.Split
        -- * The Splittable class
        Splittable(..),
        chunks,

        -- * Music.Time.Reverse
        -- * The Reversible class
        Reversible(..),
        reversed,
        revDefault,
        NoReverse(..),

        -- * Music.Time.Juxtapose
        -- * Align without composition
        lead,
        follow,
        -- * Align and compose
        after,
        before,
        during,
        sustain,
        palindrome,
        
        -- ** Composition operators
        (|>),
        (<|),

        -- ** Catenation
        scat,
        pcat,

        -- ** Repetition
        times,

        -- * Music.Time.Types
        -- * Duration
        Duration,
        toDuration,
        fromDuration,
        
        -- * Time points
        Time,
        toTime,
        fromTime,

        -- * Time spans
        Span,
        -- *** Creating spans
        (<->),
        (>->),
        (<-<),
        -- *** Accessing spans
        range,
        delta,

        -- ** Properties
        isProper,
        stretches,
        delays,
        -- Proper spans are always bounded and closed
        
        -- ** Points in spans
        inside,
        -- TODO Abjad terminology: contains/curtails/delays/intersects/isCongruentTo
        encloses,
        overlaps,
        -- union
        -- intersection (alt name 'overlap')
        -- difference (would actually become a split)

        -- ** Utility
        showRange,
        showDelta,

        -- * Music.Time.Segment
        Segment,
        -- ** Examples
        -- $musicTimeSegmentExamples
        (!.),
        segment',
        segment,
        
        -- ** Combinators
        focusing,
        appendS,
        concatS,

        -- * Music.Time.Behavior
        Behavior,
        -- ** Examples
        -- $musicTimeBehaviorExamples
        (!^),
        behavior',
        behavior,

        -- ** Combinators
        switch,
        switch',
        splice,
        trim,
        trimBefore,
        trimAfter,
        concatB,

        -- * Common behaviors
        time,
        unit,
        impulse,
        turnOn,
        turnOff,
        sawtooth,
        sine,
        cosine,

        -- * Music.Time.Reactive
        Reactive,
        initial,
        final,
        intermediate,
        discrete,
        continous,
        continousWith,
        sample,
        -- TODO
        -- window,
        -- windowed,

        -- * Music.Time.Stretched
        Stretched,
        stretched,
        getStretched,

        -- * Music.Time.Delayed
        Delayed,
        delayed,
        getDelayed,

        -- * Music.Time.Note
        Note,
        note,
        getNote,

        -- * Music.Time.Bound
        Bound,
        bounds,
        bounding,
        trim,
        splice,
        bounded',
        bounded,

{-
        -- * Music.Time.Phrase
        Phrase,
-}

        -- * Music.Time.Chord
        Chord,
        -- ** Substructure
        chord,
        -- ** TODO

        -- * Music.Time.Voice
        Voice,
        -- ** Substructure
        voice,
        -- ** Zips
        zipVoice,
        zipVoiceWith,
        dzipVoiceWith,
        mergeEqualNotes,
        
        -- mapDurations, -- ([Duration] -> [Duration]) -> Voice a -> Voice a
        -- mapPitches,   -- ([Pitch a]  -> [Pitch a])  -> Voice a -> Voice a
        -- etc

{-
        -- * Music.Time.Phrases
        Voices,
        voices',
        Phrases,
        phrases',
        concatVoices,
-}

        -- * Music.Time.Score
        Score,

        -- ** Substructure
        score,
        notes,
        voices,
        phrases,
        singleNote,
        singleVoice,
        singlePhrase,
        
        -- ** Special traversals
        mapWithSpan,
        filterWithSpan,
        mapFilterWithSpan,
        mapEvents,
        filterEvents,
        mapFilterEvents,
        
        
        -- voices -- Lens' (Score a) [Voice a]
        -- phrases -- Lens' (Voice a) [Phrase a]
        
        -- mapVoices, -- ([Voice a] -> [Voice a]) -> Score a -> Score a
        -- mapPhrases, -- ([Phrase a] -> [Phrase a]) -> Voice a -> Voice a



        -- * Music.Score.Pitch
        -- ** Pitch type functions
        Pitch,
        SetPitch,
        Interval,
        Transposable,
        -- ** Accessing pitch
        HasPitches(..),
        HasPitch(..),
        pitch',
        pitches',
        -- * Manipulating pitch
        -- ** Transposition
        up,
        down,
        above,
        below,
        invertPitches,
        octavesUp,
        octavesDown,
        octavesAbove,
        octavesBelow,
        -- ** Intervals
        augmentIntervals,
        
        -- TODO pitchIs, to write filter pitchIs ... etc        
        -- TODO gliss etc
        
        -- * Music.Score.Dynamic
        -- ** Dynamic type functions
        Dynamic,
        SetDynamic,
        -- ** Accessing dynamics
        HasDynamics(..),
        HasDynamic(..),
        dynamic',
        dynamics',
        -- * Manipulating dynamics
        Level,
        Attenuable,
        louder,
        softer,
        level,
        compressor,
        fadeIn,
        fadeOut,
        
        -- * Music.Score.Articulation
        -- ** Articulation type functions
        Articulation,
        SetArticulation,
        -- ** Accessing articulation
        HasArticulations(..),
        HasArticulation(..),
        articulation',
        articulations',
        -- * Manipulating articulation        
        accent,
        marcato,
        accentLast,
        marcatoLast,
        accentAll,
        marcatoAll,

        tenuto,
        separated,
        staccato,
        portato,
        legato,
        spiccato,
        
        -- * Music.Score.Part
        -- ** Articulation type functions
        Part,
        SetPart,
        -- ** Accessing parts
        HasParts(..),
        HasPart(..),
        part',
        parts',
        -- * Manipulating parts (TODO)
        allParts,
        extractPart,
        extractParts,     
  ) where

import Data.Fixed
import qualified Data.ByteString.Lazy         as ByteString
import           Data.Default
import           Data.Ratio
import qualified Diagrams.Backend.SVG         as SVG
import           Diagrams.Prelude             hiding (Duration, Dynamic,
                                               Segment, Time, Transformable,
                                               Product,
                                               after, atTime, clipped, duration,
                                               during, era, era, interval, inv,
                                               offset, place, position, start,
                                               stretch, stretchTo, transform,
                                               trim, trimBefore, trimAfter, 
                                               under, unit, value, view,
                                               toDuration, fromDuration, conjugate,
                                               toTime, fromTime, discrete, sample,
                                               (<->), (|>), (~~))
import           System.Process               (system)
import           Text.Blaze.Svg.Renderer.Utf8 (renderSvg)


import           Control.Applicative
import           Control.Arrow                (first, second, (***), (&&&))
import qualified Control.Category
import           Control.Comonad
import           Control.Comonad.Env
import           Control.Lens                 hiding (Indexable, Level, above,
                                               below, index, inside, parts,
                                               reversed, transform, (|>), (<|))
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Plus
import qualified Control.Comonad.Representable.Store as Store
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Distributive
import           Data.Foldable                (Foldable)
import qualified Data.Foldable                as Foldable
import           Data.Functor.Rep
import qualified Data.List
import           Data.List.NonEmpty           (NonEmpty)
import           Data.Maybe
import           Data.NumInstances
import           Data.Semigroup               hiding ()
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Traversable             (Traversable)
import qualified Data.Traversable             as T
import           Data.Typeable
import           Data.VectorSpace hiding (Sum(..))
import           Music.Dynamics.Literal
import           Music.Pitch.Literal

import           Data.Int
import           Test.SmallCheck.Series       hiding (NonEmpty, (><))
import           Test.Tasty                   hiding (over, under)
import           Test.Tasty.SmallCheck        hiding (over, under)

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

instance Eq a => Eq (b -> a) where
instance Ord a => Ord (b -> a) where
  min = liftA2 min
  max = liftA2 max
instance Real a => Real (b -> a) where
  toRational = error "No toRational for funtions"

instance IsDynamics Bool where
instance IsDynamics Float where
instance IsDynamics Int where
instance IsDynamics Integer where

instance IsInterval Float where
  fromInterval x = realToFrac (fromInterval x :: Double)
instance IsPitch Float where
  fromPitch x = realToFrac (fromPitch x :: Double)

-- TODO move to NumInstances
instance Bounded a => Bounded (b -> a) where
  minBound = pure minBound
  maxBound = pure maxBound








-- | A value in the unit interval /(0,1)/.
newtype Clipped a = UnsafeClip { unsafeGetClipped :: a }
  deriving (Eq, Ord, Show)

instance Num a => Bounded (Clipped a) where
  minBound = UnsafeClip 0
  maxBound = UnsafeClip 1

instance (Num a, Ord a) => Num (Clipped a) where
  a + b = unsafeToClipped (fromClipped a + fromClipped b)
  a - b = unsafeToClipped (fromClipped a - fromClipped b)
  a * b = unsafeToClipped (fromClipped a * fromClipped b)
  abs   = id
  signum 0 = 0
  signum _ = 1
  negate = error "negate: No instance for Clipped"
  fromInteger = unsafeToClipped . fromInteger

instance (Num a, Ord a, Fractional a) => Fractional (Clipped a) where
  a / b = unsafeToClipped (fromClipped a / fromClipped b)
  recip 1 = 1
  recip _ = error "Can not take reciprocal of a clipped value other than 1"
  fromRational = unsafeToClipped . fromRational

unsafeToClipped   = fromMaybe (error "Outside 0-1") . (^? clipped)
fromClipped = (^. unclipped)

clipped :: (Num a, Ord a) => Prism' a (Clipped a)
clipped = prism unsafeGetClipped $
  \x -> if 0 <= x && x <= 1
      then Right (UnsafeClip x)
      else Left x

unclipped :: (Num a, Ord a) => Getter (Clipped a) a
unclipped = re clipped

zipClippedWith
  :: (Num a, Ord a,
      Num b, Ord b,
      Num c, Ord c)
  => (a -> b -> c)
  -> Clipped a -> Clipped b -> Maybe (Clipped c)
zipClippedWith f a b = ((a^.unclipped) `f` (b^.unclipped))^? clipped

addLim = zipClippedWith (+)







-- $dataFunctorRepLens
-- Provides access to the definition in "Data.Functor.Rep" in terms of "Control.Lens".

-- |
-- Index a representable functor.
--
-- This is an infix alias for 'index'.
--
(!) :: Representable f => f a -> Rep f -> a
(!) = index

infixl 6 !

-- |
-- The isomorpism between a representable functor and its representation.
--
-- @
-- 'tabulated' = 'iso' 'tabulate' 'index'
-- @
--
tabulated :: (Representable f, Representable g) => Iso (Rep f -> a) (Rep g -> b) (f a) (g b)
tabulated = iso tabulate index
















-- * Music.Time.Meta

data Meta
-- TODO

-- | Type class for things which have meta-information.
class HasMeta a where
    -- | Apply meta-information by combining it (on the left) with the
    --   existing meta-information.
    meta :: Lens' a Meta

instance HasMeta Meta where
    meta = ($)



-- * Music.Time.Transform

-- |
-- Class of values that can be transformed (i.e. scaled and moved) in time.
--
-- Law
--
-- @
-- transform mempty = id
-- transform (s \<> t) = transform s . transform t
-- @
--
-- Law
--
-- @
-- onset (delay n a)       = n ^+. onset a
-- offset (delay n a)      = n ^+. offset a
-- duration (stretch n a)  = n * duration a
-- duration (compress n a) = duration a / n
-- @
--
-- @
-- delay n b ! t    = b ! (t .-^ n)
-- undelay n b ! t  = b ! (t .+^ n)
-- @
--
-- Lemma
--
-- @
-- duration a = duration (delay n a)
-- @
--
class Transformable a where
  transform :: Span -> a -> a

instance Transformable () where
  transform _ = id

instance Transformable Bool where
  transform _ = id

instance Transformable Ordering where
  transform _ = id

instance Transformable Char where
  transform _ = id

instance Transformable Int where
  transform _ = id

instance Transformable Integer where
  transform _ = id

instance Transformable a => Transformable (Ratio a) where
  transform _ = id

instance Transformable Float where
  transform _ = id

instance Transformable Double where
  transform _ = id

--
-- TODO
--
-- Should really transform the /second/ element, but this is incompatible with Note/SCcore
--
-- 1) Change this to transform both components
--    Then Note could be defined as   type Note a = (Span, TransfInv a)
--
-- 2) Redefine note as                type Note a = (a, Span)
--
instance Transformable a => Transformable (a, b) where
  transform t (s,a) = (transform t s, a)

-- |
-- Lists transform by transforming each element.
--
instance Transformable a => Transformable [a] where
  transform t = map (transform t)

instance Transformable a => Transformable (Seq a) where
  transform t = fmap (transform t)

-- |
-- Functions transform by conjugation, i.e. we reverse-transform the argument
-- and transform the result.
--
instance (Transformable a, Transformable b) => Transformable (a -> b) where
  transform t = (`whilst` negateV t)

-- |
-- Delay relative to 'origin'.
--
-- Provided for situations when we have a value that should forward based on the distance
-- between some time @t@ and the origin, but it does not necessarily have a start time.
--
delayTime :: Transformable a => Time -> a -> a
delayTime t = delay (t .-. 0)


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
delay :: Transformable a => Duration -> a -> a
delay = transform . delaying

-- |
-- Moves a value backward in time. Equnitvalent to @'stretch' . 'negate'@.
--
undelay :: Transformable a => Duration -> a -> a
undelay = transform . undelaying

-- |
-- Stretches (augments) a value by the given factor.
--
stretch :: Transformable a => Duration -> a -> a
stretch = transform . stretching

-- |
-- Compresses (diminishes) a score. Equnitvalent to @'stretch' . 'recip'@.
--
compress :: Transformable a => Duration -> a -> a
compress = transform . compressing

-- |
-- Class of values that have a duration.
--
-- Law Duration
--
-- @
-- '_duration' x = ('offset' x '.-.' 'onset' x)
-- @
--
class HasDuration a where
  _duration :: a -> Duration

instance HasDuration Duration where
  _duration = id

instance HasDuration Span where
  _duration = snd . view delta

--
-- By convention, we treat pairs and triplets as having the form
-- (t,x), (d,x) and (t,d,x) where t has a position and d has a 
-- duration. This makes it convenient to represent simple event
-- lists as [(Time, Duration, a)] without needing any special
-- structure.
--

instance HasDuration a => HasDuration (a, b) where
  _duration (d,_) = _duration d

instance HasDuration b => HasDuration (a, b, c) where
  _duration (_,d,_) = _duration d

instance HasDuration a => HasDuration (Product a) where
  _duration (Product x) = _duration x

instance HasDuration a => HasDuration (Sum a) where
  _duration (Sum x) = _duration x

instance HasDuration a => HasDuration (Min a) where
  _duration (Min x) = _duration x

instance HasDuration a => HasDuration (Max a) where
  _duration (Max x) = _duration x


-- |
-- Access the duration.
--
duration :: (Transformable a, HasDuration a) => Lens' a Duration
duration = lens _duration (flip stretchTo)
{-# INLINE duration #-}

-- |
-- Stretch a value to have the given duration.
--
stretchTo :: (Transformable a, HasDuration a) => Duration -> a -> a
stretchTo d x = (d ^/ _duration x) `stretch` x

-- |
-- Access the duration.
--
clippedDuration = stretchTo 1

-- stretchClipped :: (Transformable a, HasDuration a, InnerSpace Duration) => a -> a
-- stretchClipped x = stretchTo (clipped $ duration x) x


-- |
-- Class of values that have a position in time.
--
-- Many values such as notes, envelopes etc can in fact have many positions such as onset,
-- attack point, offset, decay point time etc. Rather than having separate methods for a
-- discrete set of cases, this class provides an interpolation from a /local/ position to
-- a /global/ position. While the local position goes from 0 to 1, the global position
-- goes from the 'onset' to the 'offset' of the value.
--
-- For instantaneous values, a suitable instance is:
--
-- @
-- '_position' x = 'const' t
-- @
--
-- For values with an onset and offset we can use 'alerp':
--
-- @
-- '_position' x = 'alerp' ('_onset' x) ('_offset' x)
-- @
--
class HasDuration a => HasPosition a where
  -- |
  -- Return the onset of the given value.
  --
  -- In an 'Envelope', this is the value between the attack and decay phases.
  --
  _position :: a -> Duration -> Time
  _position x = alerp (_onset x) (_offset x)

  -- |
  -- Return the onset of the given value.
  --
  -- In an 'Envelope', this is the value between the attack and decay phases.
  --
  _onset, _offset :: a -> Time
  _onset     = (`_position` 0)
  _offset    = (`_position` 1.0)

instance (HasPosition a, HasDuration a) => HasDuration [a] where
  _duration x = _offset x .-. _onset x

instance (HasPosition a, HasDuration a) => HasPosition [a] where
  _onset  = foldr min 0 . fmap _onset
  _offset = foldr max 0 . fmap _offset

_era :: HasPosition a => a -> Span
_era x = _onset x <-> _offset x

-- |
-- Position of the given value.
--
position :: (HasPosition a, Transformable a) => Duration -> Lens' a Time
position d = lens (`_position` d) (flip $ placeAt d)
{-# INLINE position #-}

-- |
-- Onset of the given value.
--
onset :: (HasPosition a, Transformable a) => Lens' a Time
onset = position 0
{-# INLINE onset #-}

-- |
-- Onset of the given value.
--
offset :: (HasPosition a, Transformable a) => Lens' a Time
offset = position 1
{-# INLINE offset #-}

-- |
-- Onset of the given value.
--
preOnset :: (HasPosition a, Transformable a) => Lens' a Time
preOnset = position (-0.5)
{-# INLINE preOnset #-}

-- |
-- Onset of the given value.
--
postOnset :: (HasPosition a, Transformable a) => Lens' a Time
postOnset = position 0.5
{-# INLINE postOnset #-}

-- |
-- Onset of the given value.
--
postOffset :: (HasPosition a, Transformable a) => Lens' a Time
postOffset = position 1.5
{-# INLINE postOffset #-}


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
-- 'placeAt' 0 = 'startAt'
-- 'placeAt' 1 = 'stopAt'
-- @
--
placeAt :: (Transformable a, HasPosition a) => Duration -> Time -> a -> a
placeAt p t x = (t .-. x `_position` p) `delay` x

-- |
-- Place a value over the given span.
--
-- @placeAt s t@ places the given thing so that @x^.place = s@
--
_placeAt :: (HasPosition a, Transformable a) => Span -> a -> a
_placeAt s x = transform (s ^-^ view era x) x

-- |
-- A lens to the position
--
era :: (HasPosition a, Transformable a) => Lens' a Span
era = lens _era (flip _placeAt)
{-# INLINE era #-}


--
-- TODO names
-- Especially 'after' is counter-intuitive
--

-- |
-- Move a value so that
--
-- @
-- '_offset' (a ``lead`` b) = '_onset' b
-- @
--
--
lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
a `lead` b   = placeAt 1 (b `_position` 0) a

-- |
-- Move a value so that
--
-- @
-- '_offset' a = '_onset' (a ``follow`` b)
-- @
--
follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
a `follow` b = placeAt 0 (a `_position` 1) b

-- |
-- Move a value so that
--
after :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `after` b =  a <> (a `follow` b)

-- |
-- Move a value so that
--
before :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `before` b =  (a `lead` b) <> b

-- |
-- A value followed by its reverse (retrograde).
--
palindrome :: (Semigroup a, Reversible a, HasPosition a) => a -> a
palindrome a = a `after` rev a

(|>) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(|>) = after

(<|) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(<|) = before

-- |
-- Compose a list of sequential objects, with onset and offset tangent to one another.
--
-- For non-positioned types, this is the often same as 'mconcat'
-- For positioned types, this is the same as 'afterAnother'
--
scat :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => [a] -> a
scat = Prelude.foldr (|>) mempty

-- |
-- Compose a list of parallel objects, so that their local origins align.
--
-- This not possible for non-positioned types, as they have no notion of an origin.
-- For positioned types this is the same as 'mconcat'.
--
pcat :: (Semigroup a, Monoid a) => [a] -> a
pcat = Prelude.foldr (<>) mempty

-- |
-- Move a value so that
--
during :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
y `during`  x = _placeAt (_era x) y

-- |
-- Move a value so that
--
sustain :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
x `sustain` y   = x <> y `during` x

-- |
-- Move a value so that
--
times :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => Int -> a -> a
times n   = scat . replicate n

-- |
-- Class of values that can be split.
--
-- For non-positioned values such as 'Stretched', split cuts a value into pieces
-- of the given duration and the rest.
--
-- For positioned values succh as 'Note', split cuts a value relative to its onset.
--
--
-- Law
-- 
-- @
-- '_duration' ('beginning' t x) + '_duration' ('ending' t x) = '_duration' x
-- '_duration' ('beginning' t x) = t ``min`` '_duration' x
-- @
--
class HasDuration a => Splittable a where
  split      :: Duration -> a -> (a, a)
  beginning  :: Duration -> a -> a
  ending     :: Duration -> a -> a
  split   d x = (beginning d x, ending d x)
  beginning d = fst . split d
  ending    d = snd . split d

instance Splittable Duration where
  split x y = (x `min` y, y ^-^ (x `min` y))

instance Splittable Span where
  -- split d (view range -> (t1, t2)) = (t1 <-> (t1 .+^ d), (t1 .+^ d) <-> t2)
  split d' (view delta -> (t, d)) = let (d1, d2) = split d' d in (t >-> d1, (t.+^d1) >-> d2)

takeMWhile :: (Monoid a, Splittable a) => Duration -> (a -> Bool) -> a -> a
takeMWhile d p xs = if _duration xs <= 0 then mempty else takeMWhile' d p xs
  where
    takeMWhile' d p (split d -> (x, xs)) = if p x then x `mappend` takeMWhile d p xs else mempty

chunks :: Splittable a => Duration -> a -> [a]
chunks d xs = if _duration xs <= 0 then [] else chunks' d xs
  where
    chunks' d (split d -> (x, xs)) = [x] ++ chunks d xs

  
-- |
-- Class of values that can be reversed (retrograded).
--
-- For positioned values succh as 'Note', the value is reversed relative to its middle point, i.e.
-- the onset value becomes the offset value and vice versa.
--
-- For non-positioned values such as 'Stretched', the value is reversed in-place.
--
-- FIXME Second law is incompatible with 'revDefault' (and the 'Span' definition below)
--
-- Law
--
-- @
-- 'rev' ('rev' a) = a
-- @
--
-- @
-- 'abs' ('_duration' x) = _duration ('rev' x)
-- @
--
-- @
-- 'rev' s ``transform`` a = 'rev' (s ``transform`` a)
-- @
--
-- or equivalently,
--
-- @
-- 'transform' . 'rev' = 'fmap' 'rev' . 'transform'
-- @
--
-- For 'Span'
--
-- @
-- 'rev' = 'over' 'range' 'swap'
-- @
--
class Transformable a => Reversible a where

  -- | Reverse (retrograde) the given value.
  rev :: a -> a

--
-- XXX Counter-intuitive Behavior instances (just Behavior should reverse around origin,
-- while Bound (Behavior a) should reverse around the middle, like a note)
--

--
-- XXX Alternate formulation of second Reversiblee law
-- 
--     rev s `transform` a     = rev (s `transform` a)
-- ==> (rev s `transform`)     = rev . (s `transform`)
-- ==> transform (rev s)       = rev . (transform s)
-- ==> (transform . rev) s     = (rev .) (transform s)
-- ==> (transform . rev) s     = fmap rev (transform s)
-- ==> transform . rev         = fmap rev . transform
-- 

instance Reversible () where
  rev = id

instance Reversible Int where
  rev = id

instance Reversible Double where
  rev = id

instance Reversible Integer where
  rev = id

instance Reversible a => Reversible [a] where
  rev = reverse . map rev

instance Reversible a => Reversible (Seq a) where
  rev = Seq.reverse . fmap rev

instance Reversible Duration where
  rev = stretch (-1)

--
-- There is no instance for Reversible Time
-- as we can not satisfy the second Reversible law
--

instance Reversible Span where
  rev = revDefault

instance Reversible a => Reversible (a, b) where
  rev (s,a) = (rev s, a)

-- |
-- A default implementation of 'rev'
--
revDefault :: (HasPosition a, Transformable a) => a -> a
-- revDefault x = (stretch (-1) `whilst` undelaying (_position x 0.5 .-. 0)) x
revDefault x = stretch (-1) x

newtype NoReverse a = NoReverse { getNoReverse :: a }
  deriving (Typeable, Eq, Ord, Show)

instance Transformable (NoReverse a) where
  transform _ = id

instance Reversible (NoReverse a) where
  rev = id

-- |
-- View the reverse of a value.
--
-- >>> [1,2,3] & reversed %~ sort
-- > [3,2,1]
--
reversed :: Reversible a => Iso' a a
reversed = iso rev rev







-- |
-- Internal time representation. Can be anything with instances
-- for 'Fractional' and 'RealFrac'.
--
type TimeBase = Rational
-- type TimeBase = Fixed E12

instance HasResolution a => AdditiveGroup (Fixed a) where
  zeroV = 0
  negateV = negate
  (^+^) = (+)

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
  mempty  = 1
  mappend = (*^)
 -- TODO use some notion of norm rather than 1

instance Transformable Duration where
  (view delta -> (_, d1)) `transform` d2 = d1 * d2

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

instance Transformable Time where
  (view delta -> (t1, d1)) `transform` t2 = t1 ^+^ d1 *^ t2

instance HasDuration Time where
  _duration = 0

instance HasPosition Time where
  _position = const

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




-- |
-- A 'Span' represents two points in time @u@ and @v@ or, equivalently, a time @t@ and a
-- duration @d@. A third way of looking at 'Span' is that it represents a time
-- transformation where onset is translation and duration is scaling.
--
-- Pattern matching over span is possible (with @ViewPatterns@):
--
-- @
-- foo ('view' 'range' -> (t1, t2)) = ...
-- foo ('view' 'delta' -> (t, d)) = ...
-- @
--
-- The semantics are given by
--
-- @
-- type Span = R2
-- @
--
newtype Span = Delta { _delta :: (Time, Duration) }
  deriving (Eq, Ord, Typeable)

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
  show = showDelta

showRange :: Span -> String
showRange (view range -> (t,u)) = show t ++ " <-> " ++ show u

showDelta :: Span -> String
showDelta (view delta -> (t,d)) = show t ++ " >-> " ++ show d

instance HasPosition Span where
  -- Override as an optimization:
  _onset    (view range -> (t1, t2)) = t1
  _offset   (view range -> (t1, t2)) = t2
  _position (view range -> (t1, t2)) = alerp t1 t2

instance Transformable Span where
  transform = (<>)

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
-- Whether this is a proper span, i.e. whether @'_onset' x '<' '_offset' x@.
--
isProper :: Span -> Bool
isProper (view range -> (t, u)) = t < u

-- |
-- A prism to the subset of 'Span' that performs a stretch but no delay.
--
stretches :: Prism' Span Duration
stretches = prism (\d -> view (from delta) (0, d)) $ \x -> case view delta x of
  (0, d) -> Right d
  _      -> Left x

-- |
-- A prism to the subset of 'Span' that performs a delay but no stretch.
--
delays :: Prism' Span Time
delays = prism (\t -> view (from delta) (t, 1)) $ \x -> case view delta x of
  (t, 1) -> Right t
  _      -> Left x
       

--
-- $musicTimeSpanConstruct
--
-- - To convert a span to a pair, use @s^.'delta'@.
-- - To construct a span from a pair, use @(t, d)^.'from' 'delta'@.
--

--
-- $musicTimeSpanLaws
--
-- > forall s . id `whilst` s = id
-- > forall s . return `whilstM` s = return
-- > forall s . extract `whilstW` s = extract


-- We really must flip all these functions. To do:
--
--    1) Come up with some other name for the infix version
--    2) Acknowledge that this is a valid Lens (when flipped)
--
-- Perhaps we should call the inline version `whilst`, as in @f `whilst` delaying 2@?


-- |
-- Apply a function under transformation.
--
-- Designed to be used infix, as in
--
-- @
-- 'stretch' 2 ``whilst`` 'delaying' 2
-- @
--
whilst :: (Transformable a, Transformable b) => (a -> b) -> Span -> a -> b
f `whilst` t = transform (negateV t) . f . transform t

-- |
-- Apply a morphism under transformation (monadic version).
--

whilstM :: (Functor f, Transformable a, Transformable b) => (a -> f b) -> Span -> a -> f b
f `whilstM` t = fmap (transform (negateV t)) . f . transform t

{-
-- |
-- Apply a morphism under transformation (co-monadic version).
--
whilstW :: (Functor f, Transformable a, Transformable b) => (f a -> b) -> Span -> f a -> b
f `whilstW` t = transform (negateV t) . f . fmap (transform t)
-}

-- |
-- Apply a function under transformation.
--
whilstDelay :: (Transformable a, Transformable b) => (a -> b) -> Time -> a -> b
whilstDelay     = flip (flip whilst . delaying . (.-. 0))

-- |
-- Apply a function under transformation.
--
whilstStretch :: (Transformable a, Transformable b) => (a -> b) -> Duration -> a -> b
whilstStretch = flip (flip whilst . stretching)

whilstL :: (Functor f, Transformable a, Transformable b) 
  => LensLike f s t a b 
  -> LensLike f (Span,s) (Span,t) a b
whilstL  l f (s,a) = (s,) <$> (l $ f `whilstM` s) a

whilstLT :: (Functor f, Transformable a, Transformable b) 
  => LensLike f s t a b 
  -> LensLike f (Time,s) (Time,t) a b
whilstLT l f (t,a) = (t,) <$> (l $ f `whilstM` (t >-> 1)) a

whilstLD :: (Functor f, Transformable a, Transformable b) 
  => LensLike f s t a b 
  -> LensLike f (Duration,s) (Duration,t) a b
whilstLD l f (d,a) = (d,) <$> (l $ f `whilstM` (0 >-> d)) a

conjugate :: Span -> Span -> Span
conjugate t1 t2  = negateV t1 <> t2 <> t1


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
-- Whether the given 
-- 
encloses :: Span -> Span -> Bool
a `encloses` b = _onset b `inside` a && _offset b `inside` a

-- |
-- Whether the given 
-- 
overlaps :: Span -> Span -> Bool
a `overlaps` b = not (a `isBefore` b) && not (b `isBefore` a)

isBefore :: Span -> Span -> Bool
a `isBefore` b = (_onset a `max` _offset a) <= (_onset b `min` _offset b)


{-

  TODO test
  > (0 <-> 1) `overlaps` (1 <-> 3)
  False
  > (0 <-> 1) `overlaps` (1 <-> 0)
  True
  > (0 <-> 1) `overlaps` (1 <-> 2)
  False
  > (0 <-> 1.1) `overlaps` (1 <-> 2)
  True
  > (0 <-> 1) `overlaps` ((-1) <-> 2)
  True
  > (0 <-> (-1)) `overlaps` ((-1) <-> 2)
  True
  > ((-4) <-> (-1)) `overlaps` ((-1) <-> 2)
  False
  > ((-4) <-> (-1)) `overlaps` (0 <-> 2)
  False
  > (0 <-> 1) `overlaps` (0 <-> 4)
-}


--
-- TODO
--
-- We should give the lens in the each aspect class another name and use aliases
-- :info prints in GHCI
--
-- (We can't do much about the :type prints)
--


-- |
-- Pitch type.
--
type family Pitch (s :: *) :: *

-- |
-- Pitch type.
--
type family SetPitch (b :: *) (s :: *) :: *

-- |
-- Class of types that provide a single pitch.
--
class HasPitches s t => HasPitch s t where

  -- | Access the pitch.
  pitch :: Lens s t (Pitch s) (Pitch t)

-- |
-- Class of types that provide a pitch traversal.
--
class (Transformable (Pitch s),
       Transformable (Pitch t),
       SetPitch (Pitch t) s ~ t) => HasPitches s t where

  -- | Access all pitches.
  pitches :: Traversal s t (Pitch s) (Pitch t)

-- |
-- Pitch type.
--
pitch' :: (HasPitch s t, s ~ t) => Lens' s (Pitch s)
pitch' = pitch
{-# INLINE pitch' #-}

-- |
-- Pitch type.
--
pitches' :: (HasPitches s t, s ~ t) => Traversal' s (Pitch s)
pitches' = pitches
{-# INLINE pitches' #-}

#define PRIM_PITCH_INSTANCE(TYPE)       \
                                        \
type instance Pitch TYPE = TYPE;        \
type instance SetPitch a TYPE = a;      \
                                        \
instance (Transformable a, a ~ Pitch a) \
  => HasPitch TYPE a where {            \
  pitch = ($)              } ;          \
                                        \
instance (Transformable a, a ~ Pitch a) \
  => HasPitches TYPE a where {          \
  pitches = ($)              } ;        \


PRIM_PITCH_INSTANCE(())
PRIM_PITCH_INSTANCE(Bool)
PRIM_PITCH_INSTANCE(Ordering)
PRIM_PITCH_INSTANCE(Char)
PRIM_PITCH_INSTANCE(Int)
PRIM_PITCH_INSTANCE(Integer)
PRIM_PITCH_INSTANCE(Float)
PRIM_PITCH_INSTANCE(Double)

type instance Pitch (c,a)               = Pitch a
type instance SetPitch b (c,a)          = (c,SetPitch b a)
type instance Pitch [a]                 = Pitch a
type instance SetPitch b [a]            = [SetPitch b a]
type instance Pitch (Note a)            = Pitch a
type instance SetPitch g (Note a)       = Note (SetPitch g a)
type instance Pitch (Delayed a)         = Pitch a
type instance SetPitch g (Delayed a)    = Delayed (SetPitch g a)
type instance Pitch (Stretched a)       = Pitch a
type instance SetPitch g (Stretched a)  = Stretched (SetPitch g a)

instance HasPitch a b => HasPitch (c, a) (c, b) where
  pitch = _2 . pitch
instance HasPitches a b => HasPitches (c, a) (c, b) where
  pitches = traverse . pitches

instance HasPitches a b => HasPitches [a] [b] where
  pitches = traverse . pitches

instance (HasPitches a b) => HasPitches (Note a) (Note b) where
  pitches = _Wrapped . whilstL pitches
instance (HasPitch a b) => HasPitch (Note a) (Note b) where
  pitch = _Wrapped . whilstL pitch

instance (HasPitches a b) => HasPitches (Delayed a) (Delayed b) where
  pitches = _Wrapped . whilstLT pitches
instance (HasPitch a b) => HasPitch (Delayed a) (Delayed b) where
  pitch = _Wrapped . whilstLT pitch

instance (HasPitches a b) => HasPitches (Stretched a) (Stretched b) where
  pitches = _Wrapped . whilstLD pitches
instance (HasPitch a b) => HasPitch (Stretched a) (Stretched b) where
  pitch = _Wrapped . whilstLD pitch

-- |
-- Associated interval type.
--
type Interval a = Diff (Pitch a)

-- |
-- Class of types that can be transposed.
--
type Transposable a
  = (HasPitches a a,
     VectorSpace (Interval a), AffineSpace (Pitch a),
     IsInterval (Interval a), IsPitch (Pitch a))

-- |
-- Transpose up.
--
up :: Transposable a => Interval a -> a -> a
up v = pitches %~ (.+^ v)

-- |
-- Transpose down.
--
down :: Transposable a => Interval a -> a -> a
down v = pitches %~ (.-^ v)

-- |
-- Add the given interval above.
--
above :: (Semigroup a, Transposable a) => Interval a -> a -> a
above v x = x <> up v x

-- |
-- Add the given interval below.
--
below :: (Semigroup a, Transposable a) => Interval a -> a -> a
below v x = x <> down v x

-- |
-- Invert pitches.
--
invertPitches :: Transposable a => Pitch a -> a -> a
invertPitches p = pitches %~ reflectThrough p

-- |
-- Transpose up by the given number of octaves.
--
octavesUp :: Transposable a => Scalar (Interval a) -> a -> a
octavesUp n = up (_P8^*n)

-- |
-- Transpose down by the given number of octaves.
--
octavesDown :: Transposable a => Scalar (Interval a) -> a -> a
octavesDown n = down (_P8^*n)

-- |
-- Add the given octave above.
--
octavesAbove :: (Semigroup a, Transposable a) => Scalar (Interval a) -> a -> a
octavesAbove n = above (_P8^*n)

-- |
-- Add the given octave below.
--
octavesBelow :: (Semigroup a, Transposable a) => Scalar (Interval a) -> a -> a
octavesBelow n = below (_P8^*n)

-- |
augmentIntervals :: Transposable a => Interval a -> Voice a -> Voice a
augmentIntervals = error "Not implemented: augmentIntervals"
-- TODO generalize to any type where we can traverse phrases of something that has pitch


-- TODO augment/diminish intervals (requires withPrev or similar)
-- TODO invert diatonically
-- TODO rotatePitch (requires some kind of separate traversal)










-- |
-- Dynamics type.
--
type family Dynamic (s :: *) :: *

-- |
-- Dynamic type.
--
type family SetDynamic (b :: *) (s :: *) :: *

-- |
-- Class of types that provide a single dynamic.
--
class (HasDynamics s t) => HasDynamic s t where

  -- |
  dynamic :: Lens s t (Dynamic s) (Dynamic t)

-- |
-- Class of types that provide a dynamic traversal.
--
class (Transformable (Dynamic s),
       Transformable (Dynamic t),
       SetDynamic (Dynamic t) s ~ t) => HasDynamics s t where

  -- | Dynamic type.
  dynamics :: Traversal s t (Dynamic s) (Dynamic t)

-- |
-- Dynamic type.
--
dynamic' :: (HasDynamic s t, s ~ t) => Lens' s (Dynamic s)
dynamic' = dynamic

-- |
-- Dynamic type.
--
dynamics' :: (HasDynamics s t, s ~ t) => Traversal' s (Dynamic s)
dynamics' = dynamics

#define PRIM_DYNAMIC_INSTANCE(TYPE)       \
                                          \
type instance Dynamic TYPE = TYPE;        \
type instance SetDynamic a TYPE = a;      \
                                          \
instance (Transformable a, a ~ Dynamic a) \
  => HasDynamic TYPE a where {            \
  dynamic = ($)              } ;          \
                                          \
instance (Transformable a, a ~ Dynamic a) \
  => HasDynamics TYPE a where {           \
  dynamics = ($)               } ;        \

PRIM_DYNAMIC_INSTANCE(())
PRIM_DYNAMIC_INSTANCE(Bool)
PRIM_DYNAMIC_INSTANCE(Ordering)
PRIM_DYNAMIC_INSTANCE(Char)
PRIM_DYNAMIC_INSTANCE(Int)
PRIM_DYNAMIC_INSTANCE(Integer)
PRIM_DYNAMIC_INSTANCE(Float)
PRIM_DYNAMIC_INSTANCE(Double)

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
  dynamic = _Wrapped . whilstL dynamic

instance HasDynamics a b => HasDynamics (Note a) (Note b) where
  dynamics = _Wrapped . whilstL dynamics


-- |
-- Associated interval type.
--
type Level a = Diff (Dynamic a)

-- |
-- Class of types that can be transposed.
--
type Attenuable a 
  = (HasDynamics a a,
     VectorSpace (Level a), AffineSpace (Dynamic a),
     {-IsLevel (Level a), -} IsDynamics (Dynamic a))

-- |
-- Transpose up.
--
louder :: Attenuable a => Level a -> a -> a
louder a = dynamics %~ (.+^ a)

-- |
-- Transpose down.
--
softer :: Attenuable a => Level a -> a -> a
softer a = dynamics %~ (.-^ a)

-- |
-- Transpose down.
--
volume :: (Num (Dynamic t), HasDynamics s t, Dynamic s ~ Dynamic t) => Dynamic t -> s -> t
volume a = dynamics *~ a

-- |
-- Transpose down.
--
level :: Attenuable a => Dynamic a -> a -> a
level a = dynamics .~ a

compressor :: Attenuable a => 
  Dynamic a           -- ^ Threshold
  -> Scalar (Level a) -- ^ Ratio
  -> a 
  -> a
compressor = error "Not implemented: compressor"

--
-- TODO non-linear fades etc
--

-- |
-- Fade in.
--
fadeIn :: (HasPosition a, HasDynamics a a, Dynamic a ~ Behavior c, Fractional c) => Duration -> a -> a
fadeIn d x = x & dynamics *~ (_onset x >-> d `transform` unit)

-- |
-- Fade in.
--
fadeOut :: (HasPosition a, HasDynamics a a, Dynamic a ~ Behavior c, Fractional c) => Duration -> a -> a
fadeOut d x = x & dynamics *~ (d <-< _offset x `transform` rev unit)









-- |
-- Articulations type.
--
type family Articulation (s :: *) :: *

-- |
-- Articulation type.
--
type family SetArticulation (b :: *) (s :: *) :: *

-- |
-- Class of types that provide a single articulation.
--
class (HasArticulations s t) => HasArticulation s t where

  -- | Articulation type.
  articulation :: Lens s t (Articulation s) (Articulation t)

-- |
-- Class of types that provide a articulation traversal.
--
class (Transformable (Articulation s),
       Transformable (Articulation t),
       SetArticulation (Articulation t) s ~ t) => HasArticulations s t where

  -- | Articulation type.
  articulations :: Traversal s t (Articulation s) (Articulation t)

-- |
-- Articulation type.
--
articulation' :: (HasArticulation s t, s ~ t) => Lens' s (Articulation s)
articulation' = articulation

-- |
-- Articulation type.
--
articulations' :: (HasArticulations s t, s ~ t) => Traversal' s (Articulation s)
articulations' = articulations

#define PRIM_ARTICULATION_INSTANCE(TYPE)       \
                                          \
type instance Articulation TYPE = TYPE;        \
type instance SetArticulation a TYPE = a;      \
                                          \
instance (Transformable a, a ~ Articulation a) \
  => HasArticulation TYPE a where {            \
  articulation = ($)              } ;          \
                                          \
instance (Transformable a, a ~ Articulation a) \
  => HasArticulations TYPE a where {           \
  articulations = ($)               } ;        \

PRIM_ARTICULATION_INSTANCE(())
PRIM_ARTICULATION_INSTANCE(Bool)
PRIM_ARTICULATION_INSTANCE(Ordering)
PRIM_ARTICULATION_INSTANCE(Char)
PRIM_ARTICULATION_INSTANCE(Int)
PRIM_ARTICULATION_INSTANCE(Integer)
PRIM_ARTICULATION_INSTANCE(Float)
PRIM_ARTICULATION_INSTANCE(Double)


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
  articulation = _Wrapped . whilstL articulation

instance (HasArticulations a b) => HasArticulations (Note a) (Note b) where
  articulations = _Wrapped . whilstL articulations


accent = error "Not implemented: accent"
marcato = error "Not implemented: marcato"
accentLast = error "Not implemented: accentLast"
marcatoLast = error "Not implemented: marcatoLast"
accentAll = error "Not implemented: accentAll"
marcatoAll = error "Not implemented: marcatoAll"

tenuto = error "Not implemented: tenuto"
separated = error "Not implemented: separated"
staccato = error "Not implemented: staccato"
portato = error "Not implemented: portato"
legato = error "Not implemented: legato"
spiccato = error "Not implemented: spiccato"









-- |
-- Parts type.
--
type family Part (s :: *) :: * -- Part s   = a

-- |
-- Part type.
--
type family SetPart (b :: *) (s :: *) :: * -- Part b s = t

-- |
-- Class of types that provide a single part.
--
class (HasParts s t) => HasPart s t where

  -- | Part type.
  part :: Lens s t (Part s) (Part t)

-- |
-- Class of types that provide a part traversal.
--
class (Transformable (Part s),
       Transformable (Part t),
       SetPart (Part t) s ~ t) => HasParts s t where

  -- | Part type.
  parts :: Traversal s t (Part s) (Part t)

-- |
-- Part type.
--
part' :: (HasPart s t, s ~ t) => Lens' s (Part s)
part' = part

-- |
-- Part type.
--
parts' :: (HasParts s t, s ~ t) => Traversal' s (Part s)
parts' = parts

type instance Part Bool = Bool
type instance SetPart a Bool = a
instance (b ~ Part b, Transformable b) => HasPart Bool b where
  part = ($)
instance (b ~ Part b, Transformable b) => HasParts Bool b where
  parts = ($)

type instance Part Ordering = Ordering
type instance SetPart a Ordering = a
instance (b ~ Part b, Transformable b) => HasPart Ordering b where
  part = ($)
instance (b ~ Part b, Transformable b) => HasParts Ordering b where
  parts = ($)

type instance Part () = ()
type instance SetPart a () = a
instance (b ~ Part b, Transformable b) => HasPart () b where
  part = ($)
instance (b ~ Part b, Transformable b) => HasParts () b where
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
  part = _Wrapped . whilstL part

instance (HasParts a b) => HasParts (Note a) (Note b) where
  parts = _Wrapped . whilstL parts

type HasPart' a = HasPart a a
type HasParts' a = HasParts a a

-- |
-- List all the parts
--
allParts :: (Ord (Part a), HasParts' a) => a -> [Part a]
allParts = Data.List.nub . Data.List.sort . toListOf parts

-- |
-- List all the parts
--
extractPart :: (Eq (Part a), HasPart' a) => Part a -> Score a -> Score a
extractPart = extractPartG

extractPartG :: (Eq (Part a), MonadPlus f, HasPart' a) => Part a -> f a -> f a
extractPartG p x = head $ (\p s -> filterPart (== p) s) <$> [p] <*> return x

-- |
-- List all the parts
--
extractParts :: (Ord (Part a), HasPart' a) => Score a -> [Score a]
extractParts = extractPartsG

extractPartsG
  :: (MonadPlus f,
      HasParts' (f a), HasPart' a, Part (f a) ~ Part a,
      Ord (Part a)) => f a -> [f a]
extractPartsG x = (\p s -> filterPart (== p) s) <$> allParts x <*> return x

filterPart :: (MonadPlus f, HasPart a a) => (Part a -> Bool) -> f a -> f a
filterPart p = mfilter (\x -> p (x ^. part))





-- |
-- 'Delayed' represents a value with an offset in time.
--
-- A delayed value has a known 'position', but no 'duration'.
--
-- Placing a value inside 'Delayed' does not make it invariant under 'stretch', as the
-- offset of a delayed value may be stretched with respect to the origin. However, in
-- contrast to a note the /duration/ is not stretched.
--
-- The semantics are given by
--
-- @
-- type Delayed a = (Time, a)
-- @
--
newtype Delayed a = Delayed   { _getDelayed :: (Time, a) }
  deriving (Eq, {-Ord, -}{-Show, -}
            Applicative, Monad, {-Comonad, -}
            Functor,  Foldable, Traversable)

deriving instance Typeable1 Delayed
deriving instance Show a => Show (Delayed a)

-- | TODO Unsafe
instance Wrapped (Delayed a) where
  type Unwrapped (Delayed a) = (Time, a)
  _Wrapped' = iso _getDelayed Delayed

instance Rewrapped (Delayed a) (Delayed b)

instance Transformable (Delayed a) where
  transform t = over _Wrapped $ first (transform t)

instance HasDuration (Delayed a) where
  _duration x = _offset x .-. _onset x

instance HasPosition (Delayed a) where
  x `_position` p = ask (view _Wrapped x) `_position` p

instance Reversible (Delayed a) where
  rev = revDefault

instance Splittable a => Splittable (Delayed a) where
  -- FIXME

-- |
-- View a delayed value as a pair of the original value and the transformation (and vice versa).
--
getDelayed :: (Transformable a, Transformable b) 
  => Lens 
      (Delayed a) (Delayed b) 
      a b
getDelayed = lens runDelayed (flip $ _delayed . const)
  where
    _delayed f (Delayed (t,x)) = 
      Delayed (t, f `whilstDelay` t $ x)
{-# INLINE getDelayed #-}







-- |
-- A 'Stretched' value has a known 'duration', but no 'position'.
--
-- Placing a value inside 'Stretched' makes it invariante under 'delay'.
--
-- The semantics are given by
--
-- @
-- type Stretched = (Duration, a)
-- @
--
newtype Stretched a = Stretched { _getStretched :: (Duration, a) }
  deriving (Eq, {-Ord, -}{-Show, -}
            Applicative, Monad, {-Comonad, -}
            Functor,  Foldable, Traversable)

-- >>> stretch 2 $ (5,1)^.stretched
-- (10,1)^.stretched
--
-- >>> delay 2 $ (5,1)^.stretched
-- (5,1)^.stretched
--

deriving instance Typeable1 Stretched

-- | TODO Unsafe
instance Wrapped (Stretched a) where
  type Unwrapped (Stretched a) = (Duration, a)
  _Wrapped' = iso _getStretched Stretched

instance Rewrapped (Stretched a) (Stretched b)

instance Transformable (Stretched a) where
  transform t = over _Wrapped $ first (transform t)

instance HasDuration (Stretched a) where
  _duration = _duration . ask . view _Wrapped

instance Reversible (Stretched a) where
  rev = stretch (-1)

instance Splittable a => Splittable (Stretched a) where
  beginning d = over _Wrapped $ \(s, v) -> (beginning d s, beginning d v)
  ending    d = over _Wrapped $ \(s, v) -> (ending    d s, ending    d v)

deriving instance Show a => Show (Stretched a)

-- |
-- View a stretched value as a pair of the original value and the transformation (and vice versa).
--
getStretched :: (Transformable a, Transformable b) => Lens (Stretched a) (Stretched b) a b
getStretched = lens runStretched (flip $ _stretched . const)
  where
    _stretched f (Stretched (d,x)) = 
      Stretched (d, f `whilstStretch` d $ x)
{-# INLINE getStretched #-}


-- |
-- A 'Note' is a value with a known 'era'.
--
-- You can use 'value' to apply a function in the context of the transformation,
-- i.e.
--
-- @
-- over value (* time) (delay 2 $ return time)
-- @
--
-- @
-- ('view' 'value') . 'transform' s = 'transform' s . ('view' 'value')
-- @
--
-- The semantics are given by
--
-- @
-- type Note a = (Span, a)
-- @
--
newtype Note a = Note { _getNote :: (Span, a) }

deriving instance Eq a => Eq (Note a)
deriving instance Functor Note
deriving instance Typeable1 Note
deriving instance Foldable Note
deriving instance Traversable Note

instance (Show a, Transformable a) => Show (Note a) where
  show x = show (x^.from note) ++ "^.note"

-- |
-- Note is a 'Monad' and 'Applicative' in the style of pair, with 'return' placing a value
-- at the default span 'mempty' and 'join' composing time transformations.
deriving instance Monad Note
deriving instance Applicative Note

-- | TODO Unsafe
instance Wrapped (Note a) where
  type Unwrapped (Note a) = (Span, a)
  _Wrapped' = iso _getNote Note

instance Rewrapped (Note a) (Note b)

instance Transformable (Note a) where
  transform t = over _Wrapped $ first (transform t)

instance HasDuration (Note a) where
  _duration = _duration . ask . view _Wrapped

instance HasPosition (Note a) where
  x `_position` p = ask (view _Wrapped x) `_position` p

instance Splittable a => Splittable (Note a) where
  beginning d = over _Wrapped $ \(s, v) -> (beginning d s, beginning d v)
  ending    d = over _Wrapped $ \(s, v) -> (ending    d s, ending    d v)

instance Reversible (Note a) where
  rev = revDefault

-- |
-- View a note as a pair of the original value and the transformation (and vice versa).
--
note :: (Transformable a, Transformable b) => 
  Iso 
    (Span, a) (Span, b) 
    (Note a) (Note b)
note = _Unwrapped

-- |
-- View the value in the note.
--
getNote :: (Transformable a, Transformable b) => 
  Lens 
    (Note a) (Note b) 
    a b
getNote = lens runNote (flip $ mapNote . const)
  where
    runNote = uncurry transform . view _Wrapped
    mapNote f (view (from note) -> (s,x)) = view note (s, f `whilst` negateV s $ x)

{-# INLINE getNote #-}

-- |
-- View a delayed value as a pair of a the original value and a delay time.
--
delayed :: Iso (Time, a) (Time, b) (Delayed a) (Delayed b)
delayed = _Unwrapped

-- |
-- View a stretched value as a pair of the original value and a stretch factor.
--
stretched :: Iso (Duration, a) (Duration, b) (Stretched a) (Stretched b)
stretched = _Unwrapped

-- |
-- Extract the delayed value.
--
runDelayed :: Transformable a => Delayed a -> a
runDelayed = uncurry delayTime . view _Wrapped

-- |
-- Extract the stretched value.
--
runStretched :: Transformable a => Stretched a -> a
runStretched = uncurry stretch . view _Wrapped







-- |
-- 'Bound' restricts the start and stop time of a value, and prevents access to values
-- outside the bounds.
--
-- 'Bound' is especially useful to restrict the range of a 'Behavior'. If we have a
-- value with can only be reasonably defined for a particular time range, we can
-- represent it as 'Bound' 'Behavior'. This is isomorphic to a 'Note' 'Segment', and
-- 'bounded' whitnesses the isomorphism.
--
-- 'Bound' is not 'Foldable' or 'Traversable', as that would allow us to access values
-- outside the bounds. However, we can still access values of a 'Bound' 'Behavior' in a
-- safe manner using 'trim' or 'splice'.
--
-- The semantics are given by
--
-- @
-- type Bound a = (Time, Time, a)
-- @
--
newtype Bound a = Bound { getBound :: (Span, a) }
  deriving (Functor, Semigroup, Typeable, Eq, Show)


--
-- TODO define Applicative/Monad
--
--
-- This is a Writer-style instance with interval arithmetic style union/empty as the Monoid
-- A possible problem with this is that there are multiple representations of the empty
-- set (namely [(t, t)^.from range | t <- {Time} ]).
--


-- 
-- These are both unsafe, as they allow us to define 'unBound'
-- 
-- instance Foldable Bound where
--   foldr f z (Bound (_,x)) = f x z
-- 
-- instance Traversable Bound where
--   traverse f (Bound (s,x)) = (Bound . (s,)) <$> f x
-- 

-- | TODO Unsafe
instance Wrapped (Bound a) where
  type Unwrapped (Bound a) = (Span, a)
  _Wrapped' = iso getBound Bound

instance Rewrapped (Bound a) (Bound b)

instance Reversible a => Reversible (Bound a) where
  rev = over _Wrapped $ \(s,x) -> (rev s, rev x)

instance (HasPosition a, Splittable a) => Splittable (Bound a) where
  -- TODO

-- |
-- 'Bound' transform by transforming the bounded value as well as
-- the bounds.
--
instance Transformable a => Transformable (Bound a) where
  transform t = over _Wrapped (transform t *** transform t)

instance (HasPosition a, HasDuration a) => HasDuration (Bound a) where
  _duration x = _offset x .-. _onset x

instance HasPosition a => HasPosition (Bound a) where
  -- TODO lawless
  -- _position (Bound (view range -> (t, u), x)) d = truncating t u (_position x d)
  _position (Bound (view range -> (t, u), x)) d = alerp t u d

truncating :: Ord a => a -> a -> a -> a
truncating t u x = (x `max` t) `min` u

-- |
-- Add bounds.
--
bounds :: Time -> Time -> a -> Bound a
bounds t u x = Bound (t <-> u, x)

-- |
-- Add bounds.
--
-- @
-- (s,x)^.note = (bounding s . transform s) x
-- @
--
bounding :: Span -> a -> Bound a
bounding (view range -> (t, u)) = bounds t u

-- |
-- View a 'Note' 'Segment' as a 'Bound' 'Behavior' and vice versa.
--
-- This can be used to safely turn a behavior into a segment and vice
-- versa. Usually 'focusing' is more convenient to use.
--
bounded' :: Iso'
  (Note (Segment a))
  (Bound (Behavior a))
bounded' = bounded

-- |
-- View a 'Note' 'Segment' as a 'Bound' 'Behavior' and vice versa.
--
-- This can be used to safely turn a behavior into a segment and vice
-- versa. Usually 'focusing' is more convenient to use.
--
bounded :: Iso
  (Note (Segment a))
  (Note (Segment b))
  (Bound (Behavior a))
  (Bound (Behavior b))
bounded = iso ns2bb bb2ns 
  where
    bb2ns (Bound (s, x)) = view note (s, b2s $ transform (negateV s) $ x)
    ns2bb (view (from note) -> (s, x)) = Bound (s,       transform s           $ s2b $ x)
    s2b = under tabulated (. realToFrac)
    b2s = under tabulated (. realToFrac)

--
-- Note that the isomorhism only works because of 'Bound' being abstract.
-- A function @unBound :: Bound a -> a@ could break the isomorphism
-- as follows:
--
-- > (unBound . view (from bounded . bounded) . bounds 0 1) b ! 2
-- *** Exception: Outside 0-1
--

-- |
-- Extract a bounded behavior, replacing all values outside the bound with 'mempty'.
--
-- @
-- 'trim'   = 'splice' 'mempty'
-- 'trim' x = 'trimBefore' '_onset' x . 'trimAfter' '_offset' x
-- @
--
trim :: Monoid b => Bound (Behavior b) -> Behavior b
trim = trimG
  where
    trimG :: (Monoid b, Representable f, Rep f ~ Time) => Bound (f b) -> f b
    trimG (Bound (s, x)) = tabulate (trimOutside s) `apRep` x

trimOutside :: Monoid a => Span -> Time -> a -> a
trimOutside s t x = if t `inside` s then x else mempty

-- |
-- Inserts a bounded behavior on top of another behavior.
--
-- @
-- 'trim' = 'splice' 'mempty'
-- @
--
-- (Named after the analogous tape-editing technique.)
--
splice :: Behavior a -> Bound (Behavior a) -> Behavior a
splice constant insert = fmap fromLast $ fmap toLast constant <> trim (fmap (fmap toLast) insert)
  where
    toLast   = Option . Just . Last
    fromLast = getLast . fromJust . getOption
    -- fromJust is safe here, as toLast is used to create the Maybe wrapper


-- TODO Compare Diagram's Trail and Located (and see the conal blog post)

-- |
--
-- A 'Segment' is a value varying over some unknown time span.
--
-- To give a segment an explicit duration, use 'Stretched' 'Segment'.
--
-- To place a segment in a particular time span, use 'Note' 'Segment'.
--
-- The semantics are given by
--
-- @
-- type Segment a = 'Duration' -> a
-- @
--
newtype Segment a = Segment { getSegment :: Clipped Duration -> a }
  deriving (Functor, Applicative, Monad{-, Comonad-})

--
-- TODO constant-optimize a la Conal
--

-- $musicTimeSegmentExamples
-- 
-- > foldr1 appendSegment $ map (view stretched) $ [(0.5,0::Segment Float), (1, timeS), (2,rev timeS), (3,-1)]
--
-- > openG $ draw $ (1, timeS :: Segment Float)^.stretched
-- 

instance Show (Segment a) where
  show _ = "<<Segment>>"

deriving instance Typeable1 Segment
deriving instance Distributive Segment

instance Representable Segment where
  type Rep Segment = Duration
  tabulate f = Segment (f . fromClipped)
  index    (Segment f) = f . unsafeToClipped

-- |
-- Segments are /invariant/ under transformation. To transform a timve varying value, use
-- 'fromSegment'.
--
instance Transformable (Segment a) where
  transform _ = id

instance Reversible (Segment a) where
  -- TODO in terms of Representable
  rev (Segment f) = Segment (f . unsafeToClipped . r . fromClipped)
    where
      r x = (x * (-1)) + 1

type instance Pitch                 (Segment a) = Segment (Pitch a)
type instance SetPitch (Segment g)  (Segment a) = Segment (SetPitch g a)

instance (HasPitch a a, HasPitch a b) => HasPitches (Segment a) (Segment b) where
  pitches = through pitch pitch
instance (HasPitch a a, HasPitch a b) => HasPitch (Segment a) (Segment b) where
  pitch = through pitch pitch

type instance Dynamic                 (Segment a) = Segment (Dynamic a)
type instance SetDynamic (Segment g) (Segment a) = Segment (SetDynamic g a)

instance (HasDynamic a a, HasDynamic a b) => HasDynamics (Segment a) (Segment b) where
  dynamics = through dynamic dynamic
instance (HasDynamic a a, HasDynamic a b) => HasDynamic (Segment a) (Segment b) where
  dynamic = through dynamic dynamic


type instance Articulation                 (Segment a) = Segment (Articulation a)
type instance SetArticulation (Segment g) (Segment a) = Segment (SetArticulation g a)

instance (HasArticulation a a, HasArticulation a b) => HasArticulations (Segment a) (Segment b) where
  articulations = through articulation articulation
instance (HasArticulation a a, HasArticulation a b) => HasArticulation (Segment a) (Segment b) where
  articulation = through articulation articulation


type instance Part                 (Segment a) = Segment (Part a)
type instance SetPart (Segment g) (Segment a) = Segment (SetPart g a)

instance (HasPart a a, HasPart a b) => HasParts (Segment a) (Segment b) where
  parts = through part part
instance (HasPart a a, HasPart a b) => HasPart (Segment a) (Segment b) where
  part = through part part

#ifdef INCLUDE_LIFTED
deriving instance Semigroup a => Semigroup (Segment a)
deriving instance Monoid a => Monoid (Segment a)
deriving instance Num a => Num (Segment a)
deriving instance Fractional a => Fractional (Segment a)
deriving instance Floating a => Floating (Segment a)

instance IsPitch a => IsPitch (Segment a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Segment a) where
  fromInterval = pure . fromInterval

instance Eq a => Eq (Segment a) where
  (==) = error "No fun"

instance Ord a => Ord (Segment a) where
  (<) = error "No fun"
  max = liftA2 max
  min = liftA2 min
#endif


-- |
-- Index a segment.
--
(!.) :: Segment a -> Duration -> a
(!.) = (!)

-- |
-- View a segment as a time function and vice versa.
--
segment' :: Iso' (Duration -> a) (Segment a)
segment' = segment

-- |
-- View a segment as a time function and vice versa.
--
segment :: Iso (Duration -> a) (Duration -> b) (Segment a) (Segment b)
segment = tabulated

-- |
-- A behavior that gives the current time, i.e. the identity function
-- 
timeS :: Floating a => Segment a
timeS = realToFrac^.segment

sineS :: Floating a => Segment a
#ifdef INCLUDE_LIFTED
sineS = sin (timeS*tau)
#else
sineS = undefined
#endif

appendSegment :: Stretched (Segment a) -> Stretched (Segment a) -> Stretched (Segment a)
appendSegment (Stretched (d1,s1)) (Stretched (d2,s2)) = Stretched (d1+d2, slerp (d1/(d1+d2)) s1 s2)

-- |
-- Append a voice of segments to a single stretched segment.
--
appendS :: Voice (Segment a) -> Stretched (Segment a)
appendS = foldr1 appendSegment . toListOf voiceElements

-- t < i && 0 <= t <= 1   ==> 0 < (t/i) < 1
-- i     is the fraction of the slerped segment spent in a
-- (1-i) is the fraction of the slerped segment spent in b
slerp :: Duration -> Segment a -> Segment a -> Segment a
slerp i a b
  | i < 0 || i >= 1    = error "slerp: Bad value"
  | otherwise = tabulate $ \t -> if t < i then a ! (t/i) else b ! ((t-i)/(1-i))

slerp2 :: (a -> a -> a) -> Duration -> Segment a -> Segment a -> Segment a
slerp2 f i a b
  | i < 0 || i >= 1    = error "slerp: Bad value"
  | otherwise = tabulate $ \t -> case t `compare` i of
      LT -> a ! (t/i)
      EQ -> (a ! 1) `f` (b ! 1)
      GT -> b ! ((t-i)/(1-i))


-- Behavior is 'Representable':
--
-- > ask = realToFrac <$> time
-- > localRep (- t) = delay t
-- > localRep (/ t) = stretch t

-- |
-- A 'Behavior' is a value varying over time.
-- 
-- Use 'focusing' to view a particular 'Segment'.
--
-- The semantics are given by
--
-- @
-- type Behavior a = 'Time' -> a
-- @
--
newtype Behavior a  = Behavior { getBehavior :: Time -> a }   deriving (Functor, Applicative, Monad{-, Comonad-})

--
-- $musicTimeBehaviorExamples
--
-- 'behavior' let us convert any function to a behavior using '^.' or 'view'.
--
-- We can unwrap a behavior using @'from' 'behavior'@ or '!^'.
--
-- A sine function
--
-- @
-- ('sin' . (* 'tau') . 'realToFrac')^.'behavior'
-- @
--
-- A behavior that switches from (-1) to 1 at time 0
--
-- @
-- (\\t -> if t < 0 then (-1) else 1)^.'behavior'
-- @
--
-- A time-varying function applied to a value
--
-- @
-- ('+')^.'behavior' '<*>' 10
-- @
-- 
instance Show (Behavior a) where
  show _ = "<<Behavior>>"

deriving instance Typeable1 Behavior
deriving instance Distributive Behavior

instance Transformable (Behavior a) where
  transform s (Behavior a) = Behavior (a `whilst` s)
    where
      f `whilst` s = f . transform (negateV s)

instance Reversible (Behavior a) where
  rev = stretch (-1)
  -- TODO alternative
  -- rev = (stretch (-1) `whilst` undelaying 0.5)
  -- (i.e. revDefault pretending that Behaviors have era (0 <-> 1))

instance Representable Behavior where
  type Rep Behavior = Time
  tabulate = Behavior
  index (Behavior x) = x


-- 
-- type instance Pitch                 (Behavior a) = Behavior (Pitch a)
-- type instance SetPitch (Behavior g) (Behavior a) = Behavior (SetPitch g a)
--
-- instance (HasPitch a a, HasPitch a b) => HasPitches (Behavior a) (Behavior b) where
--   pitches = through pitch pitch
-- instance (HasPitch a a, HasPitch a b) => HasPitch (Behavior a) (Behavior b) where
--   pitch = through pitch pitch
-- 

type instance Pitch      (Behavior a) = Behavior a
type instance SetPitch b (Behavior a) = b
instance (Transformable a, Transformable b, b ~ Pitch b) => HasPitches (Behavior a) b where
  pitches = ($)
instance (Transformable a, Transformable b, b ~ Pitch b) => HasPitch (Behavior a) b where
  pitch = ($)


type instance Dynamic                 (Behavior a) = Behavior (Dynamic a)
type instance SetDynamic (Behavior g) (Behavior a) = Behavior (SetDynamic g a)

instance (HasDynamic a a, HasDynamic a b) => HasDynamics (Behavior a) (Behavior b) where
  dynamics = through dynamic dynamic
instance (HasDynamic a a, HasDynamic a b) => HasDynamic (Behavior a) (Behavior b) where
  dynamic = through dynamic dynamic


type instance Articulation                 (Behavior a) = Behavior (Articulation a)
type instance SetArticulation (Behavior g) (Behavior a) = Behavior (SetArticulation g a)

instance (HasArticulation a a, HasArticulation a b) => HasArticulations (Behavior a) (Behavior b) where
  articulations = through articulation articulation
instance (HasArticulation a a, HasArticulation a b) => HasArticulation (Behavior a) (Behavior b) where
  articulation = through articulation articulation


type instance Part                 (Behavior a) = Behavior (Part a)
type instance SetPart (Behavior g) (Behavior a) = Behavior (SetPart g a)

instance (HasPart a a, HasPart a b) => HasParts (Behavior a) (Behavior b) where
  parts = through part part
instance (HasPart a a, HasPart a b) => HasPart (Behavior a) (Behavior b) where
  part = through part part



-- Needed by Reactive
deriving instance Semigroup a => Semigroup (Behavior a)
deriving instance Monoid a => Monoid (Behavior a)
deriving instance Num a => Num (Behavior a)
deriving instance Fractional a => Fractional (Behavior a)
deriving instance Floating a => Floating (Behavior a)

#ifdef INCLUDE_LIFTED
deriving instance AdditiveGroup a => AdditiveGroup (Behavior a)

instance IsPitch a => IsPitch (Behavior a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Behavior a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Behavior a) where
  fromDynamics = pure . fromDynamics

instance Eq a => Eq (Behavior a) where
  (==) = error "No fun"

instance Ord a => Ord (Behavior a) where
  (<) = error "No fun"
  max = liftA2 max
  min = liftA2 min

instance VectorSpace a => VectorSpace (Behavior a) where
  type Scalar (Behavior a) = Behavior (Scalar a)
  (*^) = liftA2 (*^)

instance AffineSpace a => AffineSpace (Behavior a) where
  type Diff (Behavior a) = Behavior (Diff a)
  (.-.) = liftA2 (.-.)
  (.+^) = liftA2 (.+^)
#endif


-- |
-- Returns the value of a behavior at a given time
--
-- Note that this is just an alias defined to make the documentation nicer:
--
-- @
-- '!^' = '!'
-- @
--
(!^) :: Behavior a -> Time -> a
(!^) = (!)

infixl 6 !^

-- |
-- View a behavior as a time function and vice versa.
--
-- Note that this is just an alias defined to make the documentation nicer:
--
--
-- @
-- 'behavior' = 'tabulated'
-- @
--
behavior' :: Iso' (Time -> a) (Behavior a)
behavior' = tabulated

-- |
-- View a behavior as a time function and vice versa.
--
-- Note that this is just an alias defined to make the documentation nicer:
--
--
-- @
-- 'behavior' = 'tabulated'
-- @
--
behavior :: Iso (Time -> a) (Time -> b) (Behavior a) (Behavior b)
behavior = tabulated

-- |
-- View a time function as a behavior.
--
-- @
-- unbehavior    = from behavior
-- x^.unbehavior = (x !)
-- @
--
unbehavior :: Iso (Behavior a) (Behavior b) (Time -> a) (Time -> b)
unbehavior = from behavior

--
-- @
-- ('const' x)^.'behavior' ! t = x   forall t
-- @
--
--


-- |
-- A behavior that
--
time' :: Behavior Time
time' = id ^. tabulated

-- |
-- A behavior that gives the current time, i.e. the identity function
--
-- Should really have the type 'Behavior' 'Time', but is provided in a more general form
-- for convenience.
--
time :: Fractional a => Behavior a
time = realToFrac ^. tabulated
--
-- > f t = t
--

-- |
-- A behavior that varies from 0 to 1 during the same time interval and is 0 before and 1 after
-- that interval.
--
unit :: Fractional a => Behavior a
unit = switch 0 0 (switch 1 time 1)
-- > f t | t < 0     = 0
-- >     | t > 1     = 1
-- >     | otherwise = t
--

-- |
-- A behavior that
--
interval :: (Fractional a, Transformable a) => Time -> Time -> Note (Behavior a)
interval t u = (t <-> u, time) ^. note

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
-- A behavior that goes from 0 to 1 repeatedly with a period of 1.
--
sawtooth :: RealFrac a => Behavior a
sawtooth = time - fmap floor' time

-- |
-- A behavior that is 1 at time 0, and 0 at all other times.
--
impulse :: Num a => Behavior a
impulse = switch' 0 0 1 0
-- > f t | t == 0    = 1
-- >     | otherwise = 0
--

-- |
-- A behavior that goes from 0 to 1 at time 0.
--
turnOn  = switch 0 0 1

-- |
-- A behavior that goes from 1 to 0 at time 0.
--
turnOff = switch 0 1 0

--
-- TODO
--
-- Because the 'Time' type is fixed and unbounded in the current version, we can not
-- define a generix isomorphism from behaviors to segments. If we change the library to
-- provide multiple time representations (using TFs or similar), we should provide
-- these combinators:
--
-- > focusOnFullRange :: Bounded Time => Behavior a -> Segment a
-- > focusingOnFullRange :: Bounded Time => Iso' (Behavior a) (Segment a)
--

-- |
-- View part of a 'Behavior' as a 'Segment'.
--
-- @
-- 'time' & 'focusing' ``on`` (2 '<->' 3) '*~' 0
-- @
--
focusing :: Lens' (Behavior a) (Segment a)
focusing = lens get set
  where
    get = view (from bounded . getNote) . {-pure-}bounding mempty
    set x = splice x . (view bounded) . pure


{-
-- |
-- View part of a 'Behavior' as a 'Segment'.
--
-- This can be used to modify a behavior in a specific range, as in
--
-- @
-- 'time' & 'focusing' ``on`` (2 '<->' 3) '+a~' 1
-- 'time' & 'focusingOn' (2 '<->' 3) '+~' 1
-- @
--
focusingOn :: Span -> Lens' (Behavior a) (Segment a)
focusingOn s = flip whilstM (negateV s) . focusing
-- or focusing . flip whilstM s
-}

-- focusing `on` s == focusingOn s
f `on` s = flip whilstM (negateV s) . f

-- |
-- Instantly switch from one behavior to another.
--
switch :: Time -> Behavior a -> Behavior a -> Behavior a
switch t rx ry = switch' t rx ry ry

-- | Replace everthing before the given time by `mempty`.
trimBefore :: Monoid a => Time -> Behavior a -> Behavior a
trimBefore start x = switch start mempty x

-- | Replace everthing after the given time by `mempty`.
trimAfter :: Monoid a => Time -> Behavior a -> Behavior a
trimAfter stop x = switch stop x mempty

-- |
-- Instantly switch from one behavior to another with an optinal intermediate value.
--
switch' :: Time -> Behavior a -> Behavior a -> Behavior a -> Behavior a
switch' t rx ry rz = tabulate $ \u -> case u `compare` t of
  LT -> rx ! u
  EQ -> ry ! u
  GT -> rz ! u

concatSegment :: Monoid a => Note (Segment a) -> Behavior a
concatSegment = trim . view bounded

-- |
-- Concatenate a score of (possibly overlapping) segments.
--
-- See also 'concatB' and 'continous'.
--
concatS :: Monoid a => Score (Segment a) -> Behavior a
concatS = mconcat . map concatSegment . view notes

-- |
-- Concatenate a score of (possibly overlapping) segments.
--
-- See also 'concatSegment' and 'continous'.
--
concatB :: Monoid a => Score (Behavior a) -> Behavior a
concatB = concatS . fmap (view focusing)






-- |
--
-- The semantics are given by
--
-- @
-- type Phrase p a = (p, Voice a)
-- @
--
newtype Phrase p a = Phrase (p, [Stretched a])
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

instance HasMeta (Phrase p a) where
  meta = error "Not implemented: meta" -- TODO




-- |
-- A 'Chord' is a sequence of stretched values.
--
-- @
-- type Chord a = [Delayed a]
-- @
--
newtype Chord a = Chord { getChord :: ChordList (ChordEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

-- Can use [] or Seq here
type ChordList = []

-- Can use any type as long as chordEv provides an Iso
type ChordEv a = Delayed a

chordEv :: Iso (Delayed a) (Delayed b) (ChordEv a) (ChordEv b)
chordEv = id

instance Applicative Chord where
  pure  = return
  (<*>) = ap

instance Monad Chord where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

-- | TODO Unsafe
instance Wrapped (Chord a) where
  type Unwrapped (Chord a) = (ChordList (ChordEv a))
  _Wrapped' = iso getChord Chord

instance Rewrapped (Chord a) (Chord b)

instance Transformable (Chord a) where
  transform s = over _Wrapped' (transform s)

instance HasDuration (Chord a) where
  _duration = Foldable.sum . fmap _duration . view _Wrapped'

instance Splittable a => Splittable (Chord a) where
  -- TODO

instance Reversible a => Reversible (Chord a) where
  rev = over _Wrapped' (fmap rev) -- TODO OK?

instance HasMeta (Chord a) where
  meta = error "Not implemented: meta" -- TODO

type instance Pitch (Chord a) = Pitch a
type instance SetPitch g (Chord a) = Chord (SetPitch g a)
instance (HasPitches a b) => HasPitches (Chord a) (Chord b) where
  pitches = _Wrapped . traverse . from chordEv . _Wrapped . whilstLT pitches


chord :: Lens (Chord a) (Chord b) [Delayed a] [Delayed b]
chord = _Wrapped 






-- |
-- A 'Voice' is a sequence of stretched values.
--
-- @
-- type Voice a = [Stretched a]
-- @
--
newtype Voice a = Voice { getVoice :: VoiceList (VoiceEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

-- Can use [] or Seq here
type VoiceList = []

-- Can use any type as long as voiceEv provides an Iso
type VoiceEv a = Stretched a

voiceEv :: Iso (Stretched a) (Stretched b) (VoiceEv a) (VoiceEv b)
voiceEv = id

instance Applicative Voice where
  pure  = return
  (<*>) = ap

instance Monad Voice where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

-- | TODO Unsafe
instance Wrapped (Voice a) where
  type Unwrapped (Voice a) = (VoiceList (VoiceEv a))
  _Wrapped' = iso getVoice Voice

instance Rewrapped (Voice a) (Voice b)

instance Transformable (Voice a) where
  transform s = over _Wrapped' (transform s)

instance HasDuration (Voice a) where
  _duration = Foldable.sum . fmap _duration . view _Wrapped'

instance Splittable a => Splittable (Voice a) where
  -- TODO

instance Reversible a => Reversible (Voice a) where
  rev = over _Wrapped' (fmap rev) -- TODO OK?

instance HasMeta (Voice a) where
  meta = error "Not implemented: meta" -- TODO

type instance Pitch (Voice a) = Pitch a
type instance SetPitch g (Voice a) = Voice (SetPitch g a)
instance (HasPitches a b) => HasPitches (Voice a) (Voice b) where
  pitches = _Wrapped . traverse . from voiceEv . _Wrapped . whilstLD pitches


voice :: Lens (Voice a) (Voice b) [Stretched a] [Stretched b]
voice = _Wrapped

singleStretched :: Prism' (Voice a) (Stretched a)
singleStretched = error "Not implemented: singleStretched"

-- |
-- Voice
--
voiceNotes :: Traversal (Voice a) (Voice b) (Note a) (Note b)
voiceNotes = error "Not implemented: voiceNotes"

-- |
-- Voice
--
voiceElements :: Traversal (Voice a) (Voice b) (Stretched a) (Stretched b)
voiceElements = _Wrapped . traverse . from voiceEv

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice :: Voice a -> Voice b -> Voice (a, b)
zipVoice = zipVoiceWith (,)

-- |
-- Join the given voices by multiplying durations and combining values using the given function.
--
zipVoiceWith :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith  = error "Not implemented: zipVoiceWith"

-- |
-- Join the given voices by combining durations and values using the given function.
--
dzipVoiceWith :: (Duration -> Duration -> a -> b -> (Duration, c)) -> Voice a -> Voice b -> Voice c
dzipVoiceWith = error "Not implemented: dzipVoiceWith"


voiceList :: Iso' (Voice a) [(Duration, a)]
voiceList = error "Not implemented: voiceList"

-- |
-- Merge consecutive equal note.
--
mergeEqualNotes :: Eq a => Voice a -> Voice a
mergeEqualNotes = over voiceList $ fmap f . Data.List.groupBy (inspecting snd)
  where
    f dsAs = let (ds,as) = unzip dsAs in (sum ds, head as)


type ScoreNote a = Note a

-- |
--
-- You typically create a 'Score' using 'score', 'notes', 'voices', and 'phrases', or the 'Alternative' interface.
-- 
-- Score is an instance of 'Transformable', so you can use 'delay' and 'stretch'.
-- 
-- Score is an instance of 'HasPosition', so you can use 'duration', 'onset', 'offset', 'era'.
--
-- To inspect or deconstruct a score, see 'notes', 'voices', and 'phrases', as
-- well as 'singleNote', 'singleVoice', and 'singlePhrase'
--
-- The semantics are given by
--
-- @
-- type Score a = [Note a]
-- @
--
newtype Score a = Score { getScore :: [ScoreNote a] }
  deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

--   * 'empty' creates an empty score 
-- 
--   * 'pure' creates a score containing a single note in the span @0 '<->' 1@
-- 
--   * '<|>' composes scores in parallel
-- 
--   * '|>' composes scores as a forward sequence
-- 
--   * '<|' composes scores as a backward sequence
--
-- You can also use '<>' and 'mempty' of course.
-- 

-- | TODO Unsafe
instance Wrapped (Score a) where
  type Unwrapped (Score a) = [ScoreNote a]
  _Wrapped' = iso getScore Score

instance Rewrapped (Score a) (Score b)

instance Applicative Score where
  pure  = return
  (<*>) = ap

instance Monad Score where
  return = (^. _Unwrapped) . pure . pure
  xs >>= f = (^. _Unwrapped) $ mbind ((^. _Wrapped') . f) ((^. _Wrapped') xs)

instance Alternative Score where
  empty = mempty
  (<|>) = mappend

instance MonadPlus Score where
  mzero = mempty
  mplus = mappend

instance FunctorWithIndex Span Score where
  imap = mapWithSpan

instance FoldableWithIndex Span Score where
  -- TODO

instance TraversableWithIndex Span Score where
  itraverse = undefined
  -- TODO

instance IsPitch a => IsPitch (Score a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Score a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Score a) where
  fromDynamics = pure . fromDynamics

instance Transformable (Score a) where
  transform t (Score xs) = Score (fmap (transform t) xs)

instance Reversible a => Reversible (Score a) where
  rev (Score xs) = Score (fmap rev xs)

instance HasPosition (Score a) where
  _onset = Foldable.minimum . fmap _onset . view _Wrapped'
  _offset = Foldable.maximum . fmap _offset . view _Wrapped'

instance HasDuration (Score a) where
  _duration x = _offset x .-. _onset x

instance Splittable a => Splittable (Score a) where
  -- TODO

instance HasMeta (Score a) where
  meta = error "Not implemented: meta" -- TODO


type instance Pitch (Score a) = Pitch a
type instance SetPitch g (Score a) = Score (SetPitch g a)

instance (HasPitches a b) => HasPitches (Score a) (Score b) where
  pitches = _Wrapped . traverse . _Wrapped . whilstL pitches

type instance Part (Score a) = Part a
type instance SetPart g (Score a) = Score (SetPart g a)

instance (HasParts a b) => HasParts (Score a) (Score b) where
  parts = _Wrapped . traverse . _Wrapped . whilstL parts

type instance Dynamic (Score a) = Dynamic a
type instance SetDynamic g (Score a) = Score (SetDynamic g a)

instance HasDynamics a b => HasDynamics (Score a) (Score b) where
  dynamics = _Wrapped . traverse . _Wrapped . whilstL dynamics

type instance Articulation (Score a) = Articulation a
type instance SetArticulation g (Score a) = Score (SetArticulation g a)

instance (HasArticulations a b) => HasArticulations (Score a) (Score b) where
  articulations = _Wrapped . traverse . _Wrapped . whilstL articulations

-- |
-- Create a score from a list of notes.
--
-- This is a getter (rather than a function) for consistency:
--
-- @
-- [ (1 \<-> 2, 1)^.note, 
--   (3 \<-> 4, 2)^.note ]^.score
-- @
-- 
-- @
-- view score $ fmap (view note) $ [(0 \<-> 1, 1)]
-- @
--
-- Se also 'notes'.
--
score :: Getter [Note a] (Score a)
score = to $ \x -> empty & notes .~ x

-- |
-- View a score as a list of notes.
--
-- This is not an 'Iso', as the note list representation does not contain meta-data.
-- To construct a score from a note list, use 'score' ('from' 'notes' does not work).
-- 
notes :: Lens (Score a) (Score b) [Note a] [Note b]
notes = _Wrapped

-- |
-- View a score as a list of voices.
-- 
voices :: Lens (Score a) (Score b) [Voice a] [Voice b]

-- |
-- View a score as a list of phrases.
-- 
phrases :: Lens (Score a) (Score b) [[Voice a]] [[Voice b]]
(voices, phrases) = error "Not implemented: (voices, phrases)"

-- |
-- View a score as a single note.
-- 
singleNote :: Prism' (Score a) (Note a)
singleNote = error "Not implemented: singleNote"

-- |
-- View a score as a single voice.
-- 
singleVoice :: Prism' (Score a) (Voice a)
singleVoice = error "Not implemented: singleNote"

-- |
-- View a score as a single phrase.
-- 
singlePhrase :: Prism' (Score a) (Phrase () a)
singlePhrase = error "Not implemented: singlePhrase"

-- | Map with the associated time span.
mapScore :: (Note a -> b) -> Score a -> Score b
mapScore f = error "Not implemented: singleNote"

-- | Map over the values in a score.
mapWithSpan :: (Span -> a -> b) -> Score a -> Score b
mapWithSpan f = mapScore (uncurry f . _getNote)

-- | Filter the values in a score.
filterWithSpan :: (Span -> a -> Bool) -> Score a -> Score a
filterWithSpan f = mapFilterWithSpan (partial2 f)

-- | Combination of 'mapEvents' and 'filterEvents'.
mapFilterWithSpan :: (Span -> a -> Maybe b) -> Score a -> Score b
mapFilterWithSpan f = mcatMaybes . mapWithSpan f

-- | Map over the values in a score.
mapEvents :: (Time -> Duration -> a -> b) -> Score a -> Score b
mapEvents f = mapWithSpan (uncurry f . view delta)

-- | Filter the values in a score.
filterEvents   :: (Time -> Duration -> a -> Bool) -> Score a -> Score a
filterEvents f = mapFilterEvents (partial3 f)

-- | Efficient combination of 'mapEvents' and 'filterEvents'.
mapFilterEvents :: (Time -> Duration -> a -> Maybe b) -> Score a -> Score b
mapFilterEvents f = mcatMaybes . mapEvents f


-- 
-- TODO
-- 
-- instance Cons (Voice a) (Voice b) (Stretched a) (Stretched b) where
--   _Cons = prism (\(s,v) -> stretchedToVoice s <> v) $ \v -> case uncons (unwr v) of
--     Just (x,xs) -> Right (x,wr xs)
--     Nothing   -> Left mempty
-- 
-- type instance Index (Voice a) = Int
-- type instance IxV (Voice a) = Stretched a
-- instance Ixed (Voice a) where
--   ix n = _Wrapped' . ix n
-- 
{-

-- |
-- The 'Voices' and 'Phrases' types represent a sequence of voices and sub-voices with possibly infinite division.
--
newtype Voices a = Voices (NonEmpty (Voice a)) deriving (Functor, Foldable, Traversable)

instance Applicative Voices where
  pure  = return
  (<*>) = ap

instance Monad Voices where
  -- TODO

instance Transformable (Voices a) where

instance Reversible a => Reversible (Voices a) where

instance HasDuration (Voices a) where

instance Splittable a => Splittable (Voices a) where

voices' :: Traversal'
  (Voices a)
  (Phrases a)
-- TODO is there a non-empty version of traversal?
voices' = error "Not implemented: voices'"

newtype Phrases a = Phrases { getPhrases :: Seq (Stretched a) }
  deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Semigroup, Monoid)

instance Applicative Phrases where
  pure  = return
  (<*>) = ap

instance Monad Phrases where
  -- TODO

instance Transformable (Phrases a) where

instance Reversible a => Reversible (Phrases a) where

instance HasDuration (Phrases a) where

instance Splittable a => Splittable (Phrases a) where

phrases' :: Traversal'
  (Phrases a)
  (Either (Voice a) (Voices a))
phrases' = error "Not implemented: phrases'"


concatVoices :: Monoid a => Phrases a -> Voice a
concatVoices = error "Not implemented: concatVoices"

-}


{-

-- |
-- Represents past and future points of change, relative time zero.
--
-- A value must have the form @Changes [a0..aN-1,aN] [b0,b1..bN] where 
-- aN <= aN+1, bN <= bN+1 and aN <= 0 <= b0.
--
data Changes a = Changes [a] [a]
  deriving (Eq, Show, Typeable)

instance Ord a => Semigroup (Changes a) where
  Changes a1 b1 <> Changes a2 b2
    = Changes (a1 `merge` a2) (b1 `merge` b2)
instance Ord a => Monoid (Changes a) where
  mempty = Changes [] []
  mappend = (<>)

instance (Num a, Ord a, Transformable a) => Transformable (Changes a) where
  transform s (Changes a b) = Changes 
    (takeWhile (< 0) $ Data.List.sort $ transform s a <> transform s b)
    (dropWhile (< 0) $ Data.List.sort $ transform s a <> transform s b)

normalizeChanges :: (Num a, Ord a) => (Changes a) -> (Changes a)
normalizeChanges (Changes a b) = 
  Changes
    (takeWhile (< 0) $ Data.List.sort $ a <> b)
    (dropWhile (< 0)  $ Data.List.sort $ a <> b)

splitChanges :: (Num a, Ord a) => (Changes a) -> ((Changes a), (Changes a))
splitChanges (Changes a b) = (Changes a [0], Changes [] (0:b))
-}

-- data Rea a
--   = Constant a
--   | Switch (Rea a) Time (Rea a)
--   deriving (Show)
-- 
-- _optDropAfter t (Constant x)   = Constant x
-- _optDropAfter t (Switch a u b)
--   -- Switch occurs after t, so we can safely discard it
--   | t         <= u = _optDropAfter t a
--   | otherwise      = Switch a u $ _optDropAfter t b
-- 
-- _isConstant (Constant x) = True
-- _isConstant _            = False
-- 
-- _initial (Constant x)   = x
-- _initial (Switch a t b) = _initial a
-- 
-- _final (Constant x)   = x
-- _final (Switch a t b) = _final b


            
-- |
-- Forms an applicative as per 'Behavior', but only switches at discrete points.
--
-- The semantics are given by
--
-- @
-- type Reactive a = (a, Time, Voice a)
-- @
--
newtype Reactive a = Reactive { getReactive :: ([Time], Behavior a) }
    deriving (Functor, Semigroup, Monoid)
--
-- TODO Define a more compact representation and reimplement Behavior as (Reactive Segment).
-- 
-- Possible approach:
-- 
--  * Implement PosReactive (no negative values) and define Reactive = Delayed (PosReactive)
-- 
--  * Implement liftA2 for PosReactive (preferably with a single traversal)
-- 

instance Transformable (Reactive a) where
    transform s (Reactive (t,r)) = Reactive (transform s t, transform s r)

instance Wrapped (Reactive a) where
    type Unwrapped (Reactive a) = ([Time], Behavior a)
    _Wrapped' = iso getReactive Reactive

instance Rewrapped (Reactive a) (Reactive b)
instance Applicative Reactive where
    pure  = pureDefault
    (<*>) = apDefault

(view _Wrapped -> (tf, rf)) `apDefault` (view _Wrapped -> (tx, rx)) = view _Unwrapped (tf <> tx, rf <*> rx)
pureDefault = view _Unwrapped . pure . pure

-- |
-- Get the initial value.
--
initial :: Reactive a -> a
initial r = r `atTime` minB (occs r)
    where
        -- If there are no updates, just use value at time 0
        -- Otherwise pick an arbitrary time /before/ the first value
        -- It looks strange but it works
        minB []    = 0
        minB (x:_) = x - 1

-- |
-- Get the final value.
--
final :: Reactive a -> a
final (renderR -> (i,[])) = i
final (renderR -> (i,xs)) = snd $ last xs

occs :: Reactive a -> [Time]
occs = fst . (^. _Wrapped')

atTime :: Reactive a -> Time -> a
atTime = (!) . snd . (^. _Wrapped')

-- | Get the time of all updates and the value switched to at this point.
updates :: Reactive a -> [(Time, a)]
updates r = (\t -> (t, r `atTime` t)) <$> (Data.List.sort . Data.List.nub) (occs r)

renderR :: Reactive a -> (a, [(Time, a)])
renderR = initial &&& updates

-- | @switch t a b@ behaves as @a@ before time @t@, then as @b@.
switchR :: Time -> Reactive a -> Reactive a -> Reactive a
switchR t (Reactive (tx, bx)) (Reactive (ty, by)) = Reactive $ (,)
    (filter (< t) tx <> [t] <> filter (> t) ty) (switch t bx by)

-- |
-- Get all intermediate values.
--
intermediate :: Transformable a => Reactive a -> [Note a]
intermediate (updates -> []) = []
intermediate (updates -> xs) = fmap (\((t1, x), (t2, _)) -> (t1 <-> t2, x)^.note) $ withNext $ xs
  where
    withNext xs = zip xs (tail xs)

-- |
-- Realize a 'Reactive' value as a discretely changing behavior.
--
discrete :: Reactive a -> Behavior a
discrete = continous . fmap pure

-- |
-- Realize a 'Reactive' value as an continous behavior.
--
-- See also 'concatSegment' and 'concatB'.
--
continous :: Reactive (Segment a) -> Behavior a

-- |
-- Realize a 'Reactive' value as an continous behavior.
--
-- See also 'concatSegment' and 'concatB'.
--
continousWith :: Segment (a -> b) -> Reactive a -> Behavior b
continousWith f x = continous $ liftA2 (<*>) (pure f) (fmap pure x)

-- |
-- Sample a 'Behavior' into a reactive.
--
sample   :: [Time] -> Behavior a -> Reactive a

-- TODO linear approximation
(continous, sample) = error "Not implemented: (discrete, continous, sample)"


window :: [Time] -> Behavior a -> Reactive (Segment a)
windowed :: Iso (Behavior a) (Behavior b) (Reactive (Segment a)) (Reactive (Segment b))
(window, windowed) = error "Not implemented: (window, windowed)"

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

-- Like Data.Ord.comparing
-- (Are both variants of contramap?)
inspecting :: Eq a => (b -> a) -> b -> b -> Bool
inspecting p x y = p x == p y

-- Same as @flip const@, useful to fix the type of the first argument.
assuming :: a -> b -> b
assuming = flip const

sameType :: a -> a -> a
sameType _ x = x
(~~) = sameType
infixl 0 ~~

#ifdef INCLUDE_TESTS

-- Tests

-- Bad examples (to verify tests)

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


-- Serial instances

instance Monad m => CoSerial m Time where
  coseries = liftM const -- TODO?

instance Monad m => CoSerial m Duration where
  coseries = liftM const -- TODO?

instance (Monad m, CoSerial m a, Ord a, Num a) => CoSerial m (Clipped a) where
  coseries = fmap (. fromClipped) . coseries

-- instance (Monad m, Num a, Ord a, Serial m a) => Serial m (Changes a) where
  -- series = fmap normalizeChanges $ cons2 Changes

instance Monad m => Serial m Time where
  -- series = msum $ fmap return [-1,0,2.13222]
  series = newtypeCons Time

instance Monad m => Serial m Duration where
  -- series = msum $ fmap return [-1,0,1.51232]
  series = newtypeCons Duration

instance Monad m => Serial m Span where
  series = newtypeCons Delta

instance (Monad m, Serial m a) => Serial m (BadFunctor a) where
  series = cons0 BF1 \/ cons0 BF2

instance (Monad m, Serial m a) => Serial m (BadMonoid a) where
  series = newtypeCons BadMonoid

instance Monad m => Serial m Int8 where
  series = msum $ fmap return [0..2]

instance (Monad m, Serial m a) => Serial m (Note a) where
  series = newtypeCons Note

instance (Monad m, Serial m a) => Serial m (Bound a) where
  series = newtypeCons Bound

instance (Monad m, Serial m a) => Serial m (NoReverse a) where
  series = newtypeCons NoReverse

instance (Monad m, Serial m a) => Serial m (Delayed a) where
  series = newtypeCons Delayed

instance (Monad m, Serial m a) => Serial m (Stretched a) where
  series = newtypeCons Stretched

instance (Monad m, Serial m a) => Serial m (Behavior a) where
  series = newtypeCons Behavior

instance (Monad m, Serial m a) => Serial m (Segment a) where
  series = newtypeCons Segment

instance (Monad m, Serial m a) => Serial m (Voice a) where
  series = do
    x <- series
    y <- series
    return $ return x <> return y

instance (Monad m, Serial m a) => Serial m (Score a) where
  series = do
    x <- series
    y <- series
    return $ return x <> return y


v ^+. p = p .+^ v

-- transform mempty = id
-- transform (s <> t) = transform s . transform t

-- > _onset (delay n a) = n ^+. _onset a
transformMonoidMorphism = transformMonoidMorphismEq (==)

transformMonoidMorphismEq (===) typ = testGroup ("Transform is a Monoid morphism " ++ show (typeOf typ)) $ [
  testProperty 
    "transform mempty = id" $ \x -> assuming (typ ~~ x) $
     transform mempty x === x,
  testProperty 
    "transform (s <> t) = transform s . transform t" $ \(s :: Span) (t :: Span) x -> assuming (typ ~~ x) $
     transform (s <> t) x === (transform s . transform t) x
  ]


-- > _onset (delay n a) = n ^+. _onset a
delayOnsetLaw typ = testGroup ("Delay/onset " ++ show (typeOf typ)) $ [
  testProperty 
    "_onset (delay n a) == n ^+. _onset a" $ \(n :: Duration) a -> assuming (typ ~~ a) $
     _onset (delay n a) == n ^+. _onset a
  ]

-- > _offset (delay n a) = n ^+. _offset a
delayOffsetLaw typ = testGroup ("Delay/offset " ++ show (typeOf typ)) $ [
  testProperty 
    "_offset (delay n a) == n ^+. _offset a" $ \(n :: Duration) a -> assuming (typ ~~ a) $
     _offset (delay n a) == n ^+. _offset a
  ]

-- > duration (stretch n a) = n ^* (duration a)
stretchDurationLaw typ = testGroup ("Stretch/duration " ++ show (typeOf typ)) $ [
  testProperty 
    "_duration (stretch n a) == n ^* (_duration a)" $ \(n :: Duration) a -> assuming (typ ~~ a) $
     _duration (stretch n a) == n ^* (_duration a)
  ]

-- > duration a = duration (delay n a)
delayDurationLaw typ = testGroup ("Delay/duration " ++ show (typeOf typ)) $ [
  testProperty 
    "_duration a == _duration (delay n a)" $ \(n :: Duration) a -> assuming (typ ~~ a) $
     _duration a == _duration (delay n a)
  ]



delayIndexLaw typ = testGroup ("Delay/! " ++ show (typeOf typ)) $ [
  testProperty 
    "delay n b ! t == b ! (t .-^ n)" $ \(n :: Duration) (t :: Time) b -> assuming (typ ~~ b) $
     delay n b ! t == b ! (t .-^ n)
  ]

--  _duration x = (offset x .-. onset x)
durationOnsetOffsetLaw typ = testGroup ("Duration/onset/offset " ++ show (typeOf typ)) $ [
  testProperty
    "_duration x == (_offset x .-. _onset x)" $ \x y -> assuming (typ ~~ x ~~ y) $
     _duration x == (_offset x .-. _onset x)
  ]

--
-- > (t<->u) `transform` b ! t           == b ! 0
-- > (t<->u) `transform` b ! ((u-t)/2+t) == b ! 0.5
-- > (t<->u) `transform` b ! u           == b ! 1
--

transformUi typ = testGroup ("Transform UI " ++ show (typeOf typ)) $ [
  testProperty
    "(t<->u) `transform` b ! t == b ! 0" $ \(t :: Time) (u2 :: Time) -> let b = (unit::Behavior Double); u = decollide t u2 in
     (t<->u) `transform` b ! t == b ! 0,

  testProperty 
    "(t<->u) `transform` b ! ((u-t)/2+t) == b ! 0.5" $ \(t :: Time) (u2 :: Time) -> let b = (unit::Behavior Double); u = decollide t u2 in 
     (t<->u) `transform` b ! ((u-t)/2+t) == b ! 0.5,

  testProperty "(t<->u) `transform` b ! u == b ! 1" $ \(t :: Time) (u2 :: Time) -> let b = (unit::Behavior Double); u = decollide t u2 in
                (t<->u) `transform` b ! u == b ! 1

  ]

decollide :: (Eq a, Num a) => a -> a -> a
decollide x y
  | x == y    = y + 1
  | otherwise = y

monoid = monoidEq (==)

monoidEq (===) typ = testGroup ("instance Monoid " ++ show (typeOf typ)) $ [
  testProperty 
    "x <> (y <> z) == (x <> y) <> z" $ \x y z -> assuming (typ ~~ x)
     (x <> (y <> z)) === ((x <> y) <> z),

  testProperty 
    " mempty <> x  === x" $ \x   -> assuming (typ ~~ x)
     (mempty <> x) === x,

  testProperty
    "((x <> mempty) === x)" $ \x   -> assuming (typ ~~ x)
     ((x <> mempty) === x)
  ]
  where
    (<>) = mappend

functor typ = testGroup ("instance Functor " ++ show (typeOf typ)) $ [
  testProperty 
    "fmap id = id" $ \x -> assuming (sameType typ x) $
     fmap id x == id x
  ]

reversible = reversibleEq (==)

reversibleEq (===) typ = testGroup ("instance Reversible " ++ show (typeOf typ)) $ [
  testProperty 
    "rev . rev == id" $ \x -> assuming (sameType typ x)
    (rev (rev x)) === x,

  -- testProperty "transform . rev == fmap rev . transform" $ \(s :: Span) x -> assuming (sameType typ x)
                -- ((transform . rev) s x) === ((fmap rev . transform) s x),

  testGroup "" []
  ]

reversibleDur = reversibleDurEq (==)
reversibleDurEq (===) typ = testProperty 
  "abs . _duration . rev = _duration" $ \x -> assuming (sameType typ x)
  abs (_duration x) === _duration (rev x)


splittable = splittableEq (==)

splittableEq (===) typ = testGroup ("instance Splittable " ++ show (typeOf typ)) $ [
  testProperty 
    "_duration (beginning t x) + _duration (ending t x) =   _duration x" $ \(t :: Duration) x -> assuming (sameType typ x)
     (_duration (beginning t x) + _duration (ending t x)) === _duration x,

  testProperty 
    "_duration (beginning t x) =   t `min` _duration x" $ \(t :: Duration) x -> assuming (sameType typ x)
     _duration (beginning t x) === (t `min` _duration x),

  -- testProperty "transform . rev == fmap rev . transform" $ \(s :: Span) x -> assuming (sameType typ x)
                -- ((transform . rev) s x) === ((fmap rev . transform) s x),

  testGroup "" []
  ]



main = defaultMain $ testGroup "All tests" $ [
  testGroup "Monoid" [
    monoid (undefined :: ()),
    monoid (undefined :: Duration),
    monoid (undefined :: Time),
    monoid (undefined :: Span),
    monoidEq (\x y -> x ! 0 == y ! 0 && x ! 1 == y ! 1) (undefined :: Segment Time),
    monoidEq (\x y -> x ! 0 == y ! 0 && x ! 1 == y ! 1) (undefined :: Behavior Time),
    -- Reactive
    -- Phrase
    monoid (undefined :: Voice Time),
    monoid (undefined :: Score Time)
  ],

  testProperty "============================================================" $ True,  
  testGroup "Functor" [
    functor (undefined :: [()]),
    functor (undefined :: Note Time),
    functor (undefined :: Delayed Time),
    functor (undefined :: Stretched Time),
    functor (undefined :: Voice Time),
    functor (undefined :: Score Time)
  ],

  testProperty "============================================================" $ True,  
  testGroup "Transformable: Monoid morphism" [
    transformMonoidMorphism   (undefined :: Delayed Time),
    transformMonoidMorphism   (undefined :: Stretched Time),
    transformMonoidMorphism   (undefined :: Note Time),
    transformMonoidMorphismEq 
      (\x y -> fmap (! 0) x == fmap (! 0) y && fmap (! 1) x == fmap (! 1) y)
      (undefined :: Bound (Behavior Time)),
    -- transformMonoidMorphism   (undefined :: Changes Time),
    transformMonoidMorphism   (undefined :: Voice Time),
    transformMonoidMorphism   (undefined :: Score Time)
  ],
  
  testProperty "============================================================" $ True,  
  testGroup "Transformable: Delay and stretch" [
    
    delayOnsetLaw      (undefined :: Delayed Time),
    delayOffsetLaw     (undefined :: Delayed Time),
    -- stretchDurationLaw (undefined :: Delayed Time),
    -- delayDurationLaw   (undefined :: Delayed Time),

    -- delayOnsetLaw      (undefined :: Stretched Time),
    -- delayOffsetLaw     (undefined :: Stretched Time),
    stretchDurationLaw (undefined :: Stretched Time),
    delayDurationLaw   (undefined :: Stretched Time),
    
    delayOnsetLaw      (undefined :: Note Time),
    delayOffsetLaw     (undefined :: Note Time),
    stretchDurationLaw (undefined :: Note Time),
    delayDurationLaw   (undefined :: Note Time),

    -- delayOnsetLaw      (undefined :: Bound (Behavior Time)),
    -- delayOffsetLaw     (undefined :: Bound (Behavior Time)),
    -- stretchDurationLaw (undefined :: Bound (Behavior Time)),
    -- delayDurationLaw   (undefined :: Bound (Behavior Time)),
    
    -- delayOnsetLaw      (undefined :: Voice Time),
    -- delayOffsetLaw     (undefined :: Voice Time),
    stretchDurationLaw (undefined :: Voice Time),
    delayDurationLaw   (undefined :: Voice Time),
    
    delayOnsetLaw      (undefined :: Score Time),
    delayOffsetLaw     (undefined :: Score Time),
    stretchDurationLaw (undefined :: Score Time),
    delayDurationLaw   (undefined :: Score Time)
  ],

  testProperty "============================================================" $ True,  
  testGroup "HasPosition/HasDuration" [
    durationOnsetOffsetLaw  (undefined :: Span),
    durationOnsetOffsetLaw  (undefined :: Time),
    -- durationOnsetOffsetLaw  (undefined :: [Time]), -- too slow
    durationOnsetOffsetLaw  (undefined :: Delayed Time),
    durationOnsetOffsetLaw  (undefined :: Note Time),
    durationOnsetOffsetLaw  (undefined :: Bound Time),
    durationOnsetOffsetLaw  (undefined :: Score Time)    
  ],

  -- delayIndexLaw (undefined :: Behavior Int8),
                             
  testProperty "============================================================" $ True,  
  testGroup "Reversible" [
    reversible (undefined :: ()),
    reversible (undefined :: Double),
    reversible (undefined :: Int),
    reversible (undefined :: Integer),
    reversible (undefined :: Duration),
    reversible (undefined :: Span),

    reversible (undefined :: [Duration]),
    -- reversible (undefined :: Seq Duration),

    reversibleEq (\x y -> x ! 0 == y ! 0 && x ! 1 == y ! 1) (undefined :: Behavior Duration),
    reversibleEq (\x y -> x ! 0 == y ! 0 && x ! 1 == y ! 1) (undefined :: Segment Time),

    reversible (undefined :: Note Duration),
    reversible (undefined :: Stretched Duration),
    reversible (undefined :: Delayed Duration),

    reversible (undefined :: NoReverse Duration),
    reversible (undefined :: Bound Duration),

    reversible (undefined :: Voice Duration),
    reversible (undefined :: Score Duration),
    
    
    -- reversibleDur (undefined :: [Duration]),
    -- reversibleDur (undefined :: Seq Duration),
    -- reversibleDurEq (\x y -> x ! 0 == y ! 0 && x ! 1 == y ! 1) (undefined :: Behavior Duration),
    -- reversibleDurEq (\x y -> x ! 0 == y ! 0 && x ! 1 == y ! 1) (undefined :: Segment Time),

    reversibleDur (undefined :: Note Duration),
    reversibleDur (undefined :: Stretched Duration),
    reversibleDur (undefined :: Delayed Duration),

    -- reversibleDur (undefined :: NoReverse Duration),
    -- reversibleDur (undefined :: Bound Duration),

    reversibleDur (undefined :: Voice Duration),
    reversibleDur (undefined :: Score Duration)
    
  ],

  testProperty "============================================================" $ True,  
  testGroup "Splittable" [
    splittable (undefined :: Duration),
    splittable (undefined :: Span),

    -- splittableEq (\x y -> x ! 0 == y ! 0 && x ! 1 == y ! 1) (undefined :: Behavior Duration),
    -- splittableEq (\x y -> x ! 0 == y ! 0 && x ! 1 == y ! 1) (undefined :: Segment Time),

    splittable (undefined :: Note Duration),
    splittable (undefined :: Note (Note Duration)),
    splittable (undefined :: Stretched Duration),
    splittable (undefined :: Delayed Duration),

    -- splittable (undefined :: Bound Duration),

    splittable (undefined :: Voice Duration),
    splittable (undefined :: Score Duration)
    
  ],

  testProperty "============================================================" $ True,  
    testGroup "Nothing" []
  ]




-- PD test
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



-- test = openG $ drawBehavior (r*5) <> lc blue (drawBehavior (c1*5)) <> drawNote (fmap (fmap snd) nc)
  -- where
    -- c = 1
c1 = sin (time/20*2*pi)


newtype PD = PD { getPD :: (Behavior Float, Behavior Float) }
  deriving (Eq, Ord, Show)
instance Wrapped PD where
  type Unwrapped PD = (Behavior Float, Behavior Float)
  _Wrapped' = iso getPD PD
instance Rewrapped PD PD
type instance Pitch PD = Behavior Float
type instance SetPitch g PD = PD
type instance Dynamic PD = Behavior Float
type instance SetDynamic g PD = PD

instance Transformable PD where
  transform s (PD x) = PD ((transform s *** transform s) x)
instance HasPitches PD PD where
  pitches = _Wrapped . _1
instance HasPitch PD PD where
  pitch = _Wrapped . _1
instance HasDynamics PD PD where
  dynamics = _Wrapped . _2
instance HasDynamic PD PD where
  dynamic = _Wrapped . _2




class Drawable a where
  draw :: Renderable (Path R2) b => a -> Diagram b R2
instance Real a => Drawable (Behavior a) where
  draw = drawBehavior
instance Real a => Drawable (Segment a) where
  draw = drawSegment
-- instance Drawable a => Drawable (Note a) where
  -- draw (view from note -> (s,x)) = draw x
-- instance Real a => Drawable (Span, a) where
--   draw = uncurry drawSegmentAt . fmap return
-- instance Real a => Drawable (Span, a) where
--   draw = uncurry drawBehaviorAt . fmap return
instance Real a => Drawable (Span, Segment a) where
  draw = uncurry drawSegmentAt
instance Real a => Drawable (Span, Behavior a) where
  draw = uncurry drawBehaviorAt
instance Drawable a => Drawable [a] where
  draw = mconcat . fmap draw

instance (Drawable (Span, Pitch a), Drawable (Span, Dynamic a), HasPitch a a, HasDynamic a a, Transformable a) => Drawable (Stretched a) where
  draw (view (from stretched) -> (d,x)) = draw $ (0 >-> d, x)^.note
instance (Drawable (Span, Pitch a), Drawable (Span, Dynamic a), HasPitch a a, HasDynamic a a, Transformable a) => Drawable (Note a) where
  draw (view (from note) -> (s, transform s -> x)) = lc red (draw (s, x^.pitch)) <> lc blue (draw (s, x^.dynamic))
instance (Drawable (Span, Pitch a), Drawable (Span, Dynamic a), HasPitch a a, HasDynamic a a, Transformable a) => Drawable (Score a) where
  draw = mconcat . fmap draw . view notes


drawPD pd = lc red (drawBehavior $ pd^.pitch) <> lc blue (drawBehavior $ pd^.dynamics)


drawPDNote :: (Renderable (Path R2) b, Real a) => Note PD -> Diagram b R2
drawPDNote (view (from note) -> (s, transform s -> pd)) = lc red (drawBehaviorAt s $ pd^.pitch) <> lc blue (drawBehaviorAt s $ pd^.dynamic)

testPD = openG $ (mconcat $ fmap draw notes2) 
  <> lc pink (draw $ pitchCurve) 
  <> lc lightblue (draw $ dynCurve)
  where   
    notes2 = notes :: [Note PD]
    notes  = [note1, note2] & dynamics' *~ dynCurve & pitches' *~ pitchCurve

    pitchCurve = delay 1 $ stretch 20 sine
    dynCurve   = delay 1 $ delay 3 unit
    note1      = delay 1 $ delay 0.5 $ stretch 3 $ pure $ PD (cosine,sine)
    note2      = delay 1 $ delay 7  $ stretch 1 $ pure $ PD (cosine,sine)
  






-- Drawing


{-
drawScore' :: (Renderable (Path R2) b, Real a) =>     [[(Time, Duration, a)]] -> Diagram b R2
drawScore' = vcat' (def & sep .~ 2) . fmap drawPart'

drawPart' :: (Renderable (Path R2) b, Real a) =>    [(Time, Duration, a)] -> Diagram b R2
drawPart' = mconcat . fmap drawNote'

drawNote' :: (Renderable (Path R2) b, Real a) => (Time, Duration, a) -> Diagram b R2
drawNote' (realToFrac -> t, realToFrac -> d, realToFrac -> y) = translateY y $ translateX t $ scaleX d noteShape
  where
  noteShape = {-showOr $-} lcA transparent $ fcA (blue `withOpacity` 0.5)
    $ strokeLoop $ closeLine $ fromOffsets $ fmap r2 $ [(1.2,0), (-0.2,0.2),(-0.8,0.2), (-0.2,0.6),(-0.2,-1)]
-}

drawBehavior :: (Renderable (Path R2) b, Real a) =>  Behavior a -> Diagram b R2
drawBehavior = drawBehavior' 0 10

drawBehaviorAt :: (Renderable (Path R2) b, Real a) => Span -> Behavior a -> Diagram b R2
drawBehaviorAt s@(view delta -> (realToFrac -> t, realToFrac -> d)) 
  -- = drawBehavior' t d
  = translateX t . scaleX d . drawBehavior' 0 1 . transform (negateV s) 

drawSegmentAt :: (Renderable (Path R2) b, Real a) => Span -> Segment a -> Diagram b R2
drawSegmentAt s@(view delta -> (realToFrac -> t, realToFrac -> d)) 
  -- = drawBehavior' t d
  = translateX t . scaleX d . drawBehavior' 0 1 . transform (negateV s) 

drawSegment :: (Renderable (Path R2) b, Real a) =>  Segment a -> Diagram b R2
drawSegment = scaleX 10 . drawBehavior' 0 1

-- Draw a behavior or a segment in the given span
drawBehavior'
  :: (Fractional (Rep f), Real a, TrailLike b, HasStyle b, Representable f, V b ~ R2) =>
     Double -> Double -> f a -> b
drawBehavior' start count b = draw points & lw 0.02
  where
    points = take (ceiling $ sampleRate*count) $ fmap (\x -> p2 (x, fromVal (b ! toTime x))) [start,start+sampleLength..]
    toTime = realToFrac
    fromVal = realToFrac
    
    sampleRate = 90
    sampleLength = 1/sampleRate
    
    -- draw = cubicSpline False
    -- TODO offset without showing
    draw = fromOffsets . (\xs -> zipWith (.-.) (tail xs) xs) . ((p2 (0,0)) :)

grid = grid' 20 <> fc lightblue (circle 0.1) <> translateX 5 (fc lightblue (circle 0.1)) <> translateX 10 (fc lightblue (circle 0.1))
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
  system $ "osascript -e 'tell application \"Google Chrome\" "
    ++ "to tell the active tab of its first window' -e 'reload' -e 'end tell'"
  return ()


testNotes j = let
  staff n = drawSomeNotes
    (take 60 $ drop n $ cycle $ fmap (subtract 5)
    $ [1,8,2,7,2,3,7,6,5,1,7,6,7,4,8,2,6,3,6,
       8,9,7,2,6,3,8,9,7,6,3,9,8,7,4,6,2,3,7,3,7])
  in
  openG $ vcat $ [staff (i+j) <> strutY 12 | i <- [1..8]]

-- drawSomeNotes [0,1,-3,5]
drawSomeNotes notes = (mconcat $ fmap (note.p2.((_1 %~ (*3)) . (_2 %~ (*0.5)))) $ zip [0..] notes) <> lines
  where
    -- pos is offset in spaces
    note pos = moveTo pos $ fc black $ rotateBy (1/15) $ scaleX 1.4 (circle 0.5)
    lines = translateX (linesWidth/2) $ translateY 2 $ vcat $ replicate 5 $ (lw 0.1 $ hrule (linesWidth+4) <> strutY 1)
    linesWidth = 3 * fromIntegral (length notes)

#endif // INCLUDE_TESTS










toDouble :: Real a => a -> Double
toDouble = realToFrac

-- | Merge lists.
-- > category: List
-- > depends: base
merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

-- | Merge lists.
-- > category: List
-- > depends: base
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f = mergeBy' $ (fmap.fmap) orderingToBool f
    where
        orderingToBool LT = True
        orderingToBool EQ = True
        orderingToBool GT = False

mergeBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeBy' pred xs []         = xs
mergeBy' pred [] ys         = ys
mergeBy' pred (x:xs) (y:ys) =
    case pred x y of
        True  -> x: mergeBy' pred xs (y:ys)
        False -> y: mergeBy' pred (x:xs) ys

through :: Applicative f => 
  Lens' s a 
  -> Lens s t a b 
  -> Lens (f s) (f t) (f a) (f b)
through lens1 lens2 = lens getter (flip setter)
  where
    getter = fmap (view lens1)
    setter = liftA2 (over lens2 . const)
{-# INLINE through #-}

{-
  TODO check

    openG $ drawNote $ rev $ delay 1 $ return 0
    openG $ drawNote $ delay 1 $ rev $ return 0

    openG $ drawNote $ rev $ stretch 0.5 $ return 0
    openG $ drawNote $ stretch 0.5 $ rev $ return 0

    openG $ drawNote $ rev $ transform (3 <-> 2) $ return 0
    openG $ drawNote $ transform (3 <-> 2) $ rev $ return 0

    openG $ drawBehavior $ delay 3 $ rev $ delay 1 $ unit
    openG $ drawBehavior $ delay 3 $ delay 1 $ rev $ unit

    openG $ drawBehavior $ delay 3 $ rev $ stretch 0.5 $ unit
    openG $ drawBehavior $ delay 3 $ stretch 0.5 $ rev $ unit

    openG $ drawBehavior $ delay 3 $ rev $ transform (3 <-> 2) $ unit
    openG $ drawBehavior $ delay 3 $ transform (3 <-> 2) $ rev $ unit

-}


floor' :: RealFrac a => a -> a
floor' = fromIntegral . floor

-- TODO mo
partial2 :: (a -> b      -> Bool) -> a -> b      -> Maybe b
partial3 :: (a -> b -> c -> Bool) -> a -> b -> c -> Maybe c
partial2 f = curry  (fmap snd  . partial (uncurry f))
partial3 f = curry3 (fmap (view _3) . partial (uncurry3 f))

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 = curry . curry . (. tripl)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 = (. untripl) . uncurry . uncurry

untripl :: (a,b,c) -> ((a,b),c)
untripl (a,b,c) = ((a,b),c)

tripl :: ((a,b),c) -> (a,b,c)
tripl ((a,b),c) = (a,b,c)

