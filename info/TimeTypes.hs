
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

module TimeTypes (
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
        retabulated,

        -- * Music.Time.Transform
        -- * The Transformable class
        Transformable(..),
        -- ** Apply under a transformation
        whilst,
        -- underM,
        -- underW,
        -- conjugate,

        -- ** Specific transformations
        delay,
        -- delayTime,
        undelay,
        stretch,
        compress,
        delaying,
        undelaying,
        stretching,
        compressing,

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

        -- * Music.Time.Reverse
        -- * The Reversible class
        Reversible(..),
        revDefault,
        reversed,
        NoReverse(..),

        -- * Music.Time.Split
        -- * The Splittable class
        Splittable(..),

        -- * Music.Time.Combinators
        -- * Align without composition
        lead,
        follow,
        -- * Align and compose
        after,
        before,
        during,
        sustain,
        
        -- ** Composition operators
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
        (<->),
        (>->),
        range,
        delta,

        -- ** Points in spans
        inside,
        encloses,
        -- isBefore,
        overlaps,

        -- * Music.Time.Stretched
        Stretched,
        stretched,
        stretchedValue,


        -- * Music.Time.Delayed
        Delayed,
        delayed,
        delayedValue,


        -- * Music.Time.Note
        Note,
        note,
        noteValue,


        -- * Music.Time.Bounds
        Bounds,
        bounds,
        bounding,
        trim,
        bounded,

        -- * Music.Time.Segment
        Segment,
        focus,
        focusOn,
        focused,
        focusedOn,
        appendSegment,
        concatSegment,

        -- * Music.Time.Behavior
        Behavior,
        -- ** Examples
        -- $musicTimeBehaviorExamples
        (!^),
        behavior,

        -- * Common behaviors
        time,
        unit,
        impulse,
        turnOn,
        turnOff,
        sawtooth,

        sine,
        cosine,

        -- * Combinators
        switch,
        switch3,
        splice,
        concatBehavior,

        -- * Music.Time.Voice
        Voice,
        voiceNotes,
        voiceElements,
        -- singleStretched,
        zipVoice,
        zipVoiceWith,
        dzipVoiceWith,
        mergeEqualNotes,

        -- * Music.Time.Phrases
        Voices,
        voices',
        Phrases,
        phrases',
        concatVoices,

        -- * Music.Time.Reactive
        Reactive(..),
        initial,
        final,
        updates,

        -- * Music.Time.Score
        Score,
        voices,
        phrases,
        singleNote,
        notes,
        mapWithSpan,
        filterWithSpan,
        mapFilterWithSpan,
        mapEvents,
        filterEvents,
        mapFilterEvents,



        -- * Music.Score.Pitch
        -- * Pitch type functions
        Pitch,
        SetPitch,
        Interval,
        Transposable,
        -- Transposable',
        -- * Pitch lens and traversal
        HasPitches(..),
        HasPitch(..),
        pitch',
        pitches',
        -- * Pitch manipulation
        -- ** Transposition
        up,
        down,
        above,
        below,
        invertPitches,
        -- octavesUp,
        -- octavesDown,
        -- octavesAbove,
        -- octavesBelow,
        -- ** Intervals
        augmentIntervals,
        -- TODO gliss etc

        -- * Music.Score.Dynamic
        Dynamic,
        SetDynamic,
        HasDynamics(..),
        HasDynamic(..),
        dynamic',
        dynamics',
        Level,
        Attenuable,
        -- Attenuable',
        louder,
        softer,
        level,
        fadeIn,
        fadeOut,

        -- * Music.Score.Articulation
        Articulation,
        SetArticulation,
        HasArticulations(..),
        HasArticulation(..),
        articulation',
        articulations',

        accent,
        marcato,
        accentLast,
        marcatoLast,
        accentAll,
        marcatoAll,
        tenuto,
        staccato,
        legato,

        -- * Music.Score.Part
        Part,
        SetPart,
        HasParts(..),
        HasPart(..),
        HasPart',
        HasParts',
        part',
        parts',
        allParts,
        extractPart,
        extractParts,
  ) where

import qualified Data.ByteString.Lazy         as ByteString
import           Data.Default
import           Data.Ratio
import qualified Diagrams.Backend.SVG         as SVG
import           Diagrams.Prelude             hiding (Duration, Dynamic,
                                               Segment, Time, Transformable,
                                               after, atTime, clipped, duration,
                                               during, era, era, interval, inv,
                                               offset, place, position, start,
                                               stretch, stretchTo, transform,
                                               trim, under, unit, value, view,
                                               toDuration, fromDuration,
                                               toTime, fromTime,
                                               (<->), (|>))
import           System.Process               (system)
import           Text.Blaze.Svg.Renderer.Utf8 (renderSvg)


import           Control.Applicative
import           Control.Arrow                (first, second, (***))
import qualified Control.Category
import           Control.Comonad
import           Control.Comonad.Env
import           Control.Lens                 hiding (Indexable, Level, above,
                                               below, index, inside, parts,
                                               reversed, transform, under, (|>))
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Plus
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
import           Data.Semigroup
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Traversable             (Traversable)
import qualified Data.Traversable             as T
import           Data.Typeable
import           Data.VectorSpace
import           Music.Dynamics.Literal
import           Music.Pitch.Literal

import           Data.Int
import           Test.SmallCheck.Series       hiding (NonEmpty, (><))
import           Test.Tasty                   hiding (over, under)
import           Test.Tasty.SmallCheck        hiding (over, under)

import qualified Data.Ratio                   as Util_Ratio


{-
  Semantics:

    Duration    ≡ R
    Time        ≡ R
    Span        ≡ (R, R)

    Stretched a ≡ (Duration, a)
    Delayed a   ≡ (Duration, a)
    Note a      ≡ (Span, a)
    Bounds a    ≡ (Time, Time, a)

    Voice a     ≡ [Stretched a]
    Track a     ≡ [Delayed a]
    Score a     ≡ [Note a]

    Segment a   ≡ Duration -> a
    Behavior a  ≡ Time -> a
    Reactive a  ≡ ([Time], Behavior ae)

-}



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
  recip _ = error "Can not take reciprocal of a clippedd value other than 1"
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

-- |
-- The isomorpism between a representable functor and its representation.
--
-- @
-- 'tabulated' ≡ 'iso' 'tabulate' 'index'
-- 'tabulated' ≡ 'from' 'retabulated'
-- @
--
tabulated :: Representable f => Iso (Rep f -> a) (Rep f -> b) (f a) (f b)
tabulated = iso tabulate index

-- |
-- The reverse isomorpism between a representable functor and its representation.
--
-- @
-- 'retabulated' ≡ 'iso' 'index' 'tabulate'
-- 'retabulated' ≡ 'from' 'tabulated'
-- @
--
retabulated :: Representable f => Iso (f a) (f b) (Rep f -> a) (Rep f -> b)
retabulated = iso index tabulate



















-- |
-- Class of values that can be transformed (i.e. scaled and moved) in time.
--
-- Law
--
-- > transform mempty ≡ id
-- > transform (s <> t) ≡ transform s . transform t
--
-- Law
--
-- > onset (delay n a)       ≡ n ^+. onset a
-- > offset (delay n a)      ≡ n ^+. offset a
-- > duration (stretch n a)  ≡ n * duration a
-- > duration (compress n a) ≡ duration a / n
--
-- > delay n b ! t    ≡ b ! (t .-^ n)
-- > undelay n b ! t  ≡ b ! (t .+^ n)
--
-- Lemma
--
-- > duration a ≡ duration (delay n a)
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
-- Provided for situations when you have a value that should forward based on the distance
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

-- Fitting things

-- Things with a duration

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
clippedDuration = stretchTo 1

-- stretchClipped :: (Transformable a, HasDuration a, InnerSpace Duration) => a -> a
-- stretchClipped x = stretchTo (clippedd $ duration x) x


-- Placing things

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
-- '_position' x ≡ 'const' t
-- @
--
-- For values with an onset and offset you can use 'alerp':
--
-- @
-- '_position' x ≡ 'alerp' ('_onset' x) ('_offset' x)
-- @
--
class HasPosition a where
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

_era :: HasPosition a => a -> Span
_era x = _onset x <-> _offset x


-- |
-- Position of the given value.
--
position :: (HasPosition a, Transformable a) => Duration -> Lens' a Time
position d = lens (`_position` d) (flip $ placeAt d)

-- |
-- Onset of the given value.
--
onset :: (HasPosition a, Transformable a) => Lens' a Time
onset = position 0

-- |
-- Onset of the given value.
--
offset :: (HasPosition a, Transformable a) => Lens' a Time
offset = position 1

-- |
-- Onset of the given value.
--
preOnset :: (HasPosition a, Transformable a) => Lens' a Time
preOnset = position (-0.5)

-- |
-- Onset of the given value.
--
postOnset :: (HasPosition a, Transformable a) => Lens' a Time
postOnset = position 0.5

-- |
-- Onset of the given value.
--
postOffset :: (HasPosition a, Transformable a) => Lens' a Time
postOffset = position 1.5


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
_placeAt s x = transform (s ^-^ view era x) x

-- |
-- A lens to the position
--
era :: (HasPosition a, Transformable a) => Lens' a Span
era = lens _era (flip _placeAt)

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

-- TODO overload these?
(|>) = after
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
-- For non-positioned values such as 'Stretched', split cuts a value into pieces a piece
-- of the given duration and the rest.
--
-- For positioned values succh as 'Note', split cuts a value relative to its onset.
--
--
-- Law
--
-- > let (a, b) ≡ split x in duration a + duration b ≡ duration x
--
class HasDuration a => Splittable a where
  split  :: Duration -> a -> (a, a)

-- XXX what about Behavior (infinite span)



takeM, dropM :: Splittable a => Duration -> a -> a

takeM t = fst . split t
dropM t = snd . split t



-- |
-- Class of values that can be reversed (retrograded).
--
-- For positioned values succh as 'Note', the value is reversed relative to its middle point, i.e.
-- the onset value becomes the offset value and vice versa.
--
-- For non-positioned values such as 'Stretched', the value is reversed in-place.
--
-- FIXME Second law is incompatible with revDefault
--
-- Law
--
-- @
-- 'rev' ('rev' a) ≡ a
-- @
--
-- @
-- 'rev' s ``transform`` a ≡ 'rev' (s ``transform`` a)
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
-- 'rev' ≡ 'over' 'range' 'swap'
-- @
--
class Reversible a where

  -- | Reverse (retrograde) the given value.
  rev :: a -> a

-- XXX counterintunittive Behavior instances (just Behavior should reverse around origin, while
-- Bounds (Behavior a) should reverse around the middle, like a note)



{-
      rev s `transform` a     ≡ rev (s `transform` a)
  ==> (rev s `transform`)     ≡ rev . (s `transform`)
  ==> transform (rev s)       ≡ rev . (transform s)
  ==> (transform . rev) s     ≡ (rev .) (transform s)
  ==> (transform . rev) s     ≡ fmap rev (transform s)
  ==> transform . rev         ≡ fmap rev . transform
-}

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

-- There is no instance for Reversible Time
-- as we can not satisfy the second Reversible law

instance Reversible Span where
  rev = revDefault

instance Reversible a => Reversible (a, b) where
  rev (s,a) = (rev s, a)

revDefault :: (HasPosition a, Transformable a) => a -> a
revDefault x = (stretch (-1) `whilst` undelaying (_position x 0.5 .-. 0)) x
-- revDefault x = (stretch (-1) `under` undelaying (0)) x

newtype NoReverse a = NoReverse { getNoReverse :: a }

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









-- |
-- Internal time representation. Can be anything with instances
-- for 'Fractional' and 'RealFrac'.
--
type TimeBase = Rational


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
-- 'Duration' is invariant under translation so 'delayTime has no effect on it.
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
  Span (_, d1) `transform` d2 = d1 * d2

instance HasDuration Duration where
  _duration = id

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
-- A 'Span' represents two points in time @u@ and @v@ or, equnitvalently, a time @t@ and a
-- duration @d@. A third way of looking at 'Span' is that it represents a time
-- transformation where onset is translation and duration is scaling.
--
-- You can pattern match over spans using the @ViewPatterns@ extension:
--
-- @
-- foo (view range -> (u,v)) = ...
-- @
--
newtype Span = Span { getDelta :: (Time, Duration) }
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
  -- show (view range -> (t,u)) = show t ++ "<->" ++ show u
  show (view delta -> (t,d)) = show t ++ ">->" ++ show d

instance HasPosition Span where
  -- _position (view range -> (t1, t2)) = alerp t1 t2
  _onset  (view range -> (t1, t2)) = t1
  _offset (view range -> (t1, t2)) = t2

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

durationToSpan d  = 0 >-> d
timeToSpan t      = t >-> 1

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
range = iso getRange $ uncurry (<->)
  where
    getRange x = let (t, d) = getDelta x in (t, t .+^ d)

-- |
-- View a span as a pair of onset and duration.
--
delta :: Iso' Span (Time, Duration)
delta = iso getDelta $ uncurry (>->)

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

-- |
-- Apply a morphism under transformation (co-monadic version).
--
whilstW :: (Functor f, Transformable a, Transformable b) => (f a -> b) -> Span -> f a -> b
f `whilstW` t = transform (negateV t) . f . fmap (transform t)

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

-- whilstL :: (Transformable a, Transformable b) => Traversal s t a b -> Traversal (Span,s) (Span,t) a b

whilstL  l f (s,a) = (s,) <$> (l $ f `whilstM` s) a
whilstLT l f (t,a) = (t,) <$> (l $ f `whilstM` (t >-> 1)) a
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

-- |
-- Pitch type.
--
pitches' :: (HasPitches s t, s ~ t) => Traversal' s (Pitch s)
pitches' = pitches

#define PRIM_PITCH_INSTANCE(TYPE) \
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


PRIM_PITCH_INSTANCE(Bool)
PRIM_PITCH_INSTANCE(Int)
PRIM_PITCH_INSTANCE(Integer)
PRIM_PITCH_INSTANCE(Float)
PRIM_PITCH_INSTANCE(Double)
PRIM_PITCH_INSTANCE(Char)
PRIM_PITCH_INSTANCE(())

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
augmentIntervals :: Transposable a => Interval a -> Voice a -> Voice a
augmentIntervals = error "No augmentIntervals"
-- TODO generalize to any type where we can traverse phrases of something that has pitch

-- |
-- Transpose up by the given number of octaves.
--
octavesUp :: Transposable a => Scalar (Interval a) -> a -> a
octavesUp x = up (_P8^*x)

-- |
-- Transpose down by the given number of octaves.
--
octavesDown :: Transposable a => Scalar (Interval a) -> a -> a
octavesDown x = down (_P8^*x)

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


type instance Dynamic Double = Double
type instance SetDynamic a Double = a

instance HasDynamic Double Double where
  dynamic = ($)
instance HasDynamics Double Double where
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
     IsDynamics (Dynamic a){-, IsLevel (Level a)-})

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

-- |
-- Fade in.
--
fadeIn :: (Fractional c, HasDynamics s s, Dynamic s ~ Behavior c) => Duration -> s -> s
fadeIn t = dynamics *~ (t `stretch` unit)

-- |
-- Fade in.
--
fadeOut :: (Fractional c, HasDynamics s s, Dynamic s ~ Behavior c) => Duration -> s -> s
fadeOut t = dynamics *~ (t `stretch` rev unit)









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
  articulation = _Wrapped . whilstL articulation

instance (HasArticulations a b) => HasArticulations (Note a) (Note b) where
  articulations = _Wrapped . whilstL articulations


accent = error "No accent"
marcato = error "No marcato"
accentLast = error "No accentLast"
marcatoLast = error "No marcatoLast"
accentAll = error "No accentAll"
marcatoAll = error "No marcatoAll"
tenuto = error "No tenuto"
staccato = error "No staccato"
legato = error "No legato"









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
  -- \afb s -> (\x -> liftA2 (\b ->  runIdentity .
  --        lens2 (\_ -> Identity b)) x s) <$> afb ((\s -> getConst (lens1 Const s)) <$> s)

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
    getBP = fmap (view lens1)
    setBP = liftA2 (over lens2 . const)



-- |
-- 'Delayed' represents a value with an offset in time.
--
-- A delayed value has a known 'position', but no 'duration'.
--
-- Placing a value inside 'Delayed' does not make it invariant under 'stretch', as the
-- offset of a delayed value may be stretched with respect to the origin. However, in
-- contrast to a note the /duration/ is not stretched.
--
newtype Delayed a = Delayed   { getDelayed :: (Time, a) }
  deriving (Eq, {-Ord, -}{-Show, -}
            Applicative, Monad, {-Comonad, -}
            Functor,  Foldable, Traversable)

-- >>> stretch 2 $ (3,1)^.delayed
-- (6,1)^.stretched
--
-- >>> delay 2 $ (3,1)^.delayed
-- (3,1)^.stretched
--

deriving instance Typeable1 Delayed
deriving instance Show a => Show (Delayed a)

instance Wrapped (Delayed a) where
  type Unwrapped (Delayed a) = (Time, a)
  _Wrapped' = iso getDelayed Delayed

instance Rewrapped (Delayed a) (Delayed b)

instance Transformable (Delayed a) where
  transform t = over _Wrapped $ first (transform t)

instance HasPosition (Delayed a) where
  x `_position` p = ask (view _Wrapped x) `_position` p

instance Reversible (Delayed a) where
  rev = revDefault

-- |
-- View a delayed value as a pair of the original value and the transformation (and vice versa).
--
delayedValue :: (Transformable a, Transformable b) => Lens (Delayed a) (Delayed b) a b
delayedValue = lens runDelayed (flip $ mapDelayed . const)
  where
      mapDelayed f (Delayed (t,x)) = Delayed (t, (f `whilstDelay` t) x)








-- |
-- A 'Stretched' value has a known 'duration', but no 'position'.
--
-- Placing a value inside 'Stretched' makes it invariante under 'delay'.
--
newtype Stretched a = Stretched { getStretched :: (Duration, a) }
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

instance Wrapped (Stretched a) where
  type Unwrapped (Stretched a) = (Duration, a)
  _Wrapped' = iso getStretched Stretched

instance Rewrapped (Stretched a) (Stretched b)

instance Transformable (Stretched a) where
  transform t = over _Wrapped $ first (transform t)

instance HasDuration (Stretched a) where
  _duration = _duration . ask . view _Wrapped

instance Reversible (Stretched a) where
  rev = stretch (-1)

instance Splittable a => Splittable (Stretched a) where

deriving instance Show a => Show (Stretched a)

-- |
-- View a stretched value as a pair of the original value and the transformation (and vice versa).
--
stretchedValue :: (Transformable a, Transformable b) => Lens (Stretched a) (Stretched b) a b
stretchedValue = lens runStretched (flip $ mapStretched . const)
  where
    mapStretched f (Stretched (d,x)) = Stretched (d, (f `whilstStretch` d) x)



-- |
-- A 'Note' is a value with a known 'era'.
--
-- You can use 'noteValue' to apply a function in the context of the transformation,
-- i.e.
--
-- @
-- over noteValue (* time) (delay 2 $ return time)
-- @
--
-- @
-- ('view' 'noteValue') . 'transform' s ≡ 'transform' s . ('view' 'noteValue')
-- @
--
newtype Note a = Note { getNote :: (Span, a) }

deriving instance Eq a => Eq (Note a)
deriving instance Functor Note
deriving instance Typeable1 Note
deriving instance Foldable Note
deriving instance Traversable Note
deriving instance Applicative Note

instance (Show a, Transformable a) => Show (Note a) where
  show x = show (x^.from note) ++ "^.note"

-- |
-- Note is a 'Monad' and 'Applicative' in the style of pair, with 'return' placing a value
-- at the default span 'mempty' and 'join' composing time transformations.
deriving instance Monad Note

instance Wrapped (Note a) where
  type Unwrapped (Note a) = (Span, a)
  _Wrapped' = iso getNote Note

instance Rewrapped (Note a) (Note b)

instance Transformable (Note a) where
  transform t = over _Wrapped $ first (transform t)

instance HasDuration (Note a) where
  _duration = _duration . ask . view _Wrapped

instance HasPosition (Note a) where
  x `_position` p = ask (view _Wrapped x) `_position` p

instance Splittable a => Splittable (Note a) where

instance Reversible (Note a) where
  rev = revDefault

-- |
-- View a note as a pair of the original value and the transformation (and vice versa).
--
note :: Iso (Span, a) (Span, b) (Note a) (Note b)
note = _Unwrapped

-- |
-- Extract the transformed value.
--
runNote :: Transformable a => Note a -> a
runNote = uncurry transform . view _Wrapped

-- |
-- View the value in the note.
--
noteValue :: (Transformable a, Transformable b) => Lens (Note a) (Note b) a b
noteValue = lens runNote (flip $ mapNote . const)
  where
    mapNote f (Note (s,x)) = Note (s, f `whilst` negateV s $ x)

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
-- 'Bounds' restricts the start and stop time of a value.
--
newtype Bounds a = Bounds { getBounds :: (Span, a) }
  deriving (Functor, Foldable, Traversable)

-- |
-- Add bounds.
--
bounded :: Lens' (Bounds (Behavior a)) (Note (Segment a))
bounded = error "No bounded"


instance Wrapped (Bounds a) where
  type Unwrapped (Bounds a) = (Span, a)
  _Wrapped' = iso getBounds Bounds

instance Rewrapped (Bounds a) (Bounds b)

instance Reversible a => Reversible (Bounds a) where
  -- TODO
instance (HasPosition a, Splittable a) => Splittable (Bounds a) where
  -- TODO

-- |
-- 'Bounds' transform by transforming the bounded value as well as
-- the bounds.
--
instance Transformable a => Transformable (Bounds a) where
  transform t = over _Wrapped (transform t *** transform t)

instance (HasPosition a, HasDuration a) => HasDuration (Bounds a) where
  _duration x = _offset x .-. _onset x

instance HasPosition a => HasPosition (Bounds a) where
  _position (Bounds (view range -> (t, u), x)) d = truncating t u (_position x d)

truncating :: Ord a => a -> a -> a -> a
truncating t u x = (x `max` t) `min` u


-- |
-- Add bounds.
--
bounds :: Time -> Time -> a -> Bounds a
bounds t u x = Bounds (t <-> u, x)

-- |
-- Add bounds.
--
-- @
-- (s,x)^.note = (bounding s . transform s) x
-- @
--
bounding :: Span -> a -> Bounds a
bounding (view range -> (t, u)) = bounds t u

-- |
-- Add bounds.
--
trim :: Monoid b => Bounds (Behavior b) -> Behavior b
trim = trimG

-- More generic definition
trimG :: (Monoid b, Representable f, Rep f ~ Time) => Bounds (f b) -> f b
trimG (Bounds (s, x)) = tabulate (\t x -> if t `inside` s then x else mempty) `apRep` x


-- TODO Compare Diagram's Trail and Located (and see the conal blog post)

-- |
--
-- A 'Segment' is a value varying over some unknown time span, semantically
--
-- @
-- type 'Segment' a = 'Duration' -> a
-- @
--
-- To place a segment in a particular time span, use 'Note' 'Segment'.
--
newtype Segment a = Segment { getSegment :: Clipped Duration -> a }
  deriving (Functor, Applicative, Monad{-, Comonad-})
-- Defined 0-1

instance Show (Segment a) where
  show _ = "<<Segment>>"

deriving instance Typeable1 Segment
deriving instance Distributive Segment
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

-- instance Real a => Real (Segment a) where
  -- toRational = toRational . (`index` 0)

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

-- |
-- Index a segment.
--
(!.) :: Segment a -> Duration -> a
(!.) = (!)

-- |
-- View a behavior as a time function and vice versa.
--
--
segment :: Iso (Duration -> a) (Duration -> b) (Segment a) (Segment b)
segment = tabulated

-- |
-- A behavior that
--
_unit' :: Segment Duration
_unit' = id^.segment

-- |
-- A behavior that gives the current time, i.e. the identity function
_unit :: Fractional a => Segment a
_unit = realToFrac^.segment



-- | XXX name
fromSegment :: Monoid a => Iso (Segment a) (Segment b) (Behavior a) (Behavior b)
fromSegment = error "No fromSegment"

fromSegment2 :: Monoid a => Iso (Segment a) (Segment b) (Bounds (Behavior a)) (Bounds (Behavior b))
fromSegment2 = error "No fromSegment2"

appendSegment :: Stretched (Segment a) -> Stretched (Segment a) -> Stretched (Segment a)
appendSegment (Stretched (d1,s1)) (Stretched (d2,s2)) = Stretched (d1+d2, slerp (d1/(d1+d2)) s1 s2)

concatSegment :: Voice (Segment a) -> Stretched (Segment a)
concatSegment = foldr1 appendSegment . toListOf voiceElements

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
notTime = stretch 10 (stretch 2 (4**time-1) `min` 1)*10
notTime2 = (rev `whilst` undelaying 4.5) notTime

-- |
-- A 'Behavior' is a time-varying value.
--
-- @
-- type 'Behavior' a => 'Time' -> a
-- @
--
-- While a 'Behavior' can not be placed (as it has no endpoints), we can focus on a
-- certain part of a behavior by placing it inside 'Bounds'.
--
-- TODO document
--
newtype Behavior a  = Behavior { getBehavior :: Time -> a }   deriving (Functor, Applicative, Monad{-, Comonad-})
-- Defined throughout, "focused" on 0-1

instance Show (Behavior a) where
  show _ = "<<Behavior>>"

deriving instance Typeable1 Behavior
deriving instance Distributive Behavior
deriving instance Semigroup a => Semigroup (Behavior a)
deriving instance Monoid a => Monoid (Behavior a)
deriving instance Num a => Num (Behavior a)
deriving instance Fractional a => Fractional (Behavior a)
deriving instance Floating a => Floating (Behavior a)

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

-- instance Real a => Real (Behavior a) where
  -- toRational = toRational . (`index` 0)

deriving instance AdditiveGroup a => AdditiveGroup (Behavior a)

instance VectorSpace a => VectorSpace (Behavior a) where
  type Scalar (Behavior a) = Behavior (Scalar a)
  (*^) = liftA2 (*^)

instance AffineSpace a => AffineSpace (Behavior a) where
  type Diff (Behavior a) = Behavior (Diff a)
  (.-.) = liftA2 (.-.)
  (.+^) = liftA2 (.+^)

-- TODO is this correct?
instance Transformable (Behavior a) where
  transform s (Behavior a) = Behavior (a `whilst` s)
    where
      f `whilst` s = f . transform (negateV s)


-- TODO correct?
instance Reversible (Behavior a) where
  rev = stretch (-1)
  -- OR
  -- rev = (stretch (-1) `whilst` undelaying 0.5)
  -- (i.e. revDefault pretending that Behaviors have era (0 <-> 1))


instance Representable Behavior where
  type Rep Behavior = Time
  tabulate = Behavior
  index (Behavior x) = x


-- type instance Pitch                 (Behavior a) = Behavior (Pitch a)
-- type instance SetPitch (Behavior g) (Behavior a) = Behavior (SetPitch g a)
--
-- instance (HasPitch a a, HasPitch a b) => HasPitches (Behavior a) (Behavior b) where
--   pitches = through pitch pitch
-- instance (HasPitch a a, HasPitch a b) => HasPitch (Behavior a) (Behavior b) where
--   pitch = through pitch pitch

type instance Pitch      (Behavior a) = Behavior a
type instance SetPitch b (Behavior a) = b
instance (Transformable a, Transformable b, b ~ Pitch b) => HasPitches (Behavior a) b where
  pitches = ($)
instance (Transformable a, Transformable b, b ~ Pitch b) => HasPitch (Behavior a) b where
  pitch = ($)



-- TODO tests
returnB = return :: (a -> Behavior a)
extractB = (!^ 0)
x = delay 2 $ return (return 3) :: Note ((), (Float, Float))
y = over pitches returnB x
z = over pitches extractB y -- TODO

aa = (\f -> {-over pitches extractB .-} over pitches f . over pitches returnB) (*(time*2)) x


-- > :t over pitch returnB x
-- > :t over pitch extractB $ over pitch (returnB) $ x


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

-- |
-- Returns the value of a behavior at a given time
--
-- Note that this is just an alias defined to make the documentation nicer:
--
-- @
-- '!^' ≡ '!'
-- @
--
(!^) :: Behavior a -> Time -> a
(!^) = (!)

-- |
-- View a behavior as a time function and vice versa.
--
-- Note that this is just an alias defined to make the documentation nicer:
--
--
-- @
-- 'behavior' ≡ 'tabulated'
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


-- TODO find a place for this
--
-- @
-- ('const' x)^.'behavior' ! t == x   forall t
-- @
--
--


-- |
-- A behavior that
--
time' :: Behavior Time
time' = id^.behavior

-- |
-- A behavior that gives the current time, i.e. the identity function
time :: Fractional a => Behavior a
time = realToFrac^.behavior
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
-- A behavior that goes from 0 to 1 repeatedly with a period of 1.
--
sawtooth :: RealFrac a => Behavior a
sawtooth = time - fmap floor' time

-- |
-- A behavior that is 1 at time 0, and 0 at all other times.
--
impulse :: Num a => Behavior a
impulse = switch3 0 0 1 0
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

floor' :: RealFrac a => a -> a
floor' = fromIntegral . floor

{-
-- | Specification of 'index'.
atTime :: Behavior a -> Time -> a
atTime = index
-}

focus :: Behavior a -> Segment a
focus = focusOn mempty

focusOn :: Span -> Behavior a -> Segment a
focusOn s = view (focusedOn s)

focused :: Lens' (Behavior a) (Segment a)
focused = focusedOn mempty

focusedOn :: Span -> Lens' (Behavior a) (Segment a)
focusedOn = error "No focusedOn"

-- |
-- Instantly switch from one behavior to another.
--
-- @'switch' t x y@ behaves as @x@ until time @t@, at which point it starts
-- behaving as @y@.
--
switch :: Time -> Behavior a -> Behavior a -> Behavior a
switch t rx ry = switch3 t rx ry ry

-- |
-- Instantly switch from one behavior to another with an optinal intermediate value.
--
-- @'switch' t x y z@ behaves as @x@ until time @t@ and as @z@ after time @t@.
-- At time @t@ it has value @y '!^' t@.
--
switch3 :: Time -> Behavior a -> Behavior a -> Behavior a -> Behavior a
switch3 t rx ry rz = tabulate $ \u -> case u `compare` t of
  LT -> rx ! u
  EQ -> ry ! u
  GT -> rz ! u

-- |
-- Splice (named for the analogous tape-editing technique) proivides an alternative behavior
-- for a limited amount of time.
--
-- @'splice' b ('bounds' t u b')@ behaves as @b'@ inside the bounds, and as @b@
-- outside.
--
splice :: Behavior a -> Bounds (Behavior a) -> Behavior a
splice constant insert = fmap fromLast $ fmap toLast constant <> trim (fmap (fmap toLast) insert)
  where
    toLast   = Option . Just . Last
    fromLast = getLast . fromMaybe undefined . getOption

-- |
-- This
--
concatBehavior :: Monoid a => Score (Behavior a) -> Behavior a
concatBehavior = error "No concatBehavior"

-- switch :: Time -> Reactive a -> Reactive a -> Reactive a
-- trim :: Monoid a => Span -> Reactive a -> Reactive a








type ScoreNote a = Note a

newtype Score a = Score { getScore :: [ScoreNote a] }
  deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

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

voices :: Traversal' (Score a) (Voices a)
phrases :: Traversal' (Score a) (Phrases a)
(voices, phrases) = error "No (voices, phrases)"

singleNote :: Prism' (Score a) (Note a)
singleNote = error "No singleNote"

-- | XXX indexed traversal?
notes :: Traversal' (Score a) (Note a)
notes = _Wrapped . traverse

-- | Map over the values in a score.
mapWithSpan :: (Span -> a -> b) -> Score a -> Score b
mapWithSpan f = error "No mapWithSpan"

-- | Filter the values in a score.
filterWithSpan :: (Span -> a -> Bool) -> Score a -> Score a
filterWithSpan f = error "No filterWithSpan"

-- | Combination of 'mapEvents' and 'filterEvents'.
mapFilterWithSpan :: (Span -> a -> Maybe b) -> Score a -> Score b
mapFilterWithSpan f = error "No mapFilterWithSpan"

-- | Map over the values in a score.
mapEvents :: (Time -> Duration -> a -> b) -> Score a -> Score b
mapEvents f = mapWithSpan (uncurry f . view delta)

-- | Filter the values in a score.
filterEvents   :: (Time -> Duration -> a -> Bool) -> Score a -> Score a
filterEvents f = error "No filterEvents"

-- | Efficient combination of 'mapEvents' and 'filterEvents'.
mapFilterEvents :: (Time -> Duration -> a -> Maybe b) -> Score a -> Score b
mapFilterEvents f = error "No mapFilterEvents"


-- |
-- A 'Voice' is a sequence of stretched values.
--
newtype Voice a = Voice { getVoice :: Seq (Stretched a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

singleStretched :: Prism' (Voice a) (Stretched a)
singleStretched = error "No singleStretched"

instance Applicative Voice where
  pure  = return
  (<*>) = ap

instance Monad Voice where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

instance Wrapped (Voice a) where
  type Unwrapped (Voice a) = (Seq (Stretched a))
  _Wrapped' = iso getVoice Voice

instance Rewrapped (Voice a) (Voice b)

instance Transformable (Voice a) where
  transform s = over _Wrapped' (transform s)

instance HasDuration (Voice a) where
  _duration = Foldable.sum . fmap _duration . view _Wrapped'

instance Splittable a => Splittable (Voice a) where
  -- TODO

instance Reversible a => Reversible (Voice a) where
  rev = over _Wrapped' rev

-- |
-- Voice
--
voiceNotes :: Traversal (Voice a) (Voice b) (Note a) (Note b)
voiceNotes = error "No voiceNotes"

-- |
-- Voice
--
voiceElements :: Traversal (Voice a) (Voice b) (Stretched a) (Stretched b)
voiceElements = _Wrapped . traverse

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice :: Voice a -> Voice b -> Voice (a, b)
zipVoice = zipVoiceWith (,)

-- |
-- Join the given voices by multiplying durations and combining values using the given function.
--
zipVoiceWith :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith  = error "No zipVoiceWith"

-- |
-- Join the given voices by combining durations and values using the given function.
--
dzipVoiceWith :: (Duration -> Duration -> a -> b -> (Duration, c)) -> Voice a -> Voice b -> Voice c
dzipVoiceWith = error "No dzipVoiceWith"


voiceList :: Iso' (Voice a) [(Duration, a)]
voiceList = error "No voiceList"

-- |
-- Merge consecutive equal note.
--
mergeEqualNotes :: Eq a => Voice a -> Voice a
mergeEqualNotes = over voiceList $ fmap f . Data.List.groupBy (inspecting snd)
  where
    f dsAs = let (ds,as) = unzip dsAs in (sum ds, head as)


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
voices' = error "No voices'"

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

-- | XXX
phrases' :: Traversal'
  (Phrases a)
  (Either (Voice a) (Voices a))
phrases' = error "No phrases'"


concatVoices :: Monoid a => Phrases a -> Voice a
concatVoices = error "No concatVoices"

-- | XXX only defined positively
-- Need to use alternative to voice similar to a zipper etc
data Reactive a = Reactive a (Voice a) a

-- | Get the initial value.
initial :: Reactive a -> a

-- | Get the final value.
final :: Reactive a -> a

updates :: Reactive a -> Voice a
(initial, final, updates) = error "No (initial, final, updates)"


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









-- -- Has... Pitch Dynamics Articulation Part Chord?? Clef Slide Tremolo Text Harmonic Meta
-- -- Has+Is ... Midi/MusicXml
-- -- Is ... Pitch Interval Dynamic














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
instance HasPitches PD PD where
  pitches = _Wrapped . _2
instance HasPitch PD PD where
  pitch = _Wrapped . _2
instance HasDynamics PD PD where
  dynamics = _Wrapped . _1
instance HasDynamic PD PD where
  dynamic = _Wrapped . _1
pd :: PD
pd = PD (time, time)

drawPD pd = lc red (drawBehavior $ pd^.dynamic) <> lc blue (drawBehavior $ pd^.pitch)



a :: Behavior Float
a = time



-- c2 :: Behavior Float -> Behavior Float
-- c2  = liftA2 (*) c1

-- nc :: Note (Behavior (Int, Float))
-- nc = transform (3 >-> 5) $ return $ fmap (0,) $ fmap toFloat adsr

-- r :: Behavior Float
-- r  = fmap snd $ runNote (nc & pitch %~ c2)

-- drawNote :: (Real a, Renderable (Path R2) b) => Note a -> Diagram b R2
-- drawNote n = let
  -- (t,d) = view delta $ n^.era
  -- a = n ^?! traverse
  -- in drawNote' (t,d,a)
























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

sameType :: a -> a -> ()
sameType = undefined


-- #define INCLUDE_TESTS

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

instance (Monad m, CoSerial m a) => CoSerial m (Clipped a) where
  coseries = undefined
  -- coseries = fmap unsafeToClipped

instance Monad m => Serial m Time where
  series = msum $ fmap return [-1,0,2.13222,10,20]

instance Monad m => Serial m Duration where
  series = msum $ fmap return [-1,0,1.51232,10,20]

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


-- > onset (delay n a)      = n ^+. onset a
-- > offset (delay n a)     = n ^+. offset a
-- > duration (stretch n a) = n ^* (duration a)
--
-- Lemma
--
-- > duration a = duration (delay n a)

delayDurationLaw typ = testGroup ("Delay and duration " ++ show (typeOf typ)) $ [
  testProperty "_duration a == _duration (delay n a)" $ \(n :: Duration) a -> assuming (sameType typ a) $
                _duration a == _duration (delay n a)
  ]

stretchDurationLaw typ = testGroup ("Delay and duration " ++ show (typeOf typ)) $ [
  testProperty "_duration (stretch n a) == n ^* (_duration a)" $ \(n :: Duration) a -> assuming (sameType typ a) $
                _duration (stretch n a) == n ^* (_duration a)
  ]

delayBehLaw typ = testGroup ("Delay behavior " ++ show (typeOf typ)) $ [
  testProperty "delay n b ! t == b ! (t .-^ n)" $ \(n :: Duration) (t :: Time) b -> assuming (sameType typ b) $
                delay n b ! t == b ! (t .-^ n)
  ]


--
-- > (t<->u) `transform` b ! t           == b ! 0
-- > (t<->u) `transform` b ! ((u-t)/2+t) == b ! 0.5
-- > (t<->u) `transform` b ! u           == b ! 1
--

transformUi typ = testGroup ("Transform UI " ++ show (typeOf typ)) $ [
  testProperty "(t<->u) `transform` b ! t          == b ! 0" $
    \(t :: Time) (u2 :: Time) -> let b = (unit::Behavior Double); u = decollide t u2 in
                (t<->u) `transform` b ! t          == b ! 0,

  testProperty "(t<->u) `transform` b ! ((u-t)/2+t) == b ! 0.5" $
    \(t :: Time) (u2 :: Time) -> let b = (unit::Behavior Double); u = decollide t u2 in
                (t<->u) `transform` b ! ((u-t)/2+t) == b ! 0.5,

  testProperty "(t<->u) `transform` b ! u           == b ! 1" $
    \(t :: Time) (u2 :: Time) -> let b = (unit::Behavior Double); u = decollide t u2 in
                (t<->u) `transform` b ! u           == b ! 1

  ]

decollide :: (Eq a, Num a) => a -> a -> a
decollide x y
  | x == y    = y + 1
  | otherwise = y

monoid = monoidEq (==)

monoidEq (===) typ = testGroup ("instance Monoid " ++ show (typeOf typ)) $ [
  testProperty "x <> (y <> z) == (x <> y) <> z" $ \x y z -> assuming (sameType typ x)
          (x <> (y <> z)) === ((x <> y) <> z),

  testProperty "mempty <> x == x"         $ \x   -> assuming (sameType typ x)
          (mempty <> x) === x,

  testProperty "x <> mempty == x"         $ \x   -> assuming (sameType typ x)
         ((x <> mempty) === x)
  ]
  where
    (<>) = mappend

functor typ = testGroup ("instance Functor " ++ show (typeOf typ)) $ [
  testProperty "fmap id = id" $ \x -> assuming (sameType typ x)
         (fmap id x == id x)
  ]

reversible = reversibleEq (==)

reversibleEq (===) typ = testGroup ("instance Reversible " ++ show (typeOf typ)) $ [
  testProperty "rev . rev == id" $ \x -> assuming (sameType typ x)
                (rev (rev x)) === x,

  testGroup "" [],

  testProperty "transform . rev == fmap rev . transform" $ \(s :: Span) x -> assuming (sameType typ x)
                ((transform . rev) s x) === ((fmap rev . transform) s x)
  ]



main = defaultMain $ testGroup "All tests" $ [
  testGroup "Monoid" [
    monoid (undefined :: ()),
    monoid (undefined :: Duration),
    monoid (undefined :: Time),
    monoid (undefined :: Span),
    monoidEq (\x y -> x ! 0 == y ! 0 && x ! 1 == y ! 1) (undefined :: Segment Time),
    monoidEq (\x y -> x ! 0 == y ! 0 && x ! 1 == y ! 1) (undefined :: Behavior Time),
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
  testGroup "Reversible" [
    reversible (undefined :: ()),
    reversible (undefined :: Double),
    reversible (undefined :: Duration),
    reversible (undefined :: Span),
    reversibleEq (\x y -> x ! 0 == y ! 0 && x ! 1 == y ! 1) (undefined :: Behavior Duration),
    reversibleEq (\x y -> x ! 0 == y ! 0 && x ! 1 == y ! 1) (undefined :: Segment Time),

    reversible (undefined :: Note Duration),
    reversible (undefined :: Stretched Duration),
    reversible (undefined :: Delayed Duration),

    reversible (undefined :: Voice Duration),
    reversible (undefined :: Score Duration)
  ],

  testProperty "============================================================" $ True,  
  testGroup "Delay and stretch" [
    stretchDurationLaw (undefined :: Stretched Time),
    delayDurationLaw   (undefined :: Stretched Time),
    stretchDurationLaw (undefined :: Note Time),
    delayDurationLaw   (undefined :: Note Time),
    stretchDurationLaw (undefined :: Voice Time),
    delayDurationLaw   (undefined :: Voice Time),
    stretchDurationLaw (undefined :: Score Time),
    delayDurationLaw   (undefined :: Score Time)
  ],

  -- delayBehLaw (undefined :: Behavior Int8),
  -- transformUi (undefined :: Behavior Int8)
  -- functor (undefined :: BadFunctor Int8),
  -- functor (undefined :: BadMonoid Int8)
                             
  testProperty "============================================================" $ True,  
    testGroup "Nothing" []
  ]
#endif // INCLUDE_TESTS
















drawScore' :: (Renderable (Path R2) b, Real a) =>     [[(Time, Duration, a)]] -> Diagram b R2
drawScore' = vcat' (def & sep .~ 2) . fmap drawPart'

drawPart' :: (Renderable (Path R2) b, Real a) =>    [(Time, Duration, a)] -> Diagram b R2
drawPart' = mconcat . fmap drawNote'

drawNote' :: (Renderable (Path R2) b, Real a) => (Time, Duration, a) -> Diagram b R2
drawNote' (realToFrac -> t, realToFrac -> d, realToFrac -> y) = translateY y $ translateX t $ scaleX d noteShape
  where
  noteShape = {-showOr $-} lcA transparent $ fcA (blue `withOpacity` 0.5)
    $ strokeLoop $ closeLine $ fromOffsets $ fmap r2 $ [(1.2,0), (-0.2,0.2),(-0.8,0.2), (-0.2,0.6),(-0.2,-1)]

drawBehavior :: (Renderable (Path R2) b, Real a) =>  Behavior a -> Diagram b R2
drawBehavior = drawBehavior' 0 10

drawSegment :: (Renderable (Path R2) b, Real a) =>  Segment a -> Diagram b R2
drawSegment = scaleX 10 . drawBehavior' 0 1

drawBehavior' start count b = draw points & lw 0.02
  where
    points = take (samplesPerCell*count) $ fmap (\x -> p2 (x, fromVal (b ! toTime x))) [start,start+1/samplesPerCell..]
    toTime = realToFrac
    fromVal = realToFrac
    samplesPerCell = 90
    -- draw = cubicSpline False
    -- TODO offset without showing
    draw = fromOffsets . (\xs -> zipWith (.-.) (tail xs) xs) . ((p2 (0,0)) :)

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



