
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}

module TimeTypes (

        -- * Music.Time.Transform
        Transformable(..),
        delaying,       -- :: Duration -> Span
        undelaying,
        stretching,     -- :: Duration -> Span
        compressing,
        delay,          -- :: Transformable a => Duration -> a -> a
        -- delay',
        undelay,
        stretch,        -- :: Transformable a => Duration -> a -> a
        compress,

        -- * Music.Time.Duration
        HasDuration(..),
        stretchTo,      -- :: (Transformable a, HasDuration a) => Duration -> a -> a
        stretchNorm,

        -- * Music.Time.Position
        HasPosition(..),
        era,
        -- preOnset,       -- :: HasPosition a => a -> Time
        onset,          -- :: HasPosition a => a -> Time
        -- postOnset,      -- :: HasPosition a => a -> Time
        offset,         -- :: HasPosition a => a -> Time
        -- postOffset,     -- :: HasPosition a => a -> Time
        startAt,        -- :: (Transformable a, HasPosition a) => Time -> a -> a
        stopAt,         -- :: (Transformable a, HasPosition a) => Time -> a -> a
        alignAt,        -- :: (Transformable a, HasPosition a) => Duration -> Time -> a -> a
        -- pinned,        -- :: (HasPosition a, HasPosition b, Transformable b) => (a -> b) -> a -> b

        -- * Music.Time.Reverse
        Reversible(..),

        -- * Music.Time.Split
        Splittable(..),

        -- * Music.Time.Sequential
        Sequential(..),

        -- * Music.Time.Parallel
        Parallel(..),

        -- * Music.Time.Combinators
        lead,           -- :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
        follow,         -- :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
        after,
        before,
        -- (|>),
        -- (>|),
        sustain,
        scat,
        pcat,
        times,

        -- * Music.Time.Types
        Duration,
        Time,
        Span,
        (<->),
        (>->),
        range,
        delta,
        under,      -- :: (Transformable a, Transformable b) => Span -> (a -> b) -> a -> b
        -- conjugate,  -- :: Span -> Span -> Span

        -- * Music.Time.Stretched
        Stretched,
        stretched,
        runStretched,

        -- * Music.Time.Delayed
        Delayed,
        delayed,
        runDelayed,

        -- * Music.Time.Note
        Note,
        note,
        mapNote,
        runNote,

        -- * Music.Time.Bounds
        Bounds,
        bounds,
        trim,

        -- * Music.Time.Segment
        Segment,

        -- * Music.Time.Behavior
        Behavior,

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
        SetPitch,
        HasPitch(..),
        HasPitches(..),

  ) where

import Diagrams.Prelude hiding (Transformable, value, trim, view, transform, Segment, duration, position, era, under, stretchTo, offset, after, Time, Duration, (|>), (<->), stretch)
import qualified Diagrams.Backend.SVG as SVG
import Data.Default
import System.Process (system)
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Data.ByteString.Lazy as ByteString


import           Control.Applicative
import           Control.Arrow          (first, second, (***))
import           Control.Comonad
import           Data.NumInstances
import           Control.Comonad.Env
import           Control.Lens           hiding (Indexable, transform, under, (|>))
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.Key
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe
import           Data.AffineSpace.Point
import           Data.Foldable          (Foldable)
import qualified Data.Foldable as Foldable
-- import           Data.Key (or use Control.Lens.Indexed?)
import           Data.Semigroup
import           Data.Traversable       (Traversable)
import qualified Data.Traversable       as T
import           Data.Typeable
import           Data.VectorSpace
import           Data.Sequence(Seq)
import qualified Data.Sequence as Seq

import           Data.Int
import           Test.SmallCheck.Series (Serial (..), cons0, newtypeCons,
                                         series, (\/))
import           Test.Tasty
import           Test.Tasty.SmallCheck

import qualified Data.Ratio             as Util_Ratio


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
        a <> (b <> c)    = (a <> b) <> c
        a      <> mempty = a
        mempty <> a      = a

    Law AdditiveGroup
        a ^+^ (b ^+^ c)    = (a ^+^ b) ^+^ c
        a      ^+^ zeroV   = a
        zeroV  ^+^ a       = a
        a      ^+^ negateV a = zeroV
        negateV a ^+^ a      = zeroV
        a ^+^ b              = b ^+^ a

    Law Functor
    Law Eq
    Law Ord
    Law
-}

-- | A value in the unit interval /(0,1)/.
newtype Normalized a = Normalized { getNormalized :: a }
    deriving (Eq, Ord, Show, Functor)

zipNormalizedWith :: (Num a, Ord a, Num b, Ord b, Num c, Ord c) => (a -> b -> c) -> Normalized a -> Normalized b -> Maybe (Normalized c)
zipNormalizedWith f a b = ((a^.unnormalize) `f` (b^.unnormalize))^? normalize

normalize' = Normalized
addLim = zipNormalizedWith (+)    
    
normalize :: (Num a, Ord a) => Prism' a (Normalized a)
normalize = prism getNormalized (\x -> if 0 <= x && x <= 1 then Right (Normalized x) else Left x)

unnormalize :: (Num a, Ord a) => Getter (Normalized a) a
unnormalize = re normalize

-- |
-- Duration, corresponding to note values in standard notation.
--
-- The standard names can be used: @1\/2@ for half note @1\/4@ for a quarter note and so on.
--
-- Duration is a one-dimensional 'VectorSpace', and is the associated vector space of time points.
-- It is a also an 'AdditiveGroup' (and hence also 'Monoid' and 'Semigroup') under addition.
--
newtype Duration = Duration { getDuration :: Rational }
instance Show Duration where
    show = showRatio . getDuration
deriving instance Typeable Duration
deriving instance Eq Duration
deriving instance Ord Duration
deriving instance Num Duration
deriving instance Enum Duration
deriving instance Fractional Duration
deriving instance Real Duration
deriving instance RealFrac Duration
deriving instance AdditiveGroup Duration
instance VectorSpace Duration where
    type Scalar Duration = Duration
    (*^) = (*)
instance Floating Duration
instance InnerSpace Duration
instance Semigroup Duration where
    (<>) = (*^)
instance Monoid Duration where
    mempty  = 1 -- TODO use some notion of norm
    mappend = (*^)
instance Transformable Duration where
    Span (_, d1) `transform` d2 = d1 * d2
instance HasDuration Duration where
    duration = id

-- |
-- Time points, representing duration since some known reference time, typically the start
-- of the music. Note that time can be negative, representing events occuring before the
-- start time.
--
-- Time forms an affine space with durations as the underlying vector space, that is, we
-- can add a time to a duration to get a new time using '.+^', take the difference of two
-- times to get a duration using '.-.'. 'Time' forms an 'AffineSpace' with 'Duration' as
-- difference space.
--
newtype Time = Time { getTime :: Rational }
instance Show Time where
    show = showRatio . getTime
deriving instance Typeable Time
deriving instance Eq Time
deriving instance Ord Time
deriving instance Num Time
deriving instance Enum Time
deriving instance Fractional Time
deriving instance Real Time
deriving instance RealFrac Time
deriving instance AdditiveGroup Time
instance VectorSpace Time where
    type Scalar Time = Duration
    Duration x *^ Time y = Time (x * y)
instance AffineSpace Time where
    type Diff Time = Duration
    Time x .-. Time y     = Duration (x - y)
    Time x .+^ Duration y = Time     (x + y)
instance Semigroup Time where
    (<>) = (^+^)
instance Monoid Time where
    mempty  = zeroV
    mappend = (^+^)
    mconcat = sumV
instance Transformable Time where
    Span (t1, d1) `transform` t2 = t1 ^+^ d1 *^ t2
instance HasPosition Time where
    position = const


-- | 
-- > 1 >-> 2 == (1,2)^.from delta
--
-- > 1 <-> 2 == (1,2)^.from range
--
newtype Span = Span { getDelta :: (Time, Duration) }
    deriving (Eq, Ord, Show, Typeable)

(<->) :: Time -> Time -> Span
(>->) :: Time -> Duration -> Span
t <-> u = t >-> (u .-. t)
t >-> d = Span (t, d)

-- |
--
range :: Iso' Span (Time, Time)
range = iso getRange $ uncurry (<->) where getRange x = let (t, d) = getDelta x in (t, t .+^ d)

-- |
--
delta :: Iso' Span (Time, Duration)
delta = iso getDelta $ uncurry (>->)

instance HasPosition Span where
    position (view range -> (t1, t2)) = alerp t1 t2
instance HasDuration Span where
    duration = snd . view delta
instance Transformable Span where
    transform = (<>)
instance Splittable Span where
    -- XXX
instance Semigroup Span where
    (<>) = (^+^)
instance Monoid Span where
    mempty  = zeroV
    mappend = (^+^)
instance AdditiveGroup Span where
    zeroV   = 0 <-> 1
    Span (t1, d1) ^+^ Span (t2, d2) = Span (t1 ^+^ d1 *^ t2, d1*d2)
    negateV (Span (t, d)) = Span (-t ^/ d, recip d)

-- > forall s . id `sunder` s = id
under :: (Transformable a, Transformable b) => Span -> (a -> b) -> a -> b
s `under` f = transform (negateV s) . f . transform s

conjugate :: Span -> Span -> Span
conjugate t1 t2  = negateV t1 <> t2 <> t1


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

type family Pitch             (s :: *) :: * -- Pitch s   = a
type family SetPitch (b :: *) (s :: *) :: * -- Pitch b s = t

-- class Has s t a b | 
--     s -> a, 
--     -- t -> b, 
--     s b -> t, 
--     -- t a -> s

-- type Lens      s t a b = forall f. Functor f     => (a -> f b) -> s -> f t
-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- |
-- Class of types that provide a single pitch.
--
class (SetPitch (Pitch t) s ~ t) => HasPitch s t where
    pitch :: Lens s t (Pitch s) (Pitch t)

pitch' :: (HasPitch s t, s ~ t) => Lens' s (Pitch s)
pitch' = pitch

-- |
-- Class of types that provide a pitch traversal.
--
class (SetPitch (Pitch t) s ~ t) => HasPitches s t where
    pitches :: Traversal s t (Pitch s) (Pitch t)

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
instance HasPitch Int Int where
    pitch = ($)
instance HasPitches Int Int where
    pitches = ($)

type instance Pitch Float = Float
type instance SetPitch a Float = a
instance HasPitch Float Float where
    pitch = ($)
instance HasPitches Float Float where
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
instance (HasPitch a b, Transformable (Pitch a), Transformable (Pitch b)) => HasPitch (Note a) (Note b) where         
    pitch = _Wrapped . pl
        where
            pl f (s,a) = (s,) <$> (pitch $ fmap (transform (negateV s)) . f . transform s) a
instance (HasPitches a b, Transformable (Pitch a), Transformable (Pitch b)) => HasPitches (Note a) (Note b) where         
    pitches = _Wrapped . pl
        where
            pl f (s,a) = (s,) <$> (pitches $ fmap (transform (negateV s)) . f . transform s) a


type instance Pitch (Score a) = Pitch a
type instance SetPitch g (Score a) = Score (SetPitch g a)
type instance Pitch (Score a) = Pitch a
instance (HasPitches a b, Transformable (Pitch a), Transformable (Pitch b)) => HasPitches (Score a) (Score b) where         
    pitches = _Wrapped . traverse . pl
        where
            pl :: (HasPitches a b, Transformable (Pitch a), Transformable (Pitch b)) => 
                Traversal (Span, a) (Span, b) (Pitch a) (Pitch b)
            pl f (s,a) = (s,) <$> (pitches $ fmap (transform (negateV s)) . f . transform s) a


-- TODO debug/move
type instance Pitch                 (Behavior a) = Behavior (Pitch a)
type instance SetPitch (Behavior g) (Behavior a) = Behavior (SetPitch g a)
type instance Pitch                 (Segment a) = Segment (Pitch a)
type instance SetPitch (Segment g) (Segment a) = Segment (SetPitch g a)

{-
    Is there a generic
        Applicative f => Lens a a pa pa -> Lens a b pb pb -> Lens (f a) (f b) (f pa) (f pb)
-}
instance (HasPitch a a, HasPitch a b) => HasPitch (Behavior a) (Behavior b) where
    pitch = lensing pitch pitch
instance (HasPitch a a, HasPitch a b) => HasPitch (Segment a) (Segment b) where
    pitch = lensing pitch pitch

lensBP :: (Applicative f, HasPitch a a, HasPitch a b) => Lens (f a) (f b) (f (Pitch a)) (f (Pitch b))
lensBP = lens getBP (flip setBP)
    where
        mapBP :: (HasPitch a a, HasPitch a b, Applicative f) => (f (Pitch a) -> f (Pitch b)) -> f a -> f b
        mapBP f a = liftA2 (pitch .~) (f $ (^. pitch) <$> a) a

        getBP :: (Functor f, HasPitch a a) => f a -> f (Pitch a)
        getBP a = (^. pitch) <$> a

        setBP :: (Applicative f, HasPitch b c) => f (Pitch c) -> f b -> f c
        setBP x a = liftA2 (pitch .~) x a

-- lensing :: Applicative f => Lens' s a -> Lens s t a b -> Lens (f s) (f t) (f a) (f b)
-- lensing lens1 lens2 = lens getBP (flip setBP)
--     where
--         getBP a = (^. lens1) <$> a
--         setBP x a = liftA2 (lens2 .~) x a

lensing :: Applicative f => Lens' s a -> Lens s t a b -> Lens (f s) (f t) (f a) (f b)
lensing lens1 lens2 = 
    -- lens getBP (flip setBP)
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
    --             <$> 
    --           afb ((\s -> getConst (lens1 Const s)) <$> s)

    -- \f s -> (\x -> (\b ->  runIdentity . lens2 (const $ Identity b)) <$> x <*> s) 
    --             <$> 
    --           f ((\s -> getConst (lens1 Const s)) <$> s)

    -- \f s -> (\x -> liftA2 (\a b -> runIdentity $ (lens2 . const . Identity $ b) a) s x)
    --             <$> 
    --           f ((getConst . lens1 Const) <$> s)

    -- \f s -> liftA2 ( \a b -> runIdentity (lens2 (const (Identity b)) a) ) s <$> (f ((getConst . lens1 Const) <$> s))
    -- \f s -> liftA2 ( \a -> runIdentity . flip lens2 a . const . Identity ) s <$> (f ((getConst . lens1 Const) <$> s))
    \f s -> liftA2 (\a -> runIdentity . (`lens2` a) . const . Identity) s <$> f (getConst <$> lens1 Const <$> s)



    where
        -- getBP a = (^. lens1) <$> a
        -- setBP x a = liftA2 (lens2 .~) x a

flx :: ((b1 -> a) -> b -> c) -> b -> a -> c
flx l x = 
    -- flip l x . const
    -- (\f a b -> f b a) l x . const
    -- (\b -> l b x) . const
    -- (\b -> l b x) . (\x y -> x)
    -- (\f g x -> f (g x)) (\b -> l b x) (\x y -> x)
    -- (\f g x2 -> f (g x2)) (\b -> l b x) (\x3 y -> x3)
    -- (\x2 -> (\b -> l b x)  ((\x3 y -> x3) x2))
    -- (\x2 -> (\b -> l b x)  (\y -> x2))
    -- (\x2 -> (l (\y -> x2) x))
    (\x2 -> l (\_ -> x2) x)
 
deriving instance Show a => Show (Note a)
instance Transformable Int where
    transform _ = id
instance Transformable (Behavior a) where
    transform (view delta -> (t,d)) (Behavior f) = Behavior $ (. (.-^ (t .-. 0))) . (. (^/ d)) $ f        
instance Transformable (Segment a) where
















-- newtype Time -- Semigroup, Monoid (sum)
-- newtype Span -- Semigroup, Monoid, AdditiveGroup (composition)
--
-- spans :: Iso (Time^2) (Time, Dur)
-- spans = iso (\(t,d) -> (t,t.+^d)) (\(t,u) -> (t,u.-.t))
--
--
--
--

-- |
-- A 'Note' is a value with a known 'position' and 'duration'.
-- Notes are isomorphic pairs of spans and values, as whitnessed by 'note'.
--
-- Another way is to view a note is that it is a suspended application of a time
-- transformation, with 'runNote' extracting the transformed value:
--
-- There is a morphism from 'runNote' to 'transform':
--
-- > runNote . transform s = transform s . runNote
--
newtype Note a      = Note      { getNote :: (Span, a)     } deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Applicative, Comonad, Foldable, Traversable)

-- | Note is a 'Monad' and 'Applicative' in the style of pair, with 'return' placing a value
--   at the default span 'mempty' and 'join' composing time transformations.
deriving instance Monad Note

-- |
-- A 'Delayed' value has a known 'position', but no duration.
--
newtype Delayed a   = Delayed   { getDelayed :: (Time, a)     } deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Applicative, Monad, Comonad, Foldable, Traversable)

-- |
-- A 'Stretched' value has a known 'position', but no duration.
--
newtype Stretched a = Stretched { getStretched :: (Duration, a) } deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Applicative, Monad, Comonad, Foldable, Traversable)
    
instance Reversible (Note a) where
    rev = stretch (-1)
instance Splittable a => Splittable (Note a) where

instance Reversible (Delayed a) where

instance Reversible (Stretched a) where
    rev = stretch (-1)
instance Splittable a => Splittable (Stretched a) where

-- XXX Compare with Located in diagrams

instance Wrapped (Note a) where { type Unwrapped (Note a) = (Span, a) ; _Wrapped' = iso getNote Note }
instance Wrapped (Delayed a) where { type Unwrapped (Delayed a) = (Time, a) ; _Wrapped' = iso getDelayed Delayed }
instance Wrapped (Stretched a) where { type Unwrapped (Stretched a) = (Duration, a) ; _Wrapped' = iso getStretched Stretched }
instance Rewrapped (Note a) (Note b)
instance Rewrapped (Delayed a) (Delayed b)
instance Rewrapped (Stretched a) (Stretched b)

instance Transformable (Note a) where transform t = unwrapped $ first (transform t)
instance Transformable (Delayed a) where transform t = unwrapped $ first (transform t)
instance Transformable (Stretched a) where transform t = unwrapped $ first (transform t)

instance HasDuration (Note a) where duration = duration . ask . unwr
instance HasDuration (Stretched a) where duration = duration . ask . unwr

instance HasPosition (Note a) where x `position` p = ask (unwr x) `position` p
instance HasPosition (Delayed a) where x `position` p = ask (unwr x)`position` p

note :: Iso' (Note a) (Span, a)
note = _Wrapped'

-- noteValue :: Lens' (Note a) a
-- noteValue = value

noteEra :: Lens' (Note a) Span
noteEra = lens era undefined

mkNote :: Time -> Duration -> a -> Note a
mkNote t d v = t >-> d `transform` return v

delayed :: Iso' (Delayed a) (Time, a)
delayed = _Wrapped'

stretched :: Iso' (Stretched a) (Duration, a)
stretched = _Wrapped'

mapNote :: (Transformable a, Transformable b) => (a -> b) -> Note a -> Note b
mapNote f (Note (s,x)) = Note (s, under s f x)
-- TODO use unwr

runNote :: Transformable a => Note a -> a
runNote = uncurry transform . unwr

runDelayed :: Transformable a => Delayed a -> a
runDelayed = uncurry delay' . unwr

runStretched :: Transformable a => Stretched a -> a
runStretched = uncurry stretch . unwr

-- instance HasPosition (Note a) where position n







-- |
-- 'Bounds' restricts the start and stop time of a value.
--
newtype Bounds a      = Bounds      { getBounds :: (Span, a)     } 
    deriving (Functor, Foldable, Traversable)

bounds :: Time -> a -> Time -> Bounds a
bounds t x u = Bounds (t <-> u, x)

-- | XXX 
trim :: Monoid a => Bounds a -> Bounds a
trim = undefined

instance Reversible (Bounds a) where
    rev = stretch (-1)
instance Splittable a => Splittable (Bounds a) where
instance Wrapped (Bounds a) where { type Unwrapped (Bounds a) = (Span, a) ; _Wrapped' = iso getBounds Bounds }
instance Rewrapped (Bounds a) (Bounds b)
instance Transformable (Bounds a) where transform t = unwrapped $ first (transform t)
instance HasDuration (Bounds a) where duration = duration . ask . unwr
instance HasPosition (Bounds a) where x `position` p = ask (unwr x) `position` p

-- TODO Compare Diagram's Trail and Located (and see the conal blog post)

-- |
--
-- A 'Segment' is a function of 'Duration'. Intuitively, it is a value varying over some unknown time span.
-- To place a segment in a particular time span, use 'Note' 'Segment'.
--
-- Segment is a 'Monad' and 'Applicative' functor, similar to the function instance:
--
-- > pure s ! t == s
--
-- > fs <*> xs ! t == (fs ! t) (xs ! t)
--
-- > join s ! t == (s ! t) ! t
--
newtype Segment a = Segment (Normalized Duration -> a) deriving (Functor, Applicative, Monad{-, Comonad-})
-- Defined 0-1
instance Semigroup a => Semigroup (Segment a) where
    (<>) = undefined
instance Monoid a => Monoid (Segment a) where

type instance Key Segment = Duration

instance Lookup Segment where
    t `lookup` Segment b = b <$> t ^? normalize
instance Indexable Segment where
    Segment b `index` t = b $ t ^?! normalize

-- |
--
-- A 'Behavior' is a function of 'Time'. Intuitively, it is a value varying over the set of all time points.
-- While a 'Behavior' can not be placed (as it has no endpoints), it can be "focused", by placing it inside
-- 'Bounds'.
--
-- Behavior is a 'Monad' and 'Applicative' functor, similar to the function instance:
--
-- > pure s ! t == s
--
-- > fs <*> xs ! t == (fs ! t) (xs ! t)
--
-- > join s ! t == (s ! t) ! t
--
newtype Behavior a  = Behavior (Time -> a)     deriving (Functor, Applicative, Monad, Comonad)
-- Defined throughout, "focused" on 0-1
deriving instance Semigroup a => Semigroup (Behavior a)
deriving instance Monoid a => Monoid (Behavior a)
deriving instance Num a => Num (Behavior a)
deriving instance Fractional a => Fractional (Behavior a)
deriving instance Floating a => Floating (Behavior a)

type instance Key Behavior = Time

instance Lookup Behavior where
    lookup = lookupDefault
instance Indexable Behavior where
    Behavior b `index` t = b t



time :: Fractional a => Behavior a
time = Behavior realToFrac

a :: Behavior Float
a = time


-- TODO these are examples...

-- Use infix
isIn :: Time -> Span -> Bool
isIn x (view range -> (t, u)) = t <= x && x <= u

-- TODO compose segments etc
adsr :: Behavior Duration
adsr = time <&> \t -> 
    if t `isIn` (0    <-> 0.15) then lerp 0   1   ((t .-. 0)^/0.15)   else
    if t `isIn` (0.15 <-> 0.3)  then lerp 1   0.3 ((t .-. 0.15)^/0.15) else
    if t `isIn` (0.3  <-> 0.65) then lerp 0.3 0.2 ((t .-. 0.3)^/0.35) else 
    if t `isIn` (0.65 <-> 1.0)  then lerp 0.2 0   ((t .-. 0.65)^/0.35) else 
    0

toFloat :: Real a => a -> Float
toFloat = realToFrac

modulate :: Floating (Pitch a) => Behavior (Pitch a -> Pitch a)
modulate = (\t x -> x * sin (t*2*pi)) <$> time



test = openG $ (<> grid) $ drawBehavior (r*5) <> lc blue (drawBehavior (c*5)) <> drawNote (fmap (fmap snd) nc)
    where
        -- c = 1
c = (sin (time/20*2*pi))

c2 :: Behavior Float -> Behavior Float
c2  = liftA2 (*) c

nc :: Note (Behavior (Int, Float))
nc = transform (4.5 >-> 5) $ return $ fmap (0,) $ fmap toFloat adsr

r :: Behavior Float
r  = fmap snd $ runNote (nc & pitch %~ c2)

drawNote :: (Real a, Renderable (Path R2) b) => Note a -> Diagram b R2
drawNote n = let
    (t,d) = view delta $ era n
    a = n ^?! traverse
    in drawNote' (t,d,a)

instance Eq a => Eq (Behavior a) where
instance Ord a => Ord (Behavior a) where
instance Real a => Real (Behavior a) where
    toRational = toRational . (! 0)




newtype Score a     = Score      { getScore :: [(Span, a)]     } deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Semigroup, Monoid)
instance Wrapped (Score a) where { type Unwrapped (Score a) = [(Span, a)] ; _Wrapped' = iso getScore Score }
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







-- | XXX indexed traversal?
score :: Traversal (Voice a) (Voice b) (Note a) (Note b)
score = undefined

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
newtype Voice a     = Voice      { getVoice :: Seq (Stretched a)     } deriving ({-Eq, -}{-Ord, -}{-Show, -}
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
instance Wrapped (Voice a) where { type Unwrapped (Voice a) = (Seq (Stretched a)) ; _Wrapped' = iso getVoice Voice }
instance Rewrapped (Voice a) (Voice b)

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
        Nothing     -> Left mempty

type instance Index (Voice a) = Int
type instance IxValue (Voice a) = Stretched a
instance Ixed (Voice a) where
    ix n = _Wrapped' . ix n
-}

stretchedToVoice :: Stretched a -> Voice a
stretchedToVoice x = Voice (return x)

-- | XXX indexed traversal?
voice :: Traversal (Voice a) (Voice b) (Stretched a) (Stretched b)
voice = undefined

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

voiceList :: Iso' (Divide a) (NonEmpty (Voices a))
voiceList = undefined

newtype Voices a     = Voices      { getVoices :: Seq (Stretched a)     } deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Semigroup, Monoid)
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
--     type Rep = Time
-- instance Applicative Behavior
-- instance Monad Behavior
-- instance Monoid a => Monoid (Behavior a)
--
-- newtype Track a = [(Time, a)]
--     -- XXX Start time, laziness
--     -- Functor, Monad
-- newtype Score a = [(Span, a)]
-- -- XXX Start time, laziness
--     -- Functor, Monad
--
--
-- newtype Reactive a = (Time -> (a, Duration^2))
-- -- XXX Start time, laziness
-- -- Distributive?
-- instance Representable Reactive where
--     type Rep = Time
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







-- |
-- Functors with a single readable and writable value.
--
-- XXX Always isomorphic to pairs?
--
-- For any @'Comonad' f@ , you can simply define:
--
-- > instance Single f where
-- >     value = valueDefault
--
class Functor f => Single f where
    value' :: Lens' (f a) a
    value  :: Lens  (f a) (f a) a a
    value = value'

instance Single ((,) a) where
    value = valueDefault

left3 :: Iso ((a1, b1), c1) ((a2, b2), c2) (a1, b1, c1) (a2, b2, c2)
left3 = iso f g where f ((x,y), z) = (x, y, z); g (x, y,  z) = ((x, y), z)

right3 :: Iso (a1, (b1, c1)) (a2, (b2, c2)) (a1, b1, c1) (a2, b2, c2)
right3 = iso f g where f (x, (y,z)) = (x, y, z); g (x, y,  z) = (x, (y,z))

-- TODO run 3 version over this
left4 = iso f g where f ((x,y,z), a) = (x, y, z, a); g (x, y, z, a) = ((x, y, z), z)
right4 = iso f g where f (x, (y,z,a)) = (x, y, z, a); g (x, y, z, a) = (x, (y, z, a))


-- Why is this not in base?
instance Functor ((,,) a b) where
    fmap f = from left3 %~ fmap f

instance Single ((,,) a b) where
    value = from left3 . valueDefault

-- | XXX suspect setter (uses const)
instance Monoid a => Single ((->) a) where
    value = valueDefault


-- Iso (g a) (g b) (c, a) (c, b)

valueDefault :: Comonad g => Lens (g a) (g b) a b
valueDefault = lens extract (\s b -> fmap (const b) s)


-- Moving and scaling things

-- FIXME compare with diagrams variant
-- translation vs linear etc

-- |
-- Law
--
-- > transform s . transform t = transform (s <> t)
--
-- Law
--
-- > onset (delay n a) = n + onset a
-- > offset (delay n a) = n + offset a
-- > duration (stretch n a) = n * (duration a)
--
-- Lemma
--
-- > duration a = duration (delay n a)
class Transformable a where
    transform :: Span -> a -> a

instance Transformable a => Transformable (a, b) where
    transform t (s,a) = (transform t s, a)

-- FIXME strange
-- transformInv (view delta -> (t,d)) = stretch (recip d) . delay' (reflectThrough 0 t)

-- FIXME get rid of this
delay' :: Transformable a => Time -> a -> a
delay' t     = delay (t .-. 0)


-- |
-- Move a value forward in time.
--
delaying x   = (0 .+^ x) >-> 1

-- |
-- Move a value forward in time.
--
stretching x = 0         >-> x

-- |
-- Move a value forward in time.
--
undelaying x = delaying (negate x)

-- |
-- Move a value forward in time.
--
compressing x = stretching (recip x)

-- |
-- Move a value forward in time.
--
delay    = transform . delaying

-- |
-- Move a value backward in time. Equivalent to @'delay' . 'negate'@.
--
undelay  = transform . undelaying

-- |
-- Stretch (augment) a value by the given factor.
--
stretch  = transform . stretching

-- |
-- Compress (diminish) a score. Equivalent to @'stretch' . 'recip'@.
--
compress = transform . compressing

-- Fitting things

-- Things with a duration

-- |
-- Law Duration
--
-- > duration x = (offset x .-. onset x)
--
class HasDuration a where
    duration :: a -> Duration

stretchTo :: (Transformable a, HasDuration a) => Duration -> a -> a
stretchTo d x = (d ^/ duration x) `stretch` x

stretchNorm :: (Transformable a, HasDuration a, InnerSpace Duration) => a -> a
stretchNorm x = stretchTo (normalized $ duration x) x


-- Placing things

class HasPosition a where
    position :: a -> {-Scalar-} Duration -> Time

-- |  XXX make into lens for any positionable thing
era :: HasPosition a => a -> Span
era x = onset x <-> offset x

-- |
-- Return the onset of the given value.
--
onset :: (HasPosition a{-, Fractional s, s ~ (Scalar (Duration))-}) => a -> Time
onset       = (`position` 0)

-- |
-- Return the offset of the given value.
--
offset :: (HasPosition a{-, Fractional s, s ~ (Scalar (Duration))-}) => a -> Time
offset      = (`position` 1.0)

preOnset    = (`position` (-0.5))
postOnset   = (`position` 0.5)
postOffset  = (`position` 1.5)

-- |
-- Move a value forward in time.
--
startAt :: (Transformable a, HasPosition a) => Time -> a -> a
startAt t x   = (t .-. onset x) `delay` x

-- |
-- Move a value forward in time.
--
stopAt  :: (Transformable a, HasPosition a) => Time -> a -> a
stopAt t  x   = (t .-. offset x) `delay` x

-- |
-- Align a value to a given position.
--
-- @alignAt p t@ places the given thing so that its position p is at time t
--
-- > alignAt 0 == startAt
-- > alignAt 1 == stopAt
--
alignAt :: (Transformable a, HasPosition a) => Duration -> Time -> a -> a
alignAt p t x = (t .-. x `position` p) `delay` x


-- | 
-- a `lead`   b  moves a so that (offset a' == onset b)
-- 
lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
a `lead` b   = alignAt 1 (b `position` 0) a

-- |
-- a `follow` b  moves b so that (offset a  == onset b')
-- 
follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
a `follow` b = alignAt 0 (a `position` 1) b

-- | 
-- a `lead`   b  moves a so that (offset a' == onset b)
-- 
after :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `after` b =  a <> (a `follow` b)

-- | 
-- a `lead`   b  moves a so that (offset a' == onset b)
-- 
before :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `before` b =  (a `lead` b) <> b

pinned :: (HasPosition a, Transformable a) => (a -> a) -> a -> a
pinned f x = startAt (onset x) (f x)

scat = Prelude.foldr (//) mempty
pcat = Prelude.foldr (><) mempty

x `sustain` y     = x <> duration x `stretchTo` y
times n     = scat . replicate n

-- | Splitting and reversing things
--
--
-- Works for both positioned and unpositioned things
-- For positioned types, splits relative onset
-- XXX would some instances look nicer if duration was absolute (compare mapping) and is this a bad sign?
-- XXX what about Behavior (infinite span)
--
-- Law
--
-- > let (a, b) = split x in duration a + duration b = duration x
--
class HasDuration a => Splittable a where
    split  :: Duration -> a -> (a, a)




class Reversible a where
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


class Sequential a where
    (//) :: a -> a -> a
    (\\) :: a -> a -> a
instance Sequential (Voice a) where
    (//) = (<>)
    (\\) = flip (<>)
instance Sequential (Voices a) where
    (//) = (<>)
    (\\) = flip (<>)
instance Sequential (Score a) where
    (//) = after
    (\\) = before

class Parallel a where
    (><) :: a -> a -> a
-- instance Parallel (Divide a) where
    -- (><) = (<>)
instance Parallel (Score a) where
    (><) = (<>)


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
--     take
--     drop
-- duration
-- position
--     onset
--     offset
-- transform
--     delay
--     stretch
-- scat
-- pcat
--
-- -- a `lead`   b  moves a so that (offset a' == onset b)
-- -- a `follow` b  moves b so that (offset a  == onset b')
-- lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
-- follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
--






















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

instance Monad m => Serial m Time where
    series = msum $ fmap return [1..2]
instance Monad m => Serial m Duration where
    series = msum $ fmap return [0..2]
instance Monad m => Serial m Span where
    series = newtypeCons Span
instance Serial IO a => Serial IO (BadFunctor a) where
    series = cons0 BF1 \/ cons0 BF2
instance Serial IO a => Serial IO (BadMonoid a) where
    series = newtypeCons BadMonoid
instance Serial IO Int8 where
    series = msum $ fmap return [0..2]

monoid :: (Monoid t, Eq t, Show t, Typeable t, Serial IO t) => t -> TestTree
monoid typ = testGroup ("instance Monoid " ++ show (typeOf typ)) $ [
    testProperty "x <> (y <> z) == (x <> y) <> z" $ \x y z -> assuming (sameType typ x)
                  x <> (y <> z) == (x <> y) <> z,

    testProperty "mempty <> x == x"               $ \x     -> assuming (sameType typ x)
                  mempty <> x == x,

    testProperty "x <> mempty == x"               $ \x     -> assuming (sameType typ x)
                 (x <> mempty == x)
    ]
    where
        (<>) = mappend

-- functor :: (Functor f, Eq (f b), Show (f b), Typeable b, Typeable1 f, Serial IO (f b)) => f b -> TestTree
-- functor typ = testGroup ("instance Functor " ++ show (typeOf typ)) $ [
--     testProperty "fmap id = id" $ \x -> assuming (sameType typ x)
--                  (fmap id x == id x)
--     ]

-- applicative :: (Applicative f, Eq (f b), Show (f b), Typeable b, Typeable1 f, Serial IO (f b)) => f b -> TestTree
-- applicative typ = testGroup ("instance Applicative " ++ show (typeOf typ)) $ [
--
--     testProperty "pure id <*> v = v" $ \x -> assuming (sameType typ x) ((pure id <*> x) == x),
--
--     testProperty "pure (.) <*> u <*> v <*> w = u <*> (v <*> w)"
--         $ \u v w -> assuming (sameType typ w)
--             ((pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w)))
--
--     ]


ap2 u v w = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

main = defaultMain $ testGroup "" $ [
    testProperty "rev . rev == id" $ \(x :: ())    -> rev (rev x) == x,
    testProperty "rev . rev == id" $ \(x :: [Int]) -> rev (rev x) == x,

    monoid (undefined :: ()),
    monoid (undefined :: Maybe ()),
    monoid (undefined :: [()]),
    monoid (undefined :: BadMonoid Int8),

    monoid (undefined :: Time),
    monoid (undefined :: Duration),
    monoid (undefined :: Span)

    -- functor (undefined :: BadFunctor Int8),
    -- functor (undefined :: BadMonoid Int8)

    ]








drawScore' :: (Renderable (Path R2) b, Real a) =>       [[(Time, Duration, a)]] -> Diagram b R2
drawScore' = vcat' (def & sep .~ 2) . fmap drawPart'

drawPart' :: (Renderable (Path R2) b, Real a) =>        [(Time, Duration, a)] -> Diagram b R2
drawPart' = mconcat . fmap drawNote'

drawNote' :: (Renderable (Path R2) b, Real a) => (Time, Duration, a) -> Diagram b R2
drawNote' (realToFrac -> t, realToFrac -> d, realToFrac -> y) = translateY y $ translateX t $ scaleX d $ noteShape
    where
    noteShape = {-showOr $-} lcA transparent $ fcA (blue `withOpacity` 0.5) $ strokeLoop $ closeLine $ fromOffsets [r2 (1,0), r2 (-0.8,0.2), r2 (-0.2,0.8)]

drawBehavior :: (Renderable (Path R2) b, Real a) =>  Behavior a -> Diagram b R2
drawBehavior = drawBehavior' 50

drawBehavior' count b = cubicSpline False points & lw 0.05
    where
        points = take (samplesPerCell*count) $ fmap (\x -> p2 (x, realToFrac $ b ! realToFrac x)) [0,1/samplesPerCell..]
        samplesPerCell = 40

grid = grid'   20
gridX = gridX' 20
gridY = gridY' 20

grid' ds = {-showOr $ -}moveOriginTo (p2 (realToFrac ds/2,-(realToFrac ds/2))) $ (gridX <> gridY & lc lightblue)

gridY' :: (Renderable (Path R2) b) => Int -> Diagram b R2
gridY' ds = alignTL $ hcat' (def & sep .~ 1) $ replicate (ds+1) $ vrule (realToFrac ds)

gridX' :: (Renderable (Path R2) b) => Int -> Diagram b R2
gridX' ds = alignTL $ vcat' (def & sep .~ 1) $ replicate (ds+1) $ hrule (realToFrac ds)

writeG :: (a ~ SVG.SVG) => FilePath -> Diagram a R2 -> IO ()
writeG path dia = do
    let svg = renderDia SVG.SVG (SVG.SVGOptions (Height 300) Nothing) dia
    let bs  = renderSvg svg
    ByteString.writeFile path bs
        
openG :: (a ~ SVG.SVG) => Diagram a R2 -> IO ()
openG dia = do
    writeG "test.svg" $ dia -- 
    -- FIXME find best reader
    system "echo '<img src=\"test.svg\"></img>' > test.html"
    -- system "open -a 'Firefox' test.html"
    system "osascript -e 'tell application \"Google Chrome\" to tell the active tab of its first window' -e 'reload' -e 'end tell'"
    return ()    