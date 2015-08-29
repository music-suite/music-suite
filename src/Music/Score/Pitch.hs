
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Provides functions for manipulating pitch.
module Music.Score.Pitch (
        
        -- * Pitch type
        Pitch,
        SetPitch,
        Interval,
        
        -- * HasPitch classes
        HasPitch(..),
        HasPitches(..),
        -- fromPitch',

        -- ** Simple versions
        HasPitch',
        HasPitches',
        pitch',
        pitches',
        
        -- * Transposition
        PitchPair,
        AffinePair,
        Transposable,
        up,
        down,
        above,
        below,
        octavesUp,
        octavesDown,
        _15va,
        _8va,
        _8vb,
        _15vb,
        upDiatonic,
        downDiatonic,
        upChromatic,
        downChromatic,

        -- * Inversion
        invertPitches,
        invertDiatonic,
        invertChromatic,
        
        -- * Ambitus
        highestPitch,
        lowestPitch,
        averagePitch,
        ambitusOctaves,
        ambitusLowestOctave,
        interpolateAmbitus,
        interpolateAmbitus',
        
        -- * Enumeration
        enumDiatonicFromTo,
        enumChromaticFromTo,
        enumDownDiatonicFromTo,
        enumDownChromaticFromTo,
        
        -- * Utility
        printPitches,

  ) where

import           Control.Applicative
import           Control.Lens                  hiding (above, below, transform)
import           Control.Monad                 (MonadPlus (..), ap, join, liftM,
                                                mfilter)
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Foldable                 (Foldable)
import           Data.Functor.Couple
import qualified Data.List
import           Data.Ratio
import           Data.Semigroup
import           Data.String
import           Data.Monoid.Average
import           Data.Traversable              (Traversable)
import           Data.Typeable
import           Data.VectorSpace              hiding (Sum)
import           Data.Set                      (Set)
import           Data.Map                      (Map)
import           Data.Sequence                 (Seq)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import           Music.Pitch.Ambitus
import           Music.Pitch.Absolute
import           Music.Pitch.Common hiding (Pitch, Interval)
import qualified Music.Pitch.Common as Common
import           Music.Pitch.Literal
import           Music.Score.Harmonics
import           Music.Score.Part
import           Music.Score.Slide
import           Music.Score.Text
import           Music.Score.Ties
import           Music.Score.Phrases
import           Music.Time
import           Music.Time.Internal.Transform

-- |
-- This type fuction is used to access the pitch type for a given type.
--
type family Pitch (s :: *) :: *
--
-- @
-- 'Pitch' (c,a)             ~ 'Pitch' a
-- 'Pitch' [a]               ~ 'Pitch' a
-- 'Pitch' ('Event' a)          ~ 'Pitch' a
-- 'Pitch' ('Voice' a)         ~ 'Pitch' a
-- 'Pitch' ('Score' a)         ~ 'Pitch' a
-- @
--
-- For types representing pitch, it is generally 'Identity', i.e
--
-- @
-- Pitch Integer ~ Integer
-- Pitch Double ~ Double
-- @
--
-- and so on.
--
-- For containers, 'Pitch' provides a morphism:
--

-- |
-- This type fuction is used to update the pitch type for a given type.
-- The first argument is the new type.
--
type family SetPitch (b :: *) (s :: *) :: *
--
-- @
-- 'SetPitch' b (c,a)          ~ (c, 'SetPitch' b a)
-- 'SetPitch' b [a]            ~ ['SetPitch' b a]
-- 'SetPitch' g ('Event' a)       ~ Event ('SetPitch' g a)
-- 'SetPitch' g ('Voice' a)      ~ 'Voice' ('SetPitch' g a)
-- 'SetPitch' g ('Score' a)      ~ 'Score' ('SetPitch' g a)
-- @
--
-- For types representing pitch, it is generally 'Constant', i.e
--
-- @
-- SetPitch a Double ~ a
-- SetPitch a Integer ~ a
-- @
--
-- For containers, 'SetPitch' provides a morphism:
--

-- |
-- Class of types that provide a single pitch.
--
class HasPitches s t => HasPitch s t where

  -- | Access the pitch.
  pitch :: Lens s t (Pitch s) (Pitch t)

-- TODO move doc
--
--   As this is a 'Traversal', you can use all combinators from the lens package,
--   for example:
--
--   @
--   'pitch' .~ c    :: ('HasPitch'' a, 'IsPitch' a)      => a -> a
--   'pitch' +~ 2    :: ('HasPitch'' a, 'Num' ('Pitch' a))  => a -> a
--   'pitch' %~ 'succ' :: ('HasPitch'' a, 'Enum' ('Pitch' a)) => a -> a
--   'view' 'pitch'    :: 'HasPitches'' a                 => a -> 'Pitch' a
--   'set'  'pitch'    :: 'HasPitches' a b                => 'Pitch' b -> a -> b
--   'over' 'pitch'    :: 'HasPitches' a b                => ('Pitch' a -> 'Pitch' b) -> a -> b
--   @
--


-- |
-- Class of types that provide zero or more pitches.
--
class (Transformable (Pitch s),
       Transformable (Pitch t),
       SetPitch (Pitch t) s ~ t) => HasPitches s t where

  -- | Access all pitches.
  pitches :: Traversal s t (Pitch s) (Pitch t)

type HasPitch' a = HasPitch a a

type HasPitches' a = HasPitches a a


-- | 
-- Access the pitch.
--
-- Same as 'pitch', but without polymorphic update.
--
pitch' :: HasPitch' s => Lens' s (Pitch s)
pitch' = pitch
{-# INLINE pitch' #-}

-- | 
-- Access all pitches.
-- 
-- Same as 'pitches', but without polymorphic update.
--
pitches' :: HasPitches' s => Traversal' s (Pitch s)
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


type instance Pitch (c,a)                 = Pitch a
type instance SetPitch b (c,a)            = (c,SetPitch b a)
type instance Pitch [a]                   = Pitch a
type instance SetPitch b [a]              = [SetPitch b a]
type instance Pitch (Map k a)             = Pitch a
type instance SetPitch b (Map k a)        = Map k (SetPitch b a)
type instance Pitch (Seq a)               = Pitch a
type instance SetPitch b (Seq a)          = Seq (SetPitch b a)

type instance Pitch (Maybe a)             = Pitch a
type instance SetPitch b (Maybe a)        = Maybe (SetPitch b a)
type instance Pitch (Either c a)          = Pitch a
type instance SetPitch b (Either c a)     = Either c (SetPitch b a)

type instance Pitch (Event a)             = Pitch a
type instance SetPitch b (Event a)        = Event (SetPitch b a)
type instance Pitch (Placed a)            = Pitch a
type instance SetPitch b (Placed a)       = Placed (SetPitch b a)
type instance Pitch (Note a)              = Pitch a
type instance SetPitch b (Note a)         = Note (SetPitch b a)

type instance Pitch (Voice a)             = Pitch a
type instance SetPitch b (Voice a)        = Voice (SetPitch b a)
type instance Pitch (Track a)             = Pitch a
type instance SetPitch b (Track a)        = Track (SetPitch b a)
type instance Pitch (Score a)             = Pitch a
type instance SetPitch b (Score a)        = Score (SetPitch b a)

type instance Pitch (Aligned a) = Pitch a
type instance SetPitch b (Aligned a) = Aligned (SetPitch b a)

instance HasPitches a b => HasPitches (Aligned a) (Aligned b) where
  pitches = _Wrapped . pitches

instance HasPitch a b => HasPitch (c, a) (c, b) where
  pitch = _2 . pitch
instance HasPitches a b => HasPitches (c, a) (c, b) where
  pitches = traverse . pitches

instance (HasPitches a b) => HasPitches (Event a) (Event b) where
  pitches = from event . whilstL pitches
instance (HasPitch a b) => HasPitch (Event a) (Event b) where
  pitch = from event . whilstL pitch

instance (HasPitches a b) => HasPitches (Placed a) (Placed b) where
  pitches = _Wrapped . whilstLT pitches
instance (HasPitch a b) => HasPitch (Placed a) (Placed b) where
  pitch = _Wrapped . whilstLT pitch

instance (HasPitches a b) => HasPitches (Note a) (Note b) where
  pitches = _Wrapped . whilstLD pitches
instance (HasPitch a b) => HasPitch (Note a) (Note b) where
  pitch = _Wrapped . whilstLD pitch

instance HasPitches a b => HasPitches [a] [b] where
  pitches = traverse . pitches

instance HasPitches a b => HasPitches (Seq a) (Seq b) where
  pitches = traverse . pitches

instance HasPitches a b => HasPitches (Map k a) (Map k b) where
  pitches = traverse . pitches

instance HasPitches a b => HasPitches (Maybe a) (Maybe b) where
  pitches = traverse . pitches

instance HasPitches a b => HasPitches (Either c a) (Either c b) where
  pitches = traverse . pitches

instance HasPitches a b => HasPitches (Voice a) (Voice b) where
  pitches = traverse . pitches

instance HasPitches a b => HasPitches (Track a) (Track b) where
  pitches = traverse . pitches

{-
type instance Pitch (Chord a)       = Pitch a
type instance SetPitch b (Chord a)  = Chord (SetPitch b a)
instance HasPitches a b => HasPitches (Chord a) (Chord b) where
  pitches = traverse . pitches
-}

instance (HasPitches a b) => HasPitches (Score a) (Score b) where
  pitches =
    _Wrapped . _2   -- into NScore
    . _Wrapped
    . traverse
    . from event    -- this needed?
    . whilstL pitches


type instance Pitch (Sum a) = Pitch a
type instance SetPitch b (Sum a) = Sum (SetPitch b a)

instance HasPitches a b => HasPitches (Sum a) (Sum b) where
  pitches = _Wrapped . pitches

type instance Pitch      (Behavior a) = Behavior a
type instance SetPitch b (Behavior a) = b

instance (Transformable a, Transformable b, b ~ Pitch b) => HasPitches (Behavior a) b where
  pitches = ($)
instance (Transformable a, Transformable b, b ~ Pitch b) => HasPitch (Behavior a) b where
  pitch = ($)


type instance Pitch (Couple c a)        = Pitch a
type instance SetPitch g (Couple c a)   = Couple c (SetPitch g a)
type instance Pitch (TextT a)           = Pitch a
type instance SetPitch g (TextT a)      = TextT (SetPitch g a)
type instance Pitch (HarmonicT a)       = Pitch a
type instance SetPitch g (HarmonicT a)  = HarmonicT (SetPitch g a)
type instance Pitch (TieT a)            = Pitch a
type instance SetPitch g (TieT a)       = TieT (SetPitch g a)
type instance Pitch (SlideT a)          = Pitch a
type instance SetPitch g (SlideT a)     = SlideT (SetPitch g a)

instance (HasPitches a b) => HasPitches (Couple c a) (Couple c b) where
  pitches = _Wrapped . pitches
instance (HasPitch a b) => HasPitch (Couple c a) (Couple c b) where
  pitch = _Wrapped . pitch
  
instance (HasPitches a b) => HasPitches (TextT a) (TextT b) where
  pitches = _Wrapped . pitches
instance (HasPitch a b) => HasPitch (TextT a) (TextT b) where
  pitch = _Wrapped . pitch

instance (HasPitches a b) => HasPitches (HarmonicT a) (HarmonicT b) where
  pitches = _Wrapped . pitches
instance (HasPitch a b) => HasPitch (HarmonicT a) (HarmonicT b) where
  pitch = _Wrapped . pitch

instance (HasPitches a b) => HasPitches (TieT a) (TieT b) where
  pitches = _Wrapped . pitches
instance (HasPitch a b) => HasPitch (TieT a) (TieT b) where
  pitch = _Wrapped . pitch

instance (HasPitches a b) => HasPitches (SlideT a) (SlideT b) where
  pitches = _Wrapped . pitches
instance (HasPitch a b) => HasPitch (SlideT a) (SlideT b) where
  pitch = _Wrapped . pitch


-- |
-- Associated interval type.
--
type Interval a = Diff (Pitch a)

type PitchPair  v w = (Num (Scalar v), IsInterval v, IsPitch w)
type AffinePair v w = (VectorSpace v, AffineSpace w)

-- |
-- Class of types that can be transposed, inverted and so on.
--
type Transposable a = (
  HasPitches' a,
  AffinePair (Interval a) (Pitch a),
  PitchPair (Interval a) (Pitch a)
  )

-- |
-- Transpose (translate) up.
--
-- >>> up m3 (c :: Pitch)
-- eb
--
-- >>> up _P5 [c,d,e :: Pitch]
-- [g,a,b]
--
-- >>> up _P5 [440 :: Hertz, 442, 810]
-- [g,a,b]
--
up :: Transposable a => Interval a -> a -> a
up v = pitches %~ (.+^ v)

-- |
-- Transpose (translate) down.
--
-- >>> down m3 (c :: Pitch)
-- a
--
-- >>> down _P5 [c,d,e]
-- [f_,g_,a_]
--
down :: Transposable a => Interval a -> a -> a
down v = pitches %~ (.-^ v)

-- |
-- Add the given interval above.
--
-- >>> above _P8 [c :: Pitch]
-- [c,c']
--
above :: (Semigroup a, Transposable a) => Interval a -> a -> a
above v x = x <> up v x

-- |
-- Add the given interval below.
--
-- >>> below _P8 [c :: Pitch]
-- [c,c_]
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
-- >>> octavesUp 2 (c :: Pitch)
-- c''
--
-- >>> octavesUp 1 [c,d,e]
-- [c',d',e']
--
-- >>> octavesUp (-1) [c,d,e]
-- [c_,d_,e_]
--
octavesUp :: Transposable a => Scalar (Interval a) -> a -> a
octavesUp n = up (_P8^*n)

-- |
-- Transpose down by the given number of octaves.
--
-- >>> octavesDown 2 (c :: Pitch)
-- c__
--
-- >>> octavesDown 1 [c,d,e]
-- [c_,d_,e_]
--
-- >>> octavesDown (-1) [c,d,e]
-- [c',d',e']
--
octavesDown :: Transposable a => Scalar (Interval a) -> a -> a
octavesDown n = down (_P8^*n)

-- | Same as @'octavesUp' 2@.
_15va :: Transposable a => a -> a
_15va = octavesUp 2

-- | Same as @'octavesUp' 1@.
_8va :: Transposable a => a -> a
_8va  = octavesUp 1

-- | Same as @'octavesDown' 1@.
_8vb :: Transposable a => a -> a
_8vb  = octavesDown 1

-- | Same as @'octavesDown' 2@.
_15vb :: Transposable a => a -> a
_15vb = octavesDown 2

-- | Extract the highest pitch. Returns @Nothing@ if there are none.
--
-- >>> highestPitch [c,d,e :: Pitch]
-- Just e
--
-- >>> highestPitch (Data.Map.fromList [("do",c),("re",d)] :: Data.Map.Map String Pitch)
-- Just d
--
highestPitch :: (HasPitches' a, Ord (Pitch a)) => a -> Maybe (Pitch a)
highestPitch = maximumOf pitches'

-- | Extract the lowest pitch. Returns @Nothing@ if there are none.
--
-- >>> highestPitch [c,d,e :: Pitch]
-- Just c
--
-- >>> highestPitch (Data.Map.fromList [("do",c),("re",d)] :: Data.Map.Map String Pitch)
-- Just c
--
lowestPitch :: (HasPitches' a, Ord (Pitch a)) => a -> Maybe (Pitch a)
lowestPitch = minimumOf pitches'

-- | Extract the average pitch. Returns @Nothing@ if there are none.
-- 
-- >>> averagePitch (Data.Map.fromList [(True,440::Hertz),(False,445)])
-- Just 442.5 Hz
-- 
averagePitch :: (HasPitches' a, Fractional (Pitch a)) => a -> Maybe (Pitch a)
averagePitch = maybeAverage . Average . toListOf pitches'

{-# DEPRECATED lowest "Use lowestPitch "#-}
{-# DEPRECATED highest "Use highestPitch "#-}
{-# DEPRECATED meanPitch "Use averagePitch "#-}
lowest = lowestPitch
highest = highestPitch
meanPitch = averagePitch

-- | The number of whole octaves in an ambitus.
ambitusOctaves :: Ambitus Common.Pitch -> Int
ambitusOctaves = fromIntegral . octaves . ambitusInterval

-- | The lowest octave (relative middle C) in present a given ambitus.
ambitusLowestOctave :: Ambitus Common.Pitch -> Int
ambitusLowestOctave = fromIntegral . octaves . (.-. c) . ambitusLowest

-- | Interpolate between the highest and lowest points in an ambitus.
--
-- Can be used as a primitive contour-based melody generator.
--
interpolateAmbitus :: (Ord a, Num a, AffinePair (Diff a) a) => Ambitus a -> Scalar (Diff a) -> a
interpolateAmbitus a = let (m,n) = a^.from ambitus in alerp m n

-- |
-- Same as @interpolateAmbitus@ but allow continous interpolation of standard pitch
-- (as @Scalar (Diff Pitch) ~ Integer@).
--
interpolateAmbitus' :: Ambitus Common.Pitch -> Double -> Common.Pitch
interpolateAmbitus' a x = (^.from pitchDouble) $ interpolateAmbitus (mapAmbitus (^.pitchDouble) a) x
  where
    -- We can't interpolate an (Ambitus Pitch) using fractions because of music-pitch/issues/16
    -- Work around by converting pitches into doubles and back
    -- Use Double rather than Hertz due to the latter's suspect affine space instance
    -- Only an Iso up to enharmonic equivalence.
    pitchDouble :: Iso' Common.Pitch Double
    pitchDouble = iso (\x -> fromIntegral (semitones (x.-.c))) (\x -> c .+^ spell usingSharps (round x::Semitones))

printPitches :: (HasPitches' a, Pitch a ~ p, Show p, Ord p) => a -> IO ()
printPitches x = mapM_ print $ Data.List.sort $ Data.List.nub $ toListOf pitches x


-- |
-- >>> enumDiatonicFromTo c c
-- [c]
-- >>> enumDiatonicFromTo f f'
-- [f,g,a,bb,c',d',e',f']
-- >>> 
-- >>> enumChromaticFromTo c c'
-- [c,cs,d,ds,e,f,fs,g,gs,a,as,b,c']
-- >>> 
-- >>> enumChromaticFromTo bs bs'
-- [bs,bss,css',csss',dss',es',ess',fss',fsss',gss',gsss',ass',bs']
--
enumDiatonicFromTo :: Common.Pitch -> Common.Pitch -> [Common.Pitch]
enumDiatonicFromTo  x y = takeWhile (<= y) $ fmap (\n -> upDiatonic x n x) [0..]

-- |
--
enumChromaticFromTo :: Common.Pitch -> Common.Pitch -> [Common.Pitch]
enumChromaticFromTo x y = takeWhile (<= y) $ fmap (\n -> upChromatic x n x) [0..]

-- |
--
enumDownDiatonicFromTo :: Common.Pitch -> Common.Pitch -> [Common.Pitch]
enumDownDiatonicFromTo  x y = takeWhile (>= y) $ fmap (\n -> downDiatonic x n x) [0..]

-- |
--
enumDownChromaticFromTo :: Common.Pitch -> Common.Pitch -> [Common.Pitch]
enumDownChromaticFromTo x y = takeWhile (>= y) $ fmap (\n -> downChromatic x n x) [0..]

-- |
-- >>> upDiatonic c 1 (e :: Pitch)
-- f
-- >>> upDiatonic g 1 (e :: Pitch)
-- fs
-- >>> upDiatonic c 2 [e,f,g :: Pitch]
-- [g,a,b]
-- >>> upDiatonic f 2 [e,f,g :: Pitch]
-- [g,a,bb]
upDiatonic :: (HasPitches' a, Pitch a ~ Common.Pitch) => Common.Pitch -> DiatonicSteps -> a -> a
upDiatonic o n  = over pitches' (upDiatonicP o n)

-- |
-- >>> upDiatonic c 1 (e :: Pitch)
-- f
-- >>> upDiatonic g 1 (e :: Pitch)
-- fs
-- >>> upDiatonic c 2 [e,f,g :: Pitch]
-- [g,a,b]
-- >>> upDiatonic f 2 [e,f,g :: Pitch]
-- [g,a,bb]
downDiatonic :: (HasPitches' a, Pitch a ~ Common.Pitch) => Common.Pitch -> DiatonicSteps -> a -> a
downDiatonic o n  = over pitches' (downDiatonicP o n)

-- |
-- >>> upDiatonic c 1 (e :: Pitch)
-- f
-- >>> upDiatonic g 1 (e :: Pitch)
-- fs
-- >>> upDiatonic c 2 [e,f,g :: Pitch]
-- [g,a,b]
-- >>> upDiatonic f 2 [e,f,g :: Pitch]
-- [g,a,bb]
upChromatic :: (HasPitches' a, Pitch a ~ Common.Pitch) => Common.Pitch -> ChromaticSteps -> a -> a
upChromatic o n = over pitches' (upChromaticP' o n)

-- |
-- >>> upDiatonic c 1 (e :: Pitch)
-- f
-- >>> upDiatonic g 1 (e :: Pitch)
-- fs
-- >>> upDiatonic c 2 [e,f,g :: Pitch]
-- [g,a,b]
-- >>> upDiatonic f 2 [e,f,g :: Pitch]
-- [g,a,bb]
downChromatic :: (HasPitches' a, Pitch a ~ Common.Pitch) => Common.Pitch -> ChromaticSteps -> a -> a
downChromatic o n = over pitches' (downChromaticP' o n)

-- |
-- >>> invertDiatonic c ([e,gs]^.score :: Score Pitch)
-- [(0 <-> 1,a_)^.event,(0 <-> 1,fs_)^.event]^.score
--
invertDiatonic :: (HasPitches' a, Pitch a ~ Common.Pitch) => Common.Pitch -> a -> a
invertDiatonic o = over pitches' (invertDiatonicallyP o)

-- |
-- >>> invertChromatic c ([e,gs]^.score :: Score Pitch)
-- [(0 <-> 1,e)^.event,(0 <-> 1,gb)^.event]^.score
-- 
invertChromatic :: (HasPitches' a, Pitch a ~ Common.Pitch) => Common.Pitch -> a -> a
invertChromatic o = over pitches' (invertChromaticallyP o)


downChromaticP' :: Common.Pitch -> ChromaticSteps -> Common.Pitch -> Common.Pitch
downChromaticP' o n = upChromaticP' o (-n)

upChromaticP' :: Common.Pitch -> ChromaticSteps -> Common.Pitch -> Common.Pitch
upChromaticP' _ n p
  | n >= 0 = p .+^ spell usingSharps (fromIntegral n :: Semitones)
  | n <  0 = p .-^ spell usingFlats (fromIntegral (abs n) :: Semitones)


instance Transformable Common.Pitch where
  transform _ = id
type instance Pitch Common.Pitch = Common.Pitch
type instance SetPitch a Common.Pitch = a

instance (Transformable a, a ~ Pitch a) => HasPitch Common.Pitch a where
  pitch = ($)
instance (Transformable a, a ~ Pitch a) => HasPitches Common.Pitch a where
  pitches = ($)

instance Transformable Hertz where
  transform _ = id
type instance Pitch Hertz = Hertz
type instance SetPitch a Hertz = a

instance (Transformable a, a ~ Pitch a) => HasPitch Hertz a where
  pitch = ($)
instance (Transformable a, a ~ Pitch a) => HasPitches Hertz a where
  pitches = ($)
