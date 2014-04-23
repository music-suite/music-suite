
{-# LANGUAGE
    CPP,
    DeriveFunctor,
    DefaultSignatures,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    TypeFamilies,
    TypeOperators,
    MultiParamTypeClasses,
    NoMonomorphismRestriction,
    UndecidableInstances,
    RankNTypes,
    ScopedTypeVariables,
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
-- Provides functions for manipulating pitch.
--
-------------------------------------------------------------------------------------


module Music.Score.Pitch (     
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
        fifthsUp,
        fifthsDown,
        fifthsAbove,
        fifthsBelow,
        -- ** Intervals
        augmentIntervals,
        
        -- TODO pitchIs, to write filter pitchIs ... etc        
        -- TODO gliss etc
        
        -- -- * Accessors
        -- pitch',
        -- pitch,
        -- -- pitch_,
        -- -- pitches',
        -- -- pitches,
        -- 
        -- -- * Transformations
        -- -- ** Transformations
        -- inv,
        -- 
        -- -- ** Transformations
        -- up,
        -- down,
        -- fifthsUp,
        -- fifthsDown,
        -- octavesUp,
        -- octavesDown,
        -- 
        -- -- ** Transformations
        -- -- above,
        -- -- below,
        -- fifthsAbove,
        -- fifthsBelow,
        -- octavesAbove,
        -- octavesBelow,
        -- 
        -- -- * Pitch representation
        -- Pitch,
        -- Interval,
        -- HasGetPitch(..),
        -- HasSetPitch(..),
        -- HasPitch'(..),
        -- HasPitch(..),
        -- HasSetPitch'(..),
        -- Transposable,
        -- Transposable1,

  ) where

import Control.Monad (ap, mfilter, join, liftM, MonadPlus(..))
import Control.Applicative
import Control.Lens hiding (above, below)
import Data.Semigroup
import Data.String
import Data.Typeable
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Ratio
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import qualified Data.List as List

import Music.Time
import Music.Pitch.Literal

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
-- Transpose up by the given number of fifths.
--
fifthsUp :: Transposable a => Scalar (Interval a) -> a -> a
fifthsUp n = up (_P8^*n)

-- |
-- Transpose down by the given number of fifths.
--
fifthsDown :: Transposable a => Scalar (Interval a) -> a -> a
fifthsDown n = down (_P8^*n)

-- |
-- Add the given octave above.
--
fifthsAbove :: (Semigroup a, Transposable a) => Scalar (Interval a) -> a -> a
fifthsAbove n = above (_P8^*n)

-- |
-- Add the given octave below.
--
fifthsBelow :: (Semigroup a, Transposable a) => Scalar (Interval a) -> a -> a
fifthsBelow n = below (_P8^*n)

-- |
augmentIntervals :: Transposable a => Interval a -> Voice a -> Voice a
augmentIntervals = error "Not implemented: augmentIntervals"
-- TODO generalize to any type where we can traverse phrases of something that has pitch


-- TODO augment/diminish intervals (requires withPrev or similar)
-- TODO invert diatonically
-- TODO rotatePitch (requires some kind of separate traversal)

{-
-- This is outside HasGetPitch etc because HasSetPitch needs it
-- (and it allow us to derive more instances, see #95)
type family Pitch a

type Interval a = Diff (Pitch a)

-- |
-- Class of types with readable pitch.
--
class HasGetPitch s where
  __getPitch :: (a ~ Pitch s) => s -> a

-- |
-- Class of types with mutable pitch.
--
-- Either 'setPitch' or 'mapPitch' can be implemented. If both are implemented,
-- the following laws should be satisfied:
--
-- > setPitch x = mapPitch (const x)
-- > mapPitch f x = setPitch p x where p = f (__getPitch x)
--
-- For types that are 'Functors', the following instance can be used
--
-- > type instance Pitch (T a) = Pitch a
-- > instance HasSetPitch a b => HasSetPitch (T a) (T b) where
-- >     type SetPitch g (T a) = T (SetPitch g a)
-- >     mapPitch = fmap . mapPitch
--
class (SetPitch (Pitch t) s ~ t) => HasSetPitch (s :: *) (t :: *) where
  type SetPitch (b :: *) (s :: *) :: *

  __setPitch :: Pitch t -> s -> t
  __setPitch x = __mapPitch (const x)
  
  __mapPitch :: (Pitch s -> Pitch t) -> s -> t
  default __mapPitch :: HasGetPitch s => (Pitch s -> Pitch t) -> s -> t
  __mapPitch f x = __setPitch p x where p = f (__getPitch x)
  
type HasPitch s t = (HasGetPitch s, HasSetPitch s t)

type HasPitch' a = HasPitch a a
type HasSetPitch' a = HasSetPitch a a

-- | A lens to the pitch in a note, score or other structure.  
--
pitch' :: HasPitch' a => Lens' a (Pitch a)
pitch' = pitch

-- | A lens to the pitch in a note, score or other structure.  
--
pitch :: HasPitch a b => Lens a b (Pitch a) (Pitch b)
pitch = lens __getPitch (flip __setPitch)

-- | A setter to the pitch in a note, score or other structure.  
--
pitch_ :: HasSetPitch a b => Setter a b (Pitch a) (Pitch b)
pitch_ = sets __mapPitch

-- | Traverses all pitches in structure.  
--
pitches' :: (Traversable t, HasPitch' a) => Traversal' (t a) (Pitch a) 
pitches' = traverse . pitch'

-- | Traverses all pitches in structure.  
--
pitches :: (Traversable t, HasPitch a b) => Traversal (t a) (t b) (Pitch a) (Pitch b) 
pitches = traverse . pitch


type HasPitchConstr a = (
    HasPitch' a, 
    VectorSpace (Interval a), Integer ~ Scalar (Interval a),
    AffineSpace (Pitch a)
    )

newtype PitchT p a = PitchT { __getPitchT :: (p, a) }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Semigroup p, Monoid p) => Applicative (PitchT p) where
    pure x = PitchT (mempty,x)
    PitchT (pf,vf) <*> PitchT (px,vx) = PitchT (pf <> px, vf $ vx)

-- TODO move these
instance Stretchable ()
instance Stretchable Double
instance Stretchable Float
instance Stretchable Int
instance Stretchable Integer
instance Integral a => Stretchable (Ratio a)

instance Delayable ()
instance Delayable Double
instance Delayable Float
instance Delayable Int
instance Delayable Integer
instance Integral a => Delayable (Ratio a)

type instance Pitch (c,a) = Pitch a
instance HasGetPitch a => HasGetPitch (c,a) where
    __getPitch (c,a) = __getPitch a

-- Undecidable ??
instance (HasGetPitch a, HasSetPitch a b) => HasSetPitch (c,a) (c,b) where
  type SetPitch b (c,a) = (c,SetPitch b a)
  __setPitch b = fmap (__setPitch b)

#define HAS_PITCH_PRIM(T)   \
type instance Pitch T = T; \
instance HasGetPitch T where { \
    __getPitch = id }
    
#define HAS_SET_PITCH_PRIM(T)   \
instance (a ~ Pitch a) => HasSetPitch T a where { \
    type SetPitch a T = a; \
    __mapPitch = id }

HAS_PITCH_PRIM(())
HAS_PITCH_PRIM(Bool)
HAS_PITCH_PRIM(Double)
HAS_PITCH_PRIM(Float)
HAS_PITCH_PRIM(Int)
HAS_PITCH_PRIM(Integer)

HAS_SET_PITCH_PRIM(())
HAS_SET_PITCH_PRIM(Bool)
HAS_SET_PITCH_PRIM(Double)
HAS_SET_PITCH_PRIM(Float)
HAS_SET_PITCH_PRIM(Int)
HAS_SET_PITCH_PRIM(Integer)

-- type Transposable p i = (Diff p ~ i, AffineSpace p, VectorSpace i, IsPitch p, IsInterval i)
type Transposable a = 
        (
            HasSetPitch' a, 
            Transposable1 a
        )
type Transposable1 a =
    (
            Diff (Pitch a) ~ Interval a,
            AffineSpace (Pitch a), 
            VectorSpace (Interval a),
            IsPitch (Pitch a), 
            IsInterval (Interval a)
    )
    

-- |
-- Transpose up.
--
up :: Transposable a => Interval a -> a -> a
up a = pitch_ %~ (.+^ a)

-- |
-- Transpose down.
--
down :: Transposable a => Interval a -> a -> a
down a = pitch_ %~ (.-^ a)

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
inv p = pitch_ %~ (reflectThrough p)

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
-- Add the given interval below.
--
fifthsUp :: Transposable a => Scalar (Interval a) -> a -> a
fifthsUp a     = up (_P8^*a)

-- |
-- Add the given interval below.
--
fifthsDown :: Transposable a => Scalar (Interval a) -> a -> a
fifthsDown a   = down (_P8^*a)


-- |
-- Add the given interval below.
--
octavesAbove :: (Semigroup a, Transposable a) => Scalar (Interval a) -> a -> a
octavesAbove n x = x <> octavesUp n x

-- |
-- Add the given interval below.
--
octavesBelow :: (Semigroup a, Transposable a) => Scalar (Interval a) -> a -> a
octavesBelow n x = x <> octavesUp n x

-- |
-- Add the given interval below.
--
fifthsAbove :: (Semigroup a, Transposable a) => Scalar (Interval a) -> a -> a
fifthsAbove n x = x <> fifthsUp n x

-- |
-- Add the given interval below.
--
fifthsBelow :: (Semigroup a, Transposable a) => Scalar (Interval a) -> a -> a
fifthsBelow n x = x <> fifthsUp n x

{-

highestPitch = maximum . __getPitches
lowestPitch = maximum . __getPitches
meanPitch = mean . __getPitches
mean x = fst $ foldl (\(m, n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x 

-}
-}
