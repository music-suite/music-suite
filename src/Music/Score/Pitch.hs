
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
        -- * Pitch representation
        Pitch,
        Interval,
        HasGetPitch(..),
        HasSetPitch(..),
        HasPitch'(..),
        HasPitch(..),
        HasSetPitch'(..),

        -- * Accessors
        pitch',
        pitch,
        -- pitch_,
        pitches',
        pitches,

        -- * Transformations
        up,
        down,
        above,
        below,
        inv,
        octavesUp,
        octavesDown,
        octavesAbove,
        octavesBelow
  ) where

import Control.Monad (ap, mfilter, join, liftM, MonadPlus(..))
import Control.Applicative
import Control.Lens
import Data.Semigroup
import Data.String
import Data.Typeable
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Ratio
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Relative
import Data.AffineSpace.Point.Relative
import qualified Data.List as List

import Music.Time
import Music.Pitch.Literal

-- This is outside HasGetPitch etc because HasSetPitch needs it
-- (and it allow us to derive more instances, see #95)
type family Pitch a

type Interval a = Diff (Pitch a)

class HasGetPitch s where
  getPitch :: (a ~ Pitch s) => s -> a

class (SetPitch (Pitch t) s ~ t) => HasSetPitch (s :: *) (t :: *) where
  type SetPitch (b :: *) (s :: *) :: *
  setPitch :: Pitch t -> s -> t
  setPitch x = mapPitch (const x)
  
  mapPitch :: (Pitch s -> Pitch t) -> s -> t
  default mapPitch :: HasGetPitch s => (Pitch s -> Pitch t) -> s -> t
  mapPitch f x = setPitch p x where p = f (getPitch x)
  
type HasPitch s t = (HasGetPitch s, HasSetPitch s t)

-- TODO use default sigs here
mapPitchDefault :: HasPitch s t => (Pitch s -> Pitch t) -> s -> t
mapPitchDefault f x = setPitch p x where p = f (getPitch x)

type HasPitch' a = HasPitch a a
type HasSetPitch' a = HasSetPitch a a

-- | A lens to the pitch in a note, score or other structure.  
pitch' :: HasPitch' a => Lens' a (Pitch a)
pitch' = pitch

-- | A lens to the pitch in a note, score or other structure.  
pitch :: HasPitch a b => Lens a b (Pitch a) (Pitch b)
pitch = lens getPitch (flip setPitch)

-- | A setter to the pitch in a note, score or other structure.  
pitch_ :: HasSetPitch a b => Setter a b (Pitch a) (Pitch b)
pitch_ = sets mapPitch

-- | Traverses all pitches in structure.  
pitches' :: (Traversable t, HasPitch' a) => Traversal' (t a) (Pitch a) 
pitches' = traverse . pitch'

-- | Traverses all pitches in structure.  
pitches :: (Traversable t, HasPitch a b) => Traversal (t a) (t b) (Pitch a) (Pitch b) 
pitches = traverse . pitch


type HasPitchConstr a = (
    HasPitch' a, 
    VectorSpace (Interval a), Integer ~ Scalar (Interval a),
    AffineSpace (Pitch a)
    )

newtype PitchT p a = PitchT { getPitchT :: (p, a) }
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
    getPitch (c,a) = getPitch a

-- Undecidable ??
instance (HasGetPitch a, HasSetPitch a b) => HasSetPitch (c,a) (c,b) where
  type SetPitch b (c,a) = (c,SetPitch b a)
  setPitch b = fmap (setPitch b)

#define HAS_PITCH_PRIM(T)   \
type instance Pitch T = T; \
instance HasGetPitch T where { \
    getPitch = id }
    
#define HAS_SET_PITCH_PRIM(T)   \
instance (a ~ Pitch a) => HasSetPitch T a where { \
    type SetPitch a T = a; \
    mapPitch = id }

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

-- |
-- Transpose up.
--
-- > Interval -> Score a -> Score a
--
up :: (HasSetPitch' a, AffineSpace p, p ~ Pitch a) => Interval a -> a -> a
up a = pitch_ %~ (.+^ a)

-- |
-- Transpose down.
--
-- > Interval -> Score a -> Score a
--
down :: (HasSetPitch' a, AffineSpace p, p ~ Pitch a) => Interval a -> a -> a
down a = pitch_ %~ (.-^ a)

-- |
-- Add the given interval above.
--
-- > Interval -> Score a -> Score a
--
above a x = x <> up a x

-- |
-- Add the given interval below.
--
-- > Interval -> Score a -> Score a
--
below a x = x <> up a x

-- |
-- Invert pitches.
--
-- > Pitch -> Score a -> Score a
--
inv :: (HasPitch' a, AffineSpace (Pitch a)) => Pitch a -> a -> a
inv p = pitch' %~ (reflectThrough p)

-- |
-- Transpose up by the given number of octaves.
--
-- > Integer -> Score a -> Score a
--
octavesUp :: (HasSetPitch' a, p ~ Pitch a, i ~ Interval a, AffineSpace p, VectorSpace i, IsInterval i) => Scalar (Interval a) -> a -> a
octavesUp = octavesUp_

-- |
-- Transpose down by the given number of octaves.
--
-- > Integer -> Score a -> Score a
--
octavesDown :: (HasSetPitch' a, p ~ Pitch a, i ~ Interval a, AffineSpace p, VectorSpace i, IsInterval i) => Scalar (Interval a) -> a -> a
octavesDown = octavesDown_

octavesUp_ a     = up (_P8^*a)
octavesDown_ a   = down (_P8^*a)

-- |
-- Add the given interval below.
--
-- > Interval -> Score a -> Score a
--
octavesAbove :: (Semigroup a, HasSetPitch' a, p ~ Pitch a, i ~ Interval a, AffineSpace p, VectorSpace i, IsInterval i) => Scalar (Interval a) -> a -> a
octavesAbove n x = x <> octavesUp n x

-- |
-- Add the given interval below.
--
-- > Interval -> Score a -> Score a
--
octavesBelow :: (Semigroup a, HasSetPitch' a, p ~ Pitch a, i ~ Interval a, AffineSpace p, VectorSpace i, IsInterval i) => Scalar (Interval a) -> a -> a
octavesBelow n x = x <> octavesUp n x


{-}
highestPitch = maximum . getPitches
lowestPitch = maximum . getPitches
meanPitch = mean . getPitches


mean :: Floating a => [a] -> a
mean x = fst $ foldl (\(m, n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x 

-}
