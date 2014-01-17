
{-# LANGUAGE
    CPP,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    TypeFamilies,
    TypeOperators,
    NoMonomorphismRestriction,
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
        Interval,
        Repl,
        Same,  
        HasPitch(..),
        HasPitch',
        getPitch,
        setPitch',
        mapPitch',
        pitch,
        pitches,
        highestPitch,
        lowestPitch,
        meanPitch,

        -- * Pitch transformer
        PitchT(..),

        -- * Pitch transformations
        -- ** Transposition
        up,
        down,
        invertAround,
        octavesUp,
        octavesDown,
  ) where

import Control.Monad (ap, mfilter, join, liftM, MonadPlus(..))
import Control.Applicative

#ifdef __HAS_LENS__
import Control.Lens
#endif __HAS_LENS__

import Data.String
import Data.Typeable
import Data.Traversable
import Data.Ratio
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Relative
import Data.AffineSpace.Point.Relative
import qualified Data.List as List

import Music.Time
import Music.Pitch.Literal

transformPitch :: Span -> Pitch a -> Pitch b
transformPitch = undefined

type Repl (f :: * -> * -> *) g a b = f (g b) a ~ b
type Same f g a = Repl f g a a

class (Transformable (Pitch a), SetPitch (Pitch a) a ~ a) => HasPitch a where

    -- | Get the pitch type.
    type Pitch    s     :: *

    -- | Update the pitch type.
    type SetPitch (b :: *) (s :: *) :: *

    getPitches :: a -> [Pitch a]
    setPitch   :: (Transformable (Pitch b), Repl SetPitch Pitch a b) => Pitch b -> a -> b
    mapPitch   :: (Transformable (Pitch b), Repl SetPitch Pitch a b) => (Pitch a -> Pitch b) -> a -> b
    setPitch x   = mapPitch (const x)
    -- mapPitch f x = setPitch (f $ head $ getPitches x) x

-- TODO move these out of class (so no one accidentally implements them wrongly)
    
    getPitch :: HasPitch a => a -> Pitch a
    getPitch = head . getPitches

    setPitch' :: HasPitch a => Pitch a -> a -> a
    setPitch' = setPitch

    mapPitch' :: HasPitch a => (Pitch a -> Pitch a) -> a -> a
    mapPitch' = mapPitch



-- |
-- A lens to the first pitch in the given value.
--
-- > pitch :: HasPitch a => Lens' a (Pitch a)
-- > pitch :: (b ~ SetPitch (Pitch b) a, HasPitch a, HasPitch b) => Lens a b (Pitch a) (Pitch b)
pitch :: (Functor f, HasPitch a, b ~ (SetPitch (Pitch b) a), Transformable (Pitch b)) => (Pitch a -> f (Pitch b)) -> a -> f b
pitch f x = fmap (`setPitch` x) $ f (getPitch x)

-- |
-- Traversal all pitches in the given value.
--
-- > pitches :: HasPitch a => Traversal' a (Pitch a)
-- > pitches :: (b ~ SetPitch (Pitch b) a, HasPitch a, HasPitch b) => Traversal a b (Pitch a) (Pitch b)
pitches :: (Applicative f, HasPitch a, b ~ (SetPitch (Pitch b) a), Transformable (Pitch b)) => (Pitch a -> f (Pitch b)) -> a -> f b
pitches f x = fmap ((flip setPitch x) . head) $ traverse f (getPitches x)


type Interval a = Diff (Pitch a)

type HasPitch' a = (
    HasPitch a, 
    VectorSpace (Interval a), Integer ~ Scalar (Interval a),
    AffineSpace (Pitch a)
    )

newtype PitchT p a = PitchT { getPitchT :: (p, a) }
    deriving (Eq, Ord, Show, Functor)

instance (Stretchable p, Delayable p) => HasPitch (PitchT p a) where
    type Pitch      (PitchT f a) = f
    type SetPitch g (PitchT f a) = PitchT g a 
    getPitches (PitchT (v,_))    = [v]
    mapPitch f (PitchT (v,x)) = PitchT (f v, x)

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

#define HAS_PITCH_PRIM(T)   \
instance HasPitch T where { \
    type Pitch T = T;       \
    type SetPitch b T = b;  \
    getPitches = return;    \
    mapPitch   = id; }

HAS_PITCH_PRIM(())
HAS_PITCH_PRIM(Double)
HAS_PITCH_PRIM(Float)
HAS_PITCH_PRIM(Int)
HAS_PITCH_PRIM(Integer)

instance Integral a => HasPitch (Ratio a) where
    type Pitch (Ratio a) = (Ratio a)
    type SetPitch b (Ratio a) = b
    getPitches = return
    mapPitch = id

instance HasPitch b => HasPitch (a, b) where
    type Pitch (a,b)      = Pitch b
    type SetPitch g (a,b) = (a, SetPitch g b)
    getPitches = getPitches . snd
    mapPitch f = fmap (mapPitch f)

instance HasPitch a => HasPitch [a] where
    type Pitch [a] = Pitch a
    type SetPitch g [a] = [SetPitch g a]
    getPitches [] = error "getPitch: Empty list"
    getPitches as = concatMap getPitches as
    mapPitch f    = fmap (mapPitch f)


    
-- |
-- Transpose up.
--
-- > Interval -> Score a -> Score a
--
up :: (HasPitch a, AffineSpace p, p ~ Pitch a) => Interval a -> a -> a
up a = mapPitch' (.+^ a)

-- |
-- Transpose down.
--
-- > Interval -> Score a -> Score a
--
down :: (HasPitch a, AffineSpace p, p ~ Pitch a) => Interval a -> a -> a
down a = mapPitch' (.-^ a)

-- |
-- Invert around the given pitch.
--
-- > Pitch -> Score a -> Score a
--
invertAround :: (AffineSpace (Pitch a), HasPitch a) => Pitch a -> a -> a
invertAround p = mapPitch' (reflectThrough p)

-- |
-- Transpose up by the given number of octaves.
--
-- > Integer -> Score a -> Score a
--
octavesUp       :: (HasPitch' a, IsInterval (Interval a)) => 
                Integer -> a -> a

-- |
-- Transpose down by the given number of octaves.
--
-- > Integer -> Score a -> Score a
--
octavesDown     :: (HasPitch' a, IsInterval (Interval a)) => 
                Integer -> a -> a

octavesUp a     = up (_P8^*a)
octavesDown a   = down (_P8^*a)


highestPitch = maximum . getPitches
lowestPitch = maximum . getPitches
meanPitch = mean . getPitches


mean :: Floating a => [a] -> a
mean x = fst $ foldl (\(m, n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x