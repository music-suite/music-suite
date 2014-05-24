
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides a representation of harmonics. 
--
-- /Warning/ This module is experimental.
--
-------------------------------------------------------------------------------------


module Music.Score.Harmonics (
        -- * Harmonics
        HasHarmonic(..),
        HarmonicT(..),
        harmonic,
        artificial,
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens            hiding (transform)
import           Data.Foldable
import           Data.Foldable
import           Data.Functor.Couple
import           Data.Ratio
import           Data.Semigroup
import           Data.Typeable
import           Data.Word

import           Music.Dynamics.Literal
import           Music.Pitch.Alterable
import           Music.Pitch.Augmentable
import           Music.Pitch.Literal
import           Music.Score.Part
import           Music.Score.Phrases
import           Music.Time


-- 0 for none, positive for natural, negative for artificial
class HasHarmonic a where
  setNatural :: Bool -> a -> a
  setHarmonic :: Int -> a -> a

-- (isNatural, overtone series index where 0 is fundamental)
newtype HarmonicT a = HarmonicT { getHarmonicT :: Couple (Any, Sum Int) a }
  deriving (
    Eq, Show, Ord, Functor, Foldable, Typeable, 
    Applicative, Monad, Comonad
    )

instance HasHarmonic a => HasHarmonic (b, a) where
  setNatural b = fmap (setNatural b)
  setHarmonic n = fmap (setHarmonic n)

instance HasHarmonic a => HasHarmonic (Couple b a) where
  setNatural b = fmap (setNatural b)
  setHarmonic n = fmap (setHarmonic n)

instance HasHarmonic a => HasHarmonic [a] where
  setNatural b = fmap (setNatural b)
  setHarmonic n = fmap (setHarmonic n)

instance HasHarmonic a => HasHarmonic (Score a) where
  setNatural b = fmap (setNatural b)
  setHarmonic n = fmap (setHarmonic n)


-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (HarmonicT a) where
  type Unwrapped (HarmonicT a) = Couple (Any, Sum Int) a
  _Wrapped' = iso getHarmonicT HarmonicT

instance Rewrapped (HarmonicT a) (HarmonicT b)

instance HasHarmonic (HarmonicT a) where
  setNatural  b = over (_Wrapped'._Wrapped') $ \((_,n),x)   -> ((Any b,n),x)
  setHarmonic n = over (_Wrapped'._Wrapped') $ \((nat,_),x) -> ((nat,Sum n),x)

-- Lifted instances
deriving instance Num a => Num (HarmonicT a)
deriving instance Fractional a => Fractional (HarmonicT a)
deriving instance Floating a => Floating (HarmonicT a)
deriving instance Enum a => Enum (HarmonicT a)
deriving instance Bounded a => Bounded (HarmonicT a)
deriving instance (Num a, Ord a, Real a) => Real (HarmonicT a)
deriving instance (Real a, Enum a, Integral a) => Integral (HarmonicT a)

-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
--
harmonic :: HasHarmonic a => Int -> a -> a
harmonic n = setNatural True . setHarmonic n
-- TODO verify this can actually be played

-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
--
artificial :: HasHarmonic a => a -> a
artificial =  setNatural False . setHarmonic 3









