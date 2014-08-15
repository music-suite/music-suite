
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- Absolute pitch representation.
-- 
-- The canonical pitch representation is frequency in Hertz (Hz). For conversion, see
-- 'HasFrequency'.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Absolute (
        Hertz(..),
        FreqRatio(..),
        -- Octaves,
        Cents,
        Fifths,
        HasFrequency(..),
        -- octaves,
        fifths,
        cents,
  ) where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Data.VectorSpace
import Data.AdditiveGroup
import Data.AffineSpace
import Control.Monad
import Control.Applicative

import Music.Pitch.Literal

-- | 
-- Absolute frequency in Hertz.    
-- 
newtype Hertz = Hertz { getHertz :: Double }
    deriving (Read, Show, Eq, Enum, Num, Ord, Fractional, Floating, Real, RealFrac)

-- | 
-- A ratio between two different (Hertz) frequencies.
-- 
newtype FreqRatio = FreqRatio { getFreqRatio :: Double }
    deriving (Read, Show, Eq, Enum, Num, Ord, Fractional, Floating, Real, RealFrac)

-- | 
-- Number of pure octaves.
--
-- Octaves are a logarithmic representation of frequency such that
--
-- > f * (2/1) = frequency (octaves f + 1)    
-- 
newtype Octaves = Octaves { getOctaves :: Hertz }
    deriving (Read, Show, Eq, Enum, Num, Ord, Fractional, Floating, Real, RealFrac)

-- | 
-- Number of pure octaves.
--
-- Cents are a logarithmic representation of frequency such that
--
-- > f * (2/1) = frequency (cents f + 1200)    
-- 
newtype Cents = Cents { getCents :: Hertz }
    deriving (Read, Show, Eq, Enum, Num, Ord, Fractional, Floating, Real, RealFrac)

-- | 
-- Number of pure fifths.
--
-- Fifths are a logarithmic representation of frequency.
--
-- > f * (3/2) = frequency (fifths f + 1)    
-- 
newtype Fifths = Fifths { getFifths :: Hertz }
    deriving (Read, Show, Eq, Enum, Num, Ord, Fractional, Floating, Real, RealFrac)


instance Semigroup Hertz    where (<>) = (*)
instance Semigroup Octaves  where (<>) = (+)
instance Semigroup Fifths   where (<>) = (+)
instance Semigroup Cents    where (<>) = (+)

instance Monoid Hertz       where { mempty  = 1 ; mappend = (*) }
instance Monoid Octaves     where { mempty  = 0 ; mappend = (+) }
instance Monoid Fifths      where { mempty  = 0 ; mappend = (+) }
instance Monoid Cents       where { mempty  = 0 ; mappend = (+) }

instance AdditiveGroup FreqRatio where
    zeroV   = 1
    (^+^)   = (*)
    negateV f = 1 / f

instance VectorSpace FreqRatio where
    type Scalar FreqRatio = Double
    (*^) x f = FreqRatio ((getFreqRatio f) ** x)

instance AffineSpace Hertz where
  type Diff Hertz = FreqRatio
  (.-.) f1 f2 = FreqRatio $ (getHertz f1) / (getHertz f2)
  (.+^) p f = Hertz $ (getHertz p) * (getFreqRatio f)

class HasFrequency a where
    frequency :: a -> Hertz

instance HasFrequency Hertz where
    frequency = id

instance HasFrequency Octaves where
    frequency (Octaves f)  = (2/1) ** f

instance HasFrequency Fifths where
    frequency (Fifths f)   =  (3/2) ** f

instance HasFrequency Cents where
    frequency (Cents f)    =  (2/1) ** (f / 1200)

octaves :: HasFrequency a => a -> Octaves
octaves a = Octaves $ logBase (2/1) (frequency a)

fifths :: HasFrequency a => a -> Fifths
fifths a = Fifths $ logBase (3/2) (frequency a)

cents :: HasFrequency a => a -> Cents
cents a = Cents $ logBase (2/1) (frequency a) * 1200

-- 
-- For convenience. TODO problematic for these reasons:
-- 
--  * Confusing as _P8+m2 is not the same as _P8<>m2
--  * Does not work with HasPitch (up, down...) etc as we do not have an affine/vector space pair
--
-- Can we fix this with newtype wrappers?
-- 
instance IsInterval Hertz where
  fromInterval (IntervalL (o,d,c)) = (2**fromIntegral o) * (r !! fromIntegral c)
    where
      r = [
        1/1,
        (8*2)/15, 9/8,      (2*3)/5, 5/4,     (2*2)/3,
        10/7,
        3/2,      (4*2)/5,  5/3,     (2*8)/9, 15/8
        ]


