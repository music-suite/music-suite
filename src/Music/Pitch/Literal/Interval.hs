{-# LANGUAGE NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------

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
-- Provides overloaded interval literals.
module Music.Pitch.Literal.Interval
  ( -- * IsInterval class
    IsInterval (..),
    IntervalL (..),

    -- * Literal values

    -- ** Simple intervals
    d1,
    _P1,
    _A1,
    d2,
    m2,
    _M2,
    _A2,
    d3,
    m3,
    _M3,
    _A3,
    d4,
    _P4,
    _A4,
    d5,
    _P5,
    _A5,
    d6,
    m6,
    _M6,
    _A6,
    d7,
    m7,
    _M7,
    _A7,

    -- ** One-octave compounds
    d8,
    _P8,
    _A8,
    d9,
    m9,
    _M9,
    _A9,
    d10,
    m10,
    _M10,
    _A10,
    d11,
    _P11,
    _A11,
    d12,
    _P12,
    _A12,
    d13,
    m13,
    _M13,
    _A13,
    d14,
    m14,
    _M14,
    _A14,

    -- ** Two-octave compounds
    d15,
    _P15,
    _A15,
  )
where

import Control.Applicative
import Data.Fixed
import Data.Functor.Couple
import Data.Ratio
import Data.Semigroup
import Data.Word
import Music.Pitch.Common.Types

newtype IntervalL = IntervalL (Integer, Integer, Integer)

-- (octaves, diatonic steps, chromatic steps)

class IsInterval a where
  fromInterval :: IntervalL -> a

instance IsInterval IntervalL where
  fromInterval = id

instance IsInterval a => IsInterval (Maybe a) where
  fromInterval = pure . fromInterval

instance IsInterval a => IsInterval (First a) where
  fromInterval = pure . fromInterval

instance IsInterval a => IsInterval (Last a) where
  fromInterval = pure . fromInterval

instance IsInterval a => IsInterval [a] where
  fromInterval = pure . fromInterval

instance (Monoid b, IsInterval a) => IsInterval (b, a) where
  fromInterval = pure . fromInterval

deriving instance (Monoid b, IsInterval a) => IsInterval (Couple b a)

instance IsInterval Int where
  fromInterval x = fromIntegral (fromInterval x :: Integer)

instance IsInterval Word where
  fromInterval x = fromIntegral (fromInterval x :: Integer)

{-
instance IsInterval Float where
    fromInterval x = realToFrac (fromInterval x :: Double)

instance HasResolution a => IsInterval (Fixed a) where
    fromInterval x = realToFrac (fromInterval x :: Double)

instance Integral a => IsInterval (Ratio a) where
    fromInterval x = realToFrac (fromInterval x :: Double)

instance IsInterval Double where
    fromInterval = fromIntegral . asInteger . fromInterval
-}

instance IsInterval Integer where
  fromInterval (IntervalL (o, d, c)) = o * 12 + c

d1 = fromInterval $ IntervalL (0, 0, -1)

_P1 = fromInterval $ IntervalL (0, 0, 0)

_A1 = fromInterval $ IntervalL (0, 0, 1)

d2 = fromInterval $ IntervalL (0, 1, 0)

m2 = fromInterval $ IntervalL (0, 1, 1)

_M2 = fromInterval $ IntervalL (0, 1, 2)

_A2 = fromInterval $ IntervalL (0, 1, 3)

d3 = fromInterval $ IntervalL (0, 2, 2)

m3 = fromInterval $ IntervalL (0, 2, 3)

_M3 = fromInterval $ IntervalL (0, 2, 4)

_A3 = fromInterval $ IntervalL (0, 2, 5)

d4 = fromInterval $ IntervalL (0, 3, 4)

_P4 = fromInterval $ IntervalL (0, 3, 5)

_A4 = fromInterval $ IntervalL (0, 3, 6)

d5 = fromInterval $ IntervalL (0, 4, 6)

_P5 = fromInterval $ IntervalL (0, 4, 7)

_A5 = fromInterval $ IntervalL (0, 4, 8)

d6 = fromInterval $ IntervalL (0, 5, 7)

m6 = fromInterval $ IntervalL (0, 5, 8)

_M6 = fromInterval $ IntervalL (0, 5, 9)

_A6 = fromInterval $ IntervalL (0, 5, 10)

d7 = fromInterval $ IntervalL (0, 6, 9)

m7 = fromInterval $ IntervalL (0, 6, 10)

_M7 = fromInterval $ IntervalL (0, 6, 11)

_A7 = fromInterval $ IntervalL (0, 6, 12)

d8 = fromInterval $ IntervalL (1, 0, -1)

_P8 = fromInterval $ IntervalL (1, 0, 0)

_A8 = fromInterval $ IntervalL (1, 0, 1)

d9 = fromInterval $ IntervalL (1, 1, 0)

m9 = fromInterval $ IntervalL (1, 1, 1)

_M9 = fromInterval $ IntervalL (1, 1, 2)

_A9 = fromInterval $ IntervalL (1, 1, 3)

d10 = fromInterval $ IntervalL (1, 2, 2)

m10 = fromInterval $ IntervalL (1, 2, 3)

_M10 = fromInterval $ IntervalL (1, 2, 4)

_A10 = fromInterval $ IntervalL (1, 2, 5)

d11 = fromInterval $ IntervalL (1, 3, 4)

_P11 = fromInterval $ IntervalL (1, 3, 5)

_A11 = fromInterval $ IntervalL (1, 3, 6)

d12 = fromInterval $ IntervalL (1, 4, 6)

_P12 = fromInterval $ IntervalL (1, 4, 7)

_A12 = fromInterval $ IntervalL (1, 4, 8)

d13 = fromInterval $ IntervalL (1, 5, 7)

m13 = fromInterval $ IntervalL (1, 5, 8)

_M13 = fromInterval $ IntervalL (1, 5, 9)

_A13 = fromInterval $ IntervalL (1, 5, 10)

d14 = fromInterval $ IntervalL (1, 6, 9)

m14 = fromInterval $ IntervalL (1, 6, 10)

_M14 = fromInterval $ IntervalL (1, 6, 11)

_A14 = fromInterval $ IntervalL (1, 6, 12)

d15 = fromInterval $ IntervalL (2, 0, -1)

_P15 = fromInterval $ IntervalL (2, 0, 0)

_A15 = fromInterval $ IntervalL (2, 0, 1)

asInteger :: Integer -> Integer
asInteger = id
