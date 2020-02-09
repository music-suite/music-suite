{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-missing-signatures
  #-}
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

import Data.Functor.Couple
import Data.Semigroup
import Music.Pitch.Common.Types
import Data.VectorSpace

newtype IntervalL = IntervalL (Integer, Integer, Integer)

-- (octaves, diatonic steps, chromatic steps)

class IsInterval a where
  fromInterval :: Interval -> a

instance IsInterval Interval where
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

instance IsInterval Integer where
  fromInterval (Interval (ChromaticSteps c, _d)) = c

fromIntervalL :: IsInterval a => IntervalL -> a
fromIntervalL = fromInterval . go
  where
    go :: IntervalL -> Interval
    go (IntervalL (o, d, c)) = (basis_P8 ^* o) ^+^ (basis_A1 ^* c) ^+^ (basis_d2 ^* d)

d1 = fromIntervalL $ IntervalL (0, 0, -1)

_P1 = fromIntervalL $ IntervalL (0, 0, 0)

_A1 = fromIntervalL $ IntervalL (0, 0, 1)

d2 = fromIntervalL $ IntervalL (0, 1, 0)

m2 = fromIntervalL $ IntervalL (0, 1, 1)

_M2 = fromIntervalL $ IntervalL (0, 1, 2)

_A2 = fromIntervalL $ IntervalL (0, 1, 3)

d3 = fromIntervalL $ IntervalL (0, 2, 2)

m3 = fromIntervalL $ IntervalL (0, 2, 3)

_M3 = fromIntervalL $ IntervalL (0, 2, 4)

_A3 = fromIntervalL $ IntervalL (0, 2, 5)

d4 = fromIntervalL $ IntervalL (0, 3, 4)

_P4 = fromIntervalL $ IntervalL (0, 3, 5)

_A4 = fromIntervalL $ IntervalL (0, 3, 6)

d5 = fromIntervalL $ IntervalL (0, 4, 6)

_P5 = fromIntervalL $ IntervalL (0, 4, 7)

_A5 = fromIntervalL $ IntervalL (0, 4, 8)

d6 = fromIntervalL $ IntervalL (0, 5, 7)

m6 = fromIntervalL $ IntervalL (0, 5, 8)

_M6 = fromIntervalL $ IntervalL (0, 5, 9)

_A6 = fromIntervalL $ IntervalL (0, 5, 10)

d7 = fromIntervalL $ IntervalL (0, 6, 9)

m7 = fromIntervalL $ IntervalL (0, 6, 10)

_M7 = fromIntervalL $ IntervalL (0, 6, 11)

_A7 = fromIntervalL $ IntervalL (0, 6, 12)

d8 = fromIntervalL $ IntervalL (1, 0, -1)

_P8 = fromIntervalL $ IntervalL (1, 0, 0)

_A8 = fromIntervalL $ IntervalL (1, 0, 1)

d9 = fromIntervalL $ IntervalL (1, 1, 0)

m9 = fromIntervalL $ IntervalL (1, 1, 1)

_M9 = fromIntervalL $ IntervalL (1, 1, 2)

_A9 = fromIntervalL $ IntervalL (1, 1, 3)

d10 = fromIntervalL $ IntervalL (1, 2, 2)

m10 = fromIntervalL $ IntervalL (1, 2, 3)

_M10 = fromIntervalL $ IntervalL (1, 2, 4)

_A10 = fromIntervalL $ IntervalL (1, 2, 5)

d11 = fromIntervalL $ IntervalL (1, 3, 4)

_P11 = fromIntervalL $ IntervalL (1, 3, 5)

_A11 = fromIntervalL $ IntervalL (1, 3, 6)

d12 = fromIntervalL $ IntervalL (1, 4, 6)

_P12 = fromIntervalL $ IntervalL (1, 4, 7)

_A12 = fromIntervalL $ IntervalL (1, 4, 8)

d13 = fromIntervalL $ IntervalL (1, 5, 7)

m13 = fromIntervalL $ IntervalL (1, 5, 8)

_M13 = fromIntervalL $ IntervalL (1, 5, 9)

_A13 = fromIntervalL $ IntervalL (1, 5, 10)

d14 = fromIntervalL $ IntervalL (1, 6, 9)

m14 = fromIntervalL $ IntervalL (1, 6, 10)

_M14 = fromIntervalL $ IntervalL (1, 6, 11)

_A14 = fromIntervalL $ IntervalL (1, 6, 12)

d15 = fromIntervalL $ IntervalL (2, 0, -1)

_P15 = fromIntervalL $ IntervalL (2, 0, 0)

_A15 = fromIntervalL $ IntervalL (2, 0, 1)

