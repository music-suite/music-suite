
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides interval numbers.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Relative.Number (
        -- ** Number
        Number,   
        unison,
        prime,
        second,
        third,
        fourth,
        fifth,
        sixth,
        seventh,
        octave,
        -- ninth,
        -- tenth,
        -- twelfth,
        -- thirteenth,
        -- fourteenth,
        -- duodecim,
  ) where

-- |
-- The number portion of an interval (i.e. second, third, etc).
--
-- Note that the interval number is always one step larger than number of steps spanned by
-- the interval (i.e. a third spans two diatonic steps). Thus 'number' does not distribute
-- over addition:
--
-- > number (a + b) = number a + number b - 1
--
newtype Number = Number { getNumber :: Integer }
    deriving (Eq, Ord, Num, Enum, Real, Integral)
    
instance Show Number where { show = show . getNumber }

unison  :: Number
prime   :: Number
second  :: Number
third   :: Number
fourth  :: Number
fifth   :: Number
sixth   :: Number
seventh :: Number
octave  :: Number
unison  = 1
prime   = 1
second  = 2
third   = 3
fourth  = 4
fifth   = 5
sixth   = 6
seventh = 7
octave  = 8

