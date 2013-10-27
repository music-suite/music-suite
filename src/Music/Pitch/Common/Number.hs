
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

module Music.Pitch.Common.Number (
        -- ** Number
        Number,
        HasNumber(..),   
        unison,
        prime,
        second,
        third,
        fourth,
        fifth,
        sixth,
        seventh,
        octave,
        ninth,
        tenth,
        twelfth, 
        duodecim,
        thirteenth,
        fourteenth,
        fifteenth,
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
instance HasNumber Number where number = id

unison      :: Number
prime       :: Number
second      :: Number
third       :: Number
fourth      :: Number
fifth       :: Number
sixth       :: Number
seventh     :: Number
octave      :: Number
ninth       :: Number
tenth       :: Number
eleventh    :: Number
twelfth     :: Number
duodecim    :: Number
thirteenth  :: Number
fourteenth  :: Number
fifteenth   :: Number

-- | A synonym for @1@.
unison      = 1

-- | A synonym for @2@.
prime       = 1

-- | A synonym for @3@.
second      = 2

-- | A synonym for @4@.
third       = 3

-- | A synonym for @5@.
fourth      = 4     

-- | A synonym for @6@.
fifth       = 5

-- | A synonym for @7@.
sixth       = 6

-- | A synonym for @8@.
seventh     = 7

-- | A synonym for @9@.
octave      = 8

-- | A synonym for @10@.
ninth       = 9

-- | A synonym for @11@.
tenth       = 10

-- | A synonym for @12@.
eleventh    = 11

-- | A synonym for @13@.
twelfth     = 12

-- | A synonym for @14@.
duodecim    = 12

-- | A synonym for @15@.
thirteenth  = 13

-- | A synonym for @16@.
fourteenth  = 14

-- | A synonym for @17@.
fifteenth   = 15

class HasNumber a where
    -- |
    -- Returns the number portion of an interval.
    --
    -- The interval number is negative if and only if the interval is negative.
    --
    -- See also 'quality', 'octaves' and 'semitones'.
    --
    number :: a -> Number

