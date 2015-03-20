
-- | Number component of intervals.
module Music.Pitch.Common.Number
(
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
        eleventh,
        twelfth,
        thirteenth,
        fourteenth,
        fifteenth,
        diatonicSteps,
) where

import Control.Lens
import Music.Pitch.Common.Diatonic

-- |
-- The number portion of an interval (i.e. second, third, etc).
--
-- Note that the interval number is always one step larger than number of steps spanned by
-- the interval (i.e. a third spans two diatonic steps). Thus 'number' does not distribute
-- over addition:
--
-- > number (a + b) = number a + number b - 1
--
newtype Number = Number { getNumber :: Int }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show Number where { show = show . getNumber }
instance HasNumber Number where number = id

-- | A synonym for @1@.
unison      :: Number
unison      = 1

-- | A synonym for @1@.
prime       :: Number
prime       = 1

-- | A synonym for @2@.
second      :: Number
second      = 2

-- | A synonym for @3@.
third       :: Number
third       = 3

-- | A synonym for @4@.
fourth      :: Number
fourth      = 4

-- | A synonym for @5@.
fifth       :: Number
fifth       = 5

-- | A synonym for @6@.
sixth       :: Number
sixth       = 6

-- | A synonym for @7@.
seventh     :: Number
seventh     = 7

-- | A synonym for @8@.
octave      :: Number
octave      = 8

-- | A synonym for @9@.
ninth       :: Number
ninth       = 9

-- | A synonym for @10@.
tenth       :: Number
tenth       = 10

-- | A synonym for @11@.
eleventh    :: Number
eleventh    = 11

-- | A synonym for @12@.
twelfth     :: Number
twelfth     = 12

-- | A synonym for @12@.
duodecim    :: Number
duodecim    = 12

-- | A synonym for @13@.
thirteenth  :: Number
thirteenth  = 13

-- | A synonym for @14@.
fourteenth  :: Number
fourteenth  = 14

-- | A synonym for @15@.
fifteenth   :: Number
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


-- TODO rename numberDiatonicSteps
diatonicSteps :: Iso' Number DiatonicSteps
diatonicSteps = iso n2d d2n
  where
    n2d n | n > 0  = fromIntegral (n - 1)
    n2d n | n == 0 = error "diatonicSteps: Invalid number 0"
    n2d n | n < 0  = fromIntegral (n + 1)

    d2n n | n >= 0 = fromIntegral (n + 1)
    d2n n | n <  0 = fromIntegral (n - 1)

