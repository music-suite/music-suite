
{-# LANGUAGE FlexibleInstances #-}

-- | Diatonic pitch.
module Music.Pitch.Common.Types
(
        Octaves,
        Steps,
        DiatonicSteps,
        ChromaticSteps,
        Semitones,
        Number,
        Quality(..),
        QualityType(..),
        Accidental,
        Name(..),
        IntervalBasis(..),        
) where

import Music.Pitch.Literal
import Music.Pitch.Alterable
import Music.Pitch.Augmentable

newtype ChromaticSteps = ChromaticSteps { getChromaticSteps :: Integer }
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

newtype DiatonicSteps = DiatonicSteps { getDiatonicSteps :: Integer }
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

-- |
-- An interval represented as a number of octaves, including negative
-- intervals.
--
-- > octaves a = semitones a `div` 12
-- > steps   a = semitones a `mod` 12
--
newtype Octaves = Octaves { getOctaves :: Integer }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show       Octaves where { show = show . getOctaves }

-- |
-- An interval represented as a number of steps in the range /0 ≤ x < 12/.
--
-- > octaves a = semitones a `div` 12
-- > steps   a = semitones a `mod` 12
--
newtype Steps = Steps { getSteps :: Integer }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show Steps where { show = show . getSteps }


-- instance HasSteps Steps where { steps = id }
-- |
-- An interval represented as a number of semitones, including negative
-- intervals, as well as intervals larger than one octave. This representation
-- does not take spelling into account, so for example a major third and a
-- diminished fourth can not be distinguished.
--
-- Intervals that name a number of semitones (i.e. 'semitone', 'tritone') does
-- not have an unequivocal spelling. To convert these to an interval, a
-- 'Spelling' must be provided:
--
-- >>> spell usingSharps tritone
-- _A4
--
-- >>> spell usingFlats  tritone
-- d5
--
newtype Semitones = Semitones { getSemitones :: Integer }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show         Semitones where { show = show . getSemitones }


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

-- |
-- Interval quality is either perfect, major, minor, augmented, and
-- diminished. This representation allows for an arbitrary number of
-- augmentations or diminutions, so /augmented/ is represented by @Augmented
-- 1@, /doubly augmented/ by @Augmented 2@ and so on.
--
-- The quality of a compound interval is the quality of the simple interval on
-- which it is based.
--
data Quality
  = Major
  | Minor
  | Perfect
  -- TODO we really want to use Positive here, but that requires a
  -- rewrite of extractQuality below
  | Augmented Integer
  | Diminished Integer
  deriving (Eq, Ord, Show)


data QualityType = PerfectType | MajorMinorType
  deriving (Eq, Ord, Read, Show)



-- |
-- An accidental is either flat, natural or sharp.
--
-- This representation allows for an arbitrary number of flats or sharps rather than just
-- single and double.
--
-- The 'Num' and 'Enum' instances treat 'Accidental' as the number of altered semitones,
-- i.e. a double flat is @-2@, natural @0@ and so on.
--
newtype Accidental = Accidental { getAccidental :: Integer }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show Accidental where
  show n | n == 0    = "natural"
         | n == 1    = "sharp"
         | n == (-1) = "flat"
         | n == 2    = "doubleSharp"
         | n == (-2) = "doubleFlat"
         | n > 0     = "sharp * " ++ show (getAccidental n)
         | n < 0     = "flat * " ++ show (negate $ getAccidental n)

instance Alterable Accidental where
  sharpen = succ
  flatten = pred

-- |
-- Magic instance that allow us to write @c sharp@ instead of @sharpen c@.
--
instance (IsPitch a, Alterable a) => IsPitch (Accidental -> a) where
  fromPitch l 1     = sharpen (fromPitch l)
  fromPitch l (-1)  = flatten (fromPitch l)
-- Requires FlexibleInstances

-- |
-- A pitch name.
--
data Name = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum)


data IntervalBasis = Chromatic | Diatonic
  deriving (Eq, Ord, Show, Enum)

