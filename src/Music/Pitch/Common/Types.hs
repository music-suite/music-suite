
-- | Diatonic pitch.
module Music.Pitch.Common.Types
(
        Octaves,
        Steps,
        DiatonicSteps,
        ChromaticSteps,
        Semitones,
        Number,        
) where

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
