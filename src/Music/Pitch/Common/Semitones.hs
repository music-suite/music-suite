
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund, Edward Lilley 2012–2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides semitone, octave and step representation of intervals.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Common.Semitones (
        -- * Types
        -- ** Octaves
        Octaves,
        -- HasOctaves(..),

        -- ** Steps
        Steps,
        -- HasSteps(..),

        -- ** Semitones
        Semitones,
        HasSemitones(..),
        semitone,
        tone,
        ditone,
        tritone,
        isSemitone,
        isTone,
        isTritone,

        -- * Enharmonic equivalence
        (=:=),
        (/:=),
  ) where

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
instance HasOctaves Octaves where { octaves = id }

-- |
-- Class of intervals that has a number of 'Octaves'.
--
class HasOctaves a where
  -- |
  -- Returns the number of octaves spanned by an interval.
  --
  -- The number of octaves is negative if and only if the interval is
  -- negative.
  --
  -- Examples:
  --
  -- > octaves (perfect unison)  =  0
  -- > octaves (d5 ^* 4)         =  2
  -- > octaves (-_P8)            =  -1
  --
  octaves :: a -> Octaves



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

{-
-- |
-- Class of intervals that has a number of 'Steps'.
--
class HasSteps a where
  -- |
  -- The number of steps is always in the range /0 ≤ x < 12/.
  --
  -- Examples:
  --
  -- > octaves (perfect unison)  =  0
  -- > octaves (d5 ^* 4)         =  2
  -- > octaves (-m7)             =  -1
  --
  steps :: a -> Steps
-}



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
instance HasSemitones Semitones where { semitones = id }

-- |
-- Class of intervals that can be converted to a number of 'Semitones'.
--
class HasSemitones a where

  -- |
  -- Returns the number of semitones spanned by an interval.
  --
  -- The number of semitones is negative if and only if the interval is
  -- negative.
  --
  -- >>> semitones (perfect unison)
  -- 0
  -- >>> semitones tritone
  -- 6
  -- >>> semitones d5
  -- 6
  -- >>> semitones (-_P8)
  -- -12
  --
  semitones :: a -> Semitones



semitone, tone, ditone, tritone :: Semitones

-- | Precisely one semitone.
semitone = 1

-- | Precisely one whole tone, or two semitones.
tone     = 2

-- | Precisely two whole tones, or four semitones.
ditone   = 4

-- | Precisely three whole tones, or six semitones.
tritone  = 6


isTone, isSemitone, isTritone :: HasSemitones a => a -> Bool

-- | Returns true iff the given interval spans one semitone.
isSemitone  = (== semitone) . abs . semitones

-- | Returns true iff the given interval spans one whole tone (two semitones).
isTone      = (== tone)     . abs . semitones

-- | Returns true iff the given interval spans three whole tones (six semitones).
isTritone   = (== tritone)  . abs . semitones



infix 4 =:=
infix 4 /:=

-- |
-- Enharmonic equivalence.
--
-- >>> asInterval _A2 == m3
-- False
-- >>> asInterval _A2 =:= m3
-- True
--
(=:=) :: HasSemitones a => a -> a -> Bool
a =:= b = semitones a == semitones b

-- |
-- Enharmonic non-equivalence.
--
-- >>> asInterval _A2 /= m3
-- True
-- >>> asInterval _A2 /:= m3
-- False
--
(/:=) :: HasSemitones a => a -> a -> Bool
a /:= b = semitones a /= semitones b


