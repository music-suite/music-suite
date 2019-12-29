-- | Semitones and enharmonic equivalence.
module Music.Pitch.Common.Semitones
  ( -- * Types

    -- ** Octaves
    Octaves,

    -- ** Semitones
    Semitones,
    HasSemitones (..),
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
  )
where

import Music.Pitch.Common.Types

-- |
-- Class of intervals that has a number of 'Octaves'.
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
  octaves :: a -> Octaves

instance HasOctaves Octaves where octaves = id

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
-- Class of intervals that can be converted to a number of 'Semitones'.
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
  semitones :: a -> Semitones

instance HasSemitones ChromaticSteps where semitones = id

semitone, tone, ditone, tritone :: Semitones

-- | Precisely one semitone.
semitone = 1

-- | Precisely one whole tone, or two semitones.
tone = 2

-- | Precisely two whole tones, or four semitones.
ditone = 4

-- | Precisely three whole tones, or six semitones.
tritone = 6

isTone, isSemitone, isTritone :: HasSemitones a => a -> Bool

-- | Returns true iff the given interval spans one semitone.
isSemitone = (== semitone) . abs . semitones

-- | Returns true iff the given interval spans one whole tone (two semitones).
isTone = (== tone) . abs . semitones

-- | Returns true iff the given interval spans three whole tones (six semitones).
isTritone = (== tritone) . abs . semitones

infix 4 =:=

infix 4 /:=

-- |
-- Enharmonic equivalence.
--
-- >>> asInterval _A2 == m3
-- False
-- >>> asInterval _A2 =:= m3
-- True
(=:=) :: HasSemitones a => a -> a -> Bool
a =:= b = semitones a == semitones b

-- |
-- Enharmonic non-equivalence.
--
-- >>> asInterval _A2 /= m3
-- True
-- >>> asInterval _A2 /:= m3
-- False
(/:=) :: HasSemitones a => a -> a -> Bool
a /:= b = semitones a /= semitones b
