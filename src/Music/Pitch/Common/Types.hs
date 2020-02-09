-- | Common (Western classical) pitches, intervals and related types.
module Music.Pitch.Common.Types
  ( -- * Even octaves and steps
    Octaves,
    DiatonicSteps,
    ChromaticSteps,
    Semitones,

    -- * Intervals
    Number,
    Quality (..),
    QualityType (..),
    IntervalBasis (..),
    Interval,

    -- * Pitch
    Name (..),
    Accidental,
    Pitch,
  )
where
import Music.Pitch.Common.Internal


