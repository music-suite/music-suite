
-- | Diatonic pitch.
module Music.Pitch.Common.Diatonic (
    DiatonicSteps,
  ) where

newtype DiatonicSteps = DiatonicSteps { getDiatonicSteps :: Integer }
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)
