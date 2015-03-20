
-- | Chromatic pitch.
module Music.Pitch.Common.Chromatic
(
        ChromaticSteps,
) where

newtype ChromaticSteps = ChromaticSteps { getChromaticSteps :: Integer }
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

