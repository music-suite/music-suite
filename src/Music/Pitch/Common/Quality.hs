-- | Common interval quality.
module Music.Pitch.Common.Quality
  ( -- * Quality
    Quality (..),
    qualityTypes,
    isStandardQuality,
    isSimpleQuality,
    HasQuality (..),
    invertQuality,
    isPerfect,
    isMajor,
    isMinor,
    isAugmented,
    isDiminished,

    -- ** Quality type
    QualityType (..),
    expectedQualityType,
    isValidQualityNumber,

    -- ** Quality to alteration
    Direction (..),
    qualityToAlteration,
    qualityToDiff,
  )
where

import Music.Pitch.Common.Types

