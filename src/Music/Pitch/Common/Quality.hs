
-- | Common interval quality.
module Music.Pitch.Common.Quality
(
        -- * Quality
        Quality(..),
        HasQuality(..),
        invertQuality,
        isPerfect,
        isMajor,
        isMinor,
        isAugmented,
        isDiminished,

        -- * Quality type
        QualityType(..),
        expectedQualityType,
        qualityTypes,
        qualityToDiff
) where

import           Music.Pitch.Augmentable
import           Music.Pitch.Common.Number
import           Music.Pitch.Common.Chromatic

-- | Types of value that has an interval quality (mainly 'Interval' and 'Quality' itself).
class HasQuality a where
  quality :: a -> Quality

-- |
-- Returns whether the given quality is perfect.
--
isPerfect :: HasQuality a => a -> Bool
isPerfect a = case quality a of { Perfect -> True ; _ -> False }

-- |
-- Returns whether the given quality is major.
--
isMajor :: HasQuality a => a -> Bool
isMajor a = case quality a of { Major -> True ; _ -> False }

-- |
-- Returns whether the given quality is minor.
--
isMinor :: HasQuality a => a -> Bool
isMinor a = case quality a of { Minor -> True ; _ -> False }

-- |
-- Returns whether the given quality is /augmented/ (including double augmented etc).
--
isAugmented :: HasQuality a => a -> Bool
isAugmented a = case quality a of { Augmented _ -> True ; _ -> False }

-- |
-- Returns whether the given quality is /diminished/ (including double diminished etc).
--
isDiminished :: HasQuality a => a -> Bool
isDiminished a = case quality a of { Diminished _ -> True ; _ -> False }

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

instance HasQuality Quality where
  quality = id


-- | Augmentable Quality instance
--
-- This Augmentable instance exists solely for use of the extractQuality
-- function, which ensures that there is never any ambiguity around
-- diminished/augmented intervals turning into major/minor/perfect
-- intervals.

instance Augmentable Quality where
  augment Major = Augmented 1
  augment Minor = Major
  augment Perfect = Augmented 1
  augment (Augmented n) = Augmented (n + 1)
  augment (Diminished n) = Diminished (n - 1)
  diminish Major = Minor
  diminish Minor = Diminished 1
  diminish Perfect = Diminished 1
  diminish (Augmented n) = Augmented (n - 1)
  diminish (Diminished n) = Diminished (n + 1)

-- |
-- Invert a quality.
--
-- Perfect is unaffected, major becomes minor and vice versa, augmented
-- becomes diminished and vice versa.
--
invertQuality :: Quality -> Quality
invertQuality = go
  where
    go Major            = Minor
    go Minor            = Major
    go Perfect          = Perfect
    go (Augmented n)    = Diminished n
    go (Diminished n)   = Augmented n


data QualityType = PerfectType | MajorMinorType
  deriving (Eq, Ord, Read, Show)

expectedQualityType :: HasNumber a => a -> QualityType
expectedQualityType x = if ((abs (number x) - 1) `mod` 7) + 1 `elem` [1,4,5]
  then PerfectType else MajorMinorType

qualityTypes :: Quality -> [QualityType]
qualityTypes Perfect = [PerfectType]
qualityTypes Major   = [MajorMinorType]
qualityTypes Minor   = [MajorMinorType]
qualityTypes _       = [PerfectType, MajorMinorType]

-- FIXME problem that this treats major as neutral, while this only holds for positive intervals
qualityToDiff :: Bool -> QualityType -> Quality -> ChromaticSteps
qualityToDiff positive qt q = fromIntegral $ go positive qt q
  where
    go True MajorMinorType (Augmented n)  = 0 + n
    go True MajorMinorType Major          = 0
    go True MajorMinorType Minor          = (-1)
    go True MajorMinorType (Diminished n) = -(1 + n)

    go False MajorMinorType (Augmented n)  = -(1 + n)
    go False MajorMinorType Major          = -1
    go False MajorMinorType Minor          = 0
    go False MajorMinorType (Diminished n) = 0 + n
    
    go _ PerfectType (Augmented n)  = 0 + n
    go _ PerfectType Perfect        = 0
    go _ PerfectType (Diminished n) = 0 - n
    
    go _ qt q = error $ "qualityToDiff: Unknown interval expression (" ++ show qt ++ ", " ++ show q ++ ")"



