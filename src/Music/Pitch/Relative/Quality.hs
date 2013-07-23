
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Music.Pitch.Relative.Quality where

class Augmentable a where
    -- | Increase the size of this interval by one.
    augment :: a -> a
    -- | Decrease the size of this interval by one.
    diminish :: a -> a

-- |
-- Interval quality is either perfect, major, minor, augmented, and
-- diminished. This representation allows for an arbitrary number of
-- augmentation or diminishions, so /augmented/ is represented by @Augmented 1@,
-- /doubly augmented/ by @Augmented 2@ and so on.
-- 
-- The quality of a compound interval is the quality of the simple interval on
-- which it is based.
--
data Quality 
    = Major
    | Minor
    | Perfect
    | Augmented Integer
    | Diminished Integer

deriving instance Eq Quality
deriving instance Ord Quality
instance Show Quality where
    show Major            = "_M"
    show Minor            = "m"
    show Perfect          = "_P"
    show (Augmented n)    = "_" ++ replicate' n 'A'
    show (Diminished n)   = replicate' n 'd'
instance HasQuality Quality where
    quality = id

-- TODO this instance should not be used
-- instance Augmentable Quality where
--     augment = go
--         where
--             go (Diminished 0)   = Augmented n    -- not unique!
--             go (Diminished n)   = Augmented n
--             go Minor            = Major
--             go Major            = Augmented 1
--             go Perfect          = Augmented 1
--             go (Augmented n)    = Diminished (n)

class HasQuality a where
    quality :: a -> Quality

invertQuality :: Quality -> Quality
invertQuality = go
    where
        go Major            = Minor
        go Minor            = Major
        go Perfect          = Perfect
        go (Augmented n)    = Diminished n
        go (Diminished n)   = Augmented n


-- | Returns whether the given quality is perfect.
isPerfect :: HasQuality a => a -> Bool
isPerfect a = case quality a of { Perfect -> True ; _ -> False }

-- | Returns whether the given quality is major.
isMajor :: HasQuality a => a -> Bool
isMajor a = case quality a of { Major -> True ; _ -> False }

-- | Returns whether the given quality is minor.
isMinor :: HasQuality a => a -> Bool
isMinor a = case quality a of { Minor -> True ; _ -> False }

-- | Returns whether the given quality is /augmented/ (including double augmented etc).
isAugmented :: HasQuality a => a -> Bool
isAugmented a = case quality a of { Augmented _ -> True ; _ -> False }

-- | Returns whether the given quality is /diminished/ (including double diminished etc).
isDiminished :: HasQuality a => a -> Bool
isDiminished a = case quality a of { Diminished _ -> True ; _ -> False }

-- | Convert an offset to a quality.
--
--   This is different for perfect and imperfect interals:
--
--      Imperfect   Perfect
--      ===         === 
-- -3   dd          ddd
-- -2   d           dd
-- -1   m           d
--  0   M           P
--  1   a           a
--  2   aa          aa
--
diffToQuality :: Bool -> Int -> Quality
diffToQuality = go
    where
        go True  0   = Perfect
        go True  n   = if n > 0 then Augmented (fromIntegral n) else Diminished (fromIntegral $ negate n)
        go False 0    = Major
        go False (-1) = Minor
        go False n    = if n > 0 then Augmented (fromIntegral n) else Diminished (fromIntegral $ negate $ n + 1)

qualityToDiff :: Bool -> Quality -> Int
qualityToDiff perfect = go
    where
        go (Diminished n)   = fromIntegral $ negate $ if perfect then n else n + 1
        go Minor            = fromIntegral $ -1
        go Perfect          = fromIntegral $ 0
        go Major            = fromIntegral $ 0
        go (Augmented n)    = fromIntegral $ n

replicate' n = replicate (fromIntegral n)

