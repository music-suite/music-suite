
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides interval quality.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Common.Quality (

        -- * Quality
        Quality(..),    
        HasQuality(..),
        invertQuality,
        isPerfect,
        isMajor,
        isMinor,
        isAugmented,
        isDiminished,
        
        diffToQuality,
        qualityToDiff,
  ) where

import Music.Pitch.Augmentable

-- |
-- Interval quality is either perfect, major, minor, augmented, and
-- diminished. This representation allows for an arbitrary number of
-- augmentation or diminishions, so /augmented/ is represented by @Augmented
-- 1@, /doubly augmented/ by @Augmented 2@ and so on.
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
    deriving (Eq, Ord, Show)

instance HasQuality Quality where
    quality = id

-- There is no instance for (Augmentable Quality) as we can not distinguish 
-- between m/M and P in cases like (augment $ Diminished 1) or 
-- (diminish $ Augmented 1).

-- instance Augmentable Quality where
--     augment = go
--         where
--             go (Diminished 0)   = error "Diminished 0"
--             go (Diminished 1)   = Minor -- Or perfect
--             go (Diminished n)   = Diminished (n - 1)
-- 
--             go Minor            = Major
--             go Perfect          = Augmented 1
--             go Major            = Augmented 1
-- 
--             go (Augmented 0)    = error "Augmented 0"
--             go (Augmented n)    = Augmented (n + 1)
--     diminish = go
--         where
--             go (Diminished 0)   = error "Diminished 0"
--             go (Diminished n)   = Diminished (n + 1)
-- 
--             go Major            = Minor
--             go Perfect          = Diminished 1
--             go Minor            = Diminished 1
-- 
--             go (Augmented 0)    = error "Augmented 0"
--             go (Augmented 1)    = Major -- or Perfect (?)
--             go (Augmented n)    = Augmented (n - 1)

class HasQuality a where
    quality :: a -> Quality

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

-- Convert an offset to a quality.
--
-- This is different for perfect and imperfect interals:
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
        go (Diminished n)   = fromIntegral $ negate $ if perfect then n else n + 1
        go Minor            = fromIntegral $ -1
        go Perfect          = fromIntegral $ 0
        go Major            = fromIntegral $ 0
        go (Augmented n)    = fromIntegral $ n


{-# DEPRECATED diffToQuality "This should be hidden "#-}
{-# DEPRECATED qualityToDiff "This should be hidden "#-}

replicate' n = replicate (fromIntegral n)
