
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TypeFamilies,
             NoMonomorphismRestriction, DeriveDataTypeable #-}

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
-- Provides standard intervals.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Common.Interval (
        -- * Quality
        Quality(..),    
        HasQuality(..),
        invertQuality,
        isPerfect,
        isMajor,
        isMinor,
        isAugmented,
        isDiminished,

        -- ** Number
        Number,
        HasNumber(..),   
        unison,
        prime,
        second,
        third,
        fourth,
        fifth,
        sixth,
        seventh,
        octave,
        ninth,
        tenth,
        twelfth, 
        duodecim,
        thirteenth,
        fourteenth,
        fifteenth,  
        
        -- ** Intervals
        Interval,

        -- *** Creating intervals
        interval,
        perfect,
        major,
        minor,
        augmented,
        diminished,
        doublyAugmented,
        doublyDiminished,

        -- *** Inspecting intervals
        isNegative,
        isPositive,
        isNonNegative,
        isPerfectUnison,
        isStep,
        isLeap,

        -- *** Simple and compound intervals
        isSimple,
        isCompound,
        separate,
        simple,

        -- *** Inversion
        invert,

        -- * Utility
        asInterval,
        intervalDiff,
        interval',
  ) where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.Typeable
import Control.Monad
import Control.Applicative
import qualified Data.List as List

import Music.Pitch.Absolute
import Music.Pitch.Augmentable
import Music.Pitch.Literal
import Music.Pitch.Common.Quality
import Music.Pitch.Common.Enharmonic
import Music.Pitch.Common.Number

-- |
-- An interval is the difference between two pitches, incuding negative
-- intervals.
--
-- Intervals and pitches can be added using '.+^'. To get the interval between
-- two pitches, use '.-.'.
--
-- > c .+^ minor third == eb
-- > f .-. c           == perfect fourth
--
-- Adding intervals preserves spelling. For example:
--
-- > m3 ^+^ _M3 = _P5
-- > d5 ^+^ _M6 = m10
--
-- The scalar type of 'Interval' is 'Integer', using '^*' to stack intervals of a certain
-- type on top of each other. For example @_P5 ^* 2@ is a stack of 2 perfect fifths, or a
-- major ninth. The 'Num' instance works as expected for '+', 'negate' and 'abs', and
-- (arbitrarily) uses minor seconds for multiplication. If you find yourself '*', or
-- 'signum' on intervals, consider switching to '*^' or 'normalized'.
--
-- Intervals are generally described in terms of 'Quality' and 'Number'. To
-- construct an interval, use the 'interval' constructor, the utility
-- constructors 'major', 'minor', 'augmented' and 'diminished', or the
-- interval literals:
--
-- > m5  == minor   fifth    == interval Minor   5
-- > _P4 == perfect fourth   == interval Perfect 5
-- > d5  == diminished fifth == diminish (perfect fifth)
--
newtype Interval = Interval { getInterval :: (
            Int,        -- octaves, may be negative
            Int,        -- diatonic semitone [0..6]
            Int         -- chromatic semitone [0..11]
    ) }
    deriving (Eq, Ord, Typeable)

instance Num Interval where
    (+)           = addInterval
    negate        = negateInterval
    abs a         = if isNegative a then negate a else a
    a * b         = fromIntegral (semitones a) `stackInterval` b
    signum a      = if isNegative a then (-m2) else (if isPositive a then m2 else _P1)
    fromInteger 0 = _P1
    fromInteger n = n `stackInterval` m2

instance Show Interval where
    show a | isNegative a = "-" ++ showQuality (quality a) ++ show (abs $ number a)
           | otherwise    =        showQuality (quality a) ++ show (abs $ number a)
           where
               showQuality Major            = "_M"
               showQuality Minor            = "m"
               showQuality Perfect          = "_P"
               showQuality (Augmented n)    = "_" ++ replicate' n 'A'
               showQuality (Diminished n)   = replicate' n 'd'

instance Semigroup Interval where
    (<>)    = addInterval

instance Monoid Interval where
    mempty  = perfect unison
    mappend = addInterval

instance AdditiveGroup Interval where
    zeroV   = perfect unison
    (^+^)   = addInterval
    negateV = negateInterval

instance VectorSpace Interval where
    type Scalar Interval = Integer
    (*^) = stackInterval

instance HasQuality Interval where
    quality (Interval (o, d, c))
        | o >= 0    =                 diffToQuality (isPerfectNumber d) (c - diatonicToChromatic d)
        | otherwise = invertQuality $ diffToQuality (isPerfectNumber d) (c - diatonicToChromatic d)

instance HasNumber Interval where
    number (Interval (o, d, c)) = fromIntegral $ inc $ o * 7 + d
        where
            inc a = if a >= 0 then succ a else pred a

instance Augmentable Interval where
    augment  (Interval (o, d, c)) = Interval (o, d, c + 1)
    diminish (Interval (o, d, c)) = Interval (o, d, c - 1)

instance HasOctaves Interval where
    octaves = fst . separate

instance HasSemitones Interval where
    semitones (Interval (o, d, c)) = fromIntegral $ o * 12 + c

instance HasSteps Interval where
    steps a = fromIntegral $ semitones a `mod` 12

instance IsInterval Interval where
    fromInterval (IntervalL (o,d,c)) = Interval (fromIntegral o, fromIntegral d, fromIntegral c)

-- |
-- This is just the identity function, but is useful to fix the type of 'Interval'.
--
asInterval :: Interval -> Interval
asInterval = id

-- |
-- Creates an interval from a quality and number.
--
-- Given 'Perfect' with an number not indicating a perfect consonant, 'interval' returns a
-- major interval instead. Given 'Major' or 'Minor' with a number indicating a perfect
-- consonance, 'interval' returns a perfect or diminished interval respectively.
--
interval :: Quality -> Number -> Interval
interval quality number = interval' (qualityToDiff (isPerfectNumber diatonic) quality) (fromIntegral number)
    where
        (_, diatonic) = (fromIntegral $ number - 1) `divMod` 7

interval' :: Int -> Int -> Interval
interval' diff number = Interval (octave, diatonic, diatonicToChromatic diatonic + diff)
    where
        (octave, diatonic) = (number - 1) `divMod` 7


-- | Creates a perfect interval.
--   If given an inperfect number, constructs a major interval.
perfect = interval Perfect

-- | Creates a major interval.
--   If given a perfect number, constructs a perfect interval.
major = interval Major

-- | Creates a minor interval.
--   If given a perfect number, constructs a diminished interval.
minor = interval Minor

-- | Creates an augmented interval.
augmented  = interval (Augmented 1)

-- | Creates a diminished interval.
diminished = interval (Diminished 1)

-- | Creates a doubly augmented interval.
doublyAugmented  = interval (Augmented 2)

-- | Creates a doubly diminished interval.
doublyDiminished = interval (Diminished 2)


invertDiatonic :: Num a => a -> a
invertDiatonic d  = 7  - d

invertChromatic :: Num a => a -> a
invertChromatic c = 12 - c

negateInterval :: Interval -> Interval
negateInterval (Interval (o, 0, 0))    = Interval (negate o, 0, 0)
negateInterval (Interval (oa, da, ca)) = Interval (negate (oa + 1), invertDiatonic da, invertChromatic ca)

addInterval :: Interval -> Interval -> Interval
addInterval (Interval (oa, da,ca)) (Interval (ob, db,cb))
    = Interval (oa + ob + carry, steps, chroma)
    where
        (carry, steps) = (da + db) `divMod` 7
        chroma         = trunc (ca + cb)
        trunc          = if carry > 0 then (`mod` 12) else id

stackInterval :: Integer -> Interval -> Interval
stackInterval n a | n >= 0    = mconcat $ replicate (fromIntegral n) a
                  | otherwise = negate $ stackInterval (negate n) a

intervalDiff :: Interval -> Int
intervalDiff (Interval (o, d, c)) = c - diatonicToChromatic d

-- |
-- Separate a compound interval into octaves and a simple interval.
--
-- > (perfect octave)^*x + y = z  iff  (x, y) = separate z
--
separate :: Interval -> (Octaves, Interval)
separate (Interval (o, d, c)) = (fromIntegral o, Interval (0, d, c))

-- |
-- Returns the simple part of an interval.
--
-- > (perfect octave)^*x + y = z  iff  y = simple z
--
simple :: Interval -> Interval
simple = snd . separate

-- |
-- Returns whether the given interval is simple.
--
-- A simple interval is a non-negative interval spanning less than one octave.
--
isSimple :: Interval -> Bool
isSimple x = octaves x == 0

-- |
-- Returns whether the given interval is compound.
--
-- A compound interval is either a negative interval, or a positive interval spanning
-- more than octave.
--
isCompound :: Interval -> Bool
isCompound x = octaves x /= 0

-- |
-- Returns whether the given interval is negative.
--
isNegative :: Interval -> Bool
isNegative x = octaves x < 0

-- |
-- Returns whether the given interval is positive.
--
isPositive :: Interval -> Bool
isPositive x = octaves x >= 0 && not (isPerfectUnison x)

-- |
-- Returns whether the given interval is non-negative.
--
-- That is, whether it is either positive or a perfect unison.
--
isNonNegative :: Interval -> Bool
isNonNegative x = octaves x >= 0

-- |
-- Returns whether the given interval a perfect unison.
--
isPerfectUnison :: Interval -> Bool
isPerfectUnison = (== perfect unison)

-- |
-- Returns whether the given interval is a step (a second or smaller).
--
-- Only the number portion is taken into account, so @_A2@ is considered
-- a step and @m3@ a leap, even though they have the same number of
-- semitones.
--
isStep :: Interval -> Bool
isStep x = isSimple (abs x) && number (abs x) <= 2

-- |
-- Returns whether the given interval is a leap (larger than a secon).
--
-- Only the number portion is taken into account, so @_A2@ is considered
-- a step and @m3@ a leap, even though they have the same number of
-- semitones.
--
isLeap :: Interval -> Bool
isLeap x = isCompound (abs x) || number (abs x) > 2


-- |
-- Intervallic inversion.
--
-- The inversion an interval is determined as follows:
--
-- * The number of a simple interval the difference of nine and the number of its inversion.
--
-- * The quality of a simple interval is the inversion of the quality of its inversion.
--
-- * The inversion of a compound interval is the inversion of its simple component.
--
invert :: Interval -> Interval
invert = simple . negate



isPerfectNumber :: Int -> Bool
isPerfectNumber 0 = True
isPerfectNumber 1 = False
isPerfectNumber 2 = False
isPerfectNumber 3 = True
isPerfectNumber 4 = True
isPerfectNumber 5 = False
isPerfectNumber 6 = False

diatonicToChromatic :: Int -> Int
diatonicToChromatic = go
    where
        go 0 = 0
        go 1 = 2
        go 2 = 4
        go 3 = 5
        go 4 = 7
        go 5 = 9
        go 6 = 11

{-# DEPRECATED intervalDiff "This should be hidden" #-}
{-# DEPRECATED interval'    "This should be hidden "#-}

replicate' n = replicate (fromIntegral n)
