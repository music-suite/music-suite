
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
        -- isPerfectUnison,
        isPositive,
        isNegative,

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
import Music.Pitch.Common.Semitones
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
-- The scalar type of intervals are 'Integer', using '^*' to stack intervals
-- of a certain type on top of each other. For example @_P5 ^* 2@ is a stack
-- of 2 perfect fifths. The 'Num' instance works as expected for '+', 'negate'
-- and 'abs', and arbitrarily uses minor seconds for multiplication. If you
-- find yourself '*', or 'signum' on intervals, consider switching to '*^' or
-- 'normalized'.
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
-- If given 'Perfect' with an imperfect number (such as 3 or 7) a major interval is
-- returned. If given 'Major' or 'Minor' with a perfect number (such as 5), constructs
-- a perfect or diminished interval respectively.
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
-- A simple interval is an positive interval spanning less than one octave.
--
isSimple :: Interval -> Bool
isSimple = (== 0) . octaves

-- |
-- Returns whether the given interval is compound.
--
-- A compound interval is either a negative interval, or a positive interval spanning
-- more than octave.
--
isCompound :: Interval -> Bool
isCompound = (/= 0) . octaves

isPerfectUnison :: Interval -> Bool
isPerfectUnison a = a == perfect unison

-- |
-- Returns whether the given interval is positive.
--
isPositive :: Interval -> Bool
isPositive (Interval (oa, _, _)) = oa > 0

-- |
-- Returns whether the given interval is negative.
--
isNegative :: Interval -> Bool
isNegative (Interval (oa, _, _)) = oa < 0


-- |
-- Intervallic inversion.
--
-- The inversion of a simple interval is determined by the following rules:
--
-- * The interval number and the number of its inversion always add up to nine
--   (i.e. 4 + 5 = 9).
--
-- * The inversion of a major interval is a minor interval, and vice versa;
--   the inversion of a perfect interval is also perfect; the inversion of an
--   augmented interval is a diminished interval, and vice versa; the
--   inversion of a doubly augmented interval is a doubly diminished interval,
--   and vice versa.
--
-- The inversion of any compound interval is always the same as the inversion
-- of the simple interval from which it is compounded, i.e.:
--
-- > invert = simple . negate
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
