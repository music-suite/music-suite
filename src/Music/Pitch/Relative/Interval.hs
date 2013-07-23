
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TypeFamilies #-}

module Music.Pitch.Relative.Interval (
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
        number,
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
        -- ** Spelling
        Spelling,
        spell,
        sharps,
        flats,

        -- * Literals (TODO move)
        d1, _P1, _A1,
        d2, m2, _M2, _A2,
        d3, m3, _M3, _A3,
        d4, _P4, _A4,
        d5, _P5, _A5,
        d6, m6, _M6, _A6,
        d7, m7, _M7, _A7,
        d8, _P8, _A8,

        -- TODO
        intervalDiff,
        interval',
        octave,
  ) where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad
import Control.Applicative
import Music.Pitch.Absolute hiding (Octaves(..), octaves)
import Music.Pitch.Literal
import qualified Data.List as List

import Music.Pitch.Relative.Quality
import Music.Pitch.Relative.Semitones
import Music.Pitch.Relative.Number

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
-- and 'abs', and arbitrarily uses octaves for multiplication. If you find
-- yourself '*', or 'signum' on intervals, consider switching to '^*' or
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

deriving instance Eq Interval
deriving instance Ord Interval
instance Num Interval where
    (+)           = addInterval
    negate        = negateInterval
    abs a         = if isNegative a then negate a else a
    a * b         = fromIntegral (semitones a `div` 12) `stackInterval` b
    signum a      = if isNegative a then (-_P8) else _P8
    fromInteger 0 = _P1
    fromInteger _ = undefined

instance Show Interval where
    show a | isNegative a = "-" ++ show (quality a) ++ show (abs $ number a)
           | otherwise    =        show (quality a) ++ show (abs $ number a)

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

instance Augmentable Interval where
    augment  (Interval (o, d, c)) = Interval (o, d, c + 1)
    diminish (Interval (o, d, c)) = Interval (o, d, c - 1)

instance HasOctaves Interval where
    octaves = fst . separate

instance HasSemitones Interval where
    semitones (Interval (o, d, c)) = fromIntegral $ o * 12 + c

instance HasSteps Interval where
    steps a = fromIntegral $ semitones a `mod` 12

intervalDiff :: Interval -> Int
intervalDiff (Interval (o, d, c)) = c - diatonicToChromatic d

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
perfect    = interval Perfect
-- | Creates a major interval.
--   If given a perfect number, constructs a perfect interval.
major      = interval Major
-- | Creates a minor interval.
--   If given a perfect number, constructs a diminished interval.
minor      = interval Minor
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
negateInterval (Interval (o, 0, 0))   = Interval (negate o, 0, 0)
negateInterval (Interval (oa, da,ca)) = Interval (negate (oa + 1), invertDiatonic da, invertChromatic ca)

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
-- Returns the number portion of an interval.
--
-- The interval number is negative if and only if the interval is negative.
--
-- See also 'quality', 'octaves' and 'semitones'.
--
number :: Interval -> Number
number (Interval (o, d, c)) = fromIntegral $ inc $ o * 7 + d
    where
        inc a = if a >= 0 then succ a else pred a



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



type Spelling = Semitones -> Number

spell :: HasSemitones a => Spelling -> a -> Interval
spell z = (\s -> Interval (fromIntegral $ s `div` 12, fromIntegral $ z s, fromIntegral s)) .  semitones

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

sharps :: Semitones -> Number
sharps = go
    where
        go 0  = 0
        go 1  = 0
        go 2  = 1
        go 3  = 1
        go 4  = 2
        go 5  = 3
        go 6  = 3
        go 7  = 4
        go 8  = 4
        go 9  = 5
        go 10 = 5
        go 11 = 6

flats :: Semitones -> Number
flats = go
    where
        go 0  = 0
        go 1  = 1
        go 2  = 1
        go 3  = 2
        go 4  = 2
        go 5  = 3
        go 6  = 4
        go 7  = 4
        go 8  = 5
        go 9  = 5
        go 10 = 6
        go 11 = 6




_ = 1 ;                  d1 = Interval (0,0,-1) ; _P1 = Interval (0,0,0)  ; _A1 = Interval (0,0,1)
d2 = Interval (0,1,0)  ; m2 = Interval (0,1,1)  ; _M2 = Interval (0,1,2)  ; _A2 = Interval (0,1,3)
d3 = Interval (0,2,2)  ; m3 = Interval (0,2,3)  ; _M3 = Interval (0,2,4)  ; _A3 = Interval (0,2,5)
_ = 1 ;                  d4 = Interval (0,3,4)  ; _P4 = Interval (0,3,5)  ; _A4 = Interval (0,3,6)
_ = 1 ;                  d5 = Interval (0,4,6)  ; _P5 = Interval (0,4,7)  ; _A5 = Interval (0,4,8)
d6 = Interval (0,5,7)  ; m6 = Interval (0,5,8)  ; _M6 = Interval (0,5,9)  ; _A6 = Interval (0,5,10)
d7 = Interval (0,6,9)  ; m7 = Interval (0,6,10) ; _M7 = Interval (0,6,11) ; _A7 = Interval (0,6,12)
_ = 1 ;                  d8 = Interval (1,0,-1) ; _P8 = Interval (1,0,0)  ; _A8 = Interval (1,0,1)

d9  = d2  + _P8 ; m9  = m2  + _P8 ; _M9  = _M2 + _P8 ; _A9  = _A2 + _P8
d10 = d3  + _P8 ; m10 = m3  + _P8 ; _M10 = _M3 + _P8 ; _A10 = _A3 + _P8



