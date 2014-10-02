
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TypeFamilies,
             NoMonomorphismRestriction, DeriveDataTypeable #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund, Edward Lilley 2012–2014
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
        Interval(..),

        -- *** Creating intervals
        mkInterval,
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
        octaves,

        -- *** Inversion
        invert,

        -- * Utility
        asInterval,

        -- * Basis values
        IntervalBasis(..),
       
        -- ** Converting basis
        convertBasis,
        convertBasisFloat,
        intervalDiv,
        
        -- ** Basis values (TODO cleanup)
        basis_P1,
        basis_A1,
        basis_d2,
        basis_P8,
        basis_P5,
        
        -- ** Utility
        intervalDiff,
        mkInterval',
  ) where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Data.VectorSpace
-- import Data.AffineSpace
import Data.Basis
import Data.Typeable
import Control.Monad
import Control.Applicative
import qualified Data.List as List

import Music.Pitch.Absolute
import Music.Pitch.Augmentable
import Music.Pitch.Literal
import Music.Pitch.Common.Semitones

-- |
-- Interval quality is either perfect, major, minor, augmented, and
-- diminished. This representation allows for an arbitrary number of
-- augmentations or diminutions, so /augmented/ is represented by @Augmented
-- 1@, /doubly augmented/ by @Augmented 2@ and so on.
--
-- The quality of a compound interval is the quality of the simple interval on
-- which it is based.
--
-- Note that (Augmented 0) and (Diminished 0) are superfluous identity
-- values, use Perfect instead. Augmented and Diminished must also
-- take only positive arguments.

data Quality
    = Major
    | Minor
    | Perfect
    | Augmented Int
    | Diminished Int
    deriving (Eq, Ord, Show)

instance HasQuality Quality where
    quality = id


-- | Augmentable Quality instance
-- 
-- This Augmentable instance exists solely for use of the getQuality
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

-- |
-- The number portion of an interval (i.e. second, third, etc).
--
-- Note that the interval number is always one step larger than number of steps spanned by
-- the interval (i.e. a third spans two diatonic steps). Thus 'number' does not distribute
-- over addition:
--
-- > number (a + b) = number a + number b - 1
--
newtype Number = Number { getNumber :: Int }
    deriving (Eq, Ord, Num, Enum, Real, Integral)
    
instance Show Number where { show = show . getNumber }
instance HasNumber Number where number = id


-- | A synonym for @1@.
unison      :: Number
unison      = 1

-- | A synonym for @1@.
prime       :: Number
prime       = 1

-- | A synonym for @2@.
second      :: Number
second      = 2

-- | A synonym for @3@.
third       :: Number
third       = 3

-- | A synonym for @4@.
fourth      :: Number
fourth      = 4     

-- | A synonym for @5@.
fifth       :: Number
fifth       = 5

-- | A synonym for @6@.
sixth       :: Number
sixth       = 6

-- | A synonym for @7@.
seventh     :: Number
seventh     = 7

-- | A synonym for @8@.
octave      :: Number
octave      = 8

-- | A synonym for @9@.
ninth       :: Number
ninth       = 9

-- | A synonym for @10@.
tenth       :: Number
tenth       = 10

-- | A synonym for @11@.
eleventh    :: Number
eleventh    = 11

-- | A synonym for @12@.
twelfth     :: Number
twelfth     = 12

-- | A synonym for @12@.
duodecim    :: Number
duodecim    = 12

-- | A synonym for @13@.
thirteenth  :: Number
thirteenth  = 13

-- | A synonym for @14@.
fourteenth  :: Number
fourteenth  = 14

-- | A synonym for @15@.
fifteenth   :: Number
fifteenth   = 15

class HasNumber a where
    -- |
    -- Returns the number portion of an interval.
    --
    -- The interval number is negative if and only if the interval is negative.
    --
    -- See also 'quality', 'octaves' and 'semitones'.
    --
    number :: a -> Number

                                                   
                                                   
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
-- The scalar type of 'Interval' is 'Int', using '^*' to stack intervals of a certain
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
            Int,        -- number of A1, i.e. chromatic steps
            Int        -- number of d2, i.e. diatonic steps
    ) }
    deriving (Eq, Typeable)

-- | Given that intervals are two-dimensional vectors, it's not
-- possible to order them in general. The best we can do is to rely on
-- the 'number' of the interval, i.e. thirds are bigger than seconds
-- etc.
instance Ord Interval where
  i > j = (snd . getInterval) i > (snd . getInterval) j
  i < j = (snd . getInterval) i < (snd . getInterval) j
  i >= j = (snd . getInterval) i >= (snd . getInterval) j
  i <= j = (snd . getInterval) i <= (snd . getInterval) j

-- | Avoid using '(*)', or 'signum' on intervals.
instance Num Interval where
    (+)           = addInterval
    negate        = negateInterval
    abs a         = if isNegative a then negate a else a
    (*)           = error "Music.Pitch.Common.Interval: no overloading for (*)"
    signum        = error "Music.Pitch.Common.Interval: no overloading for signum"
    fromInteger   = error "Music.Pitch.Common.Interval: no overloading for fromInteger"
        
instance Show Interval where
  show a
    | isNegative a = "-" ++ showQuality (extractQuality a) ++ show (abs $ extractNumber a)
    | otherwise    =        showQuality (extractQuality a) ++ show (abs $ extractNumber a)
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

-- TODO move
data IntervalBasis = Chromatic | Diatonic
  deriving (Eq, Ord, Show, Enum)

instance HasBasis Interval where
  type Basis Interval = IntervalBasis
  basisValue Chromatic = basis_A1
  basisValue Diatonic  = basis_d2
  decompose  (Interval (c,d)) = [(Chromatic, fromIntegral c), (Diatonic, fromIntegral d)]
  decompose' (Interval (c,d)) Chromatic = fromIntegral c
  decompose' (Interval (c,d)) Diatonic  = fromIntegral d

instance HasQuality Interval where
  quality i = extractQuality i

instance HasNumber Interval where
  number i = extractNumber i

instance Augmentable Interval where
  augment i = i ^+^ basis_A1
  diminish i = i ^-^ basis_A1

instance HasSemitones Interval where
  semitones (Interval (a, d)) = fromIntegral a -- assuming "semitone" == A1

instance IsInterval Interval where
  fromInterval (IntervalL (o,d,c)) = (basis_P8^*o) ^+^ (basis_A1^*c) ^+^ (basis_d2^*d)

negateInterval :: Interval -> Interval
negateInterval (Interval (a, d)) = Interval (-a, -d)

addInterval :: Interval -> Interval -> Interval
addInterval (Interval (a1, d1)) (Interval (a2, d2)) = Interval (a1 + a2, d1 + d2)

stackInterval :: Integer -> Interval -> Interval
stackInterval n a | n >= 0    = mconcat $ replicate (fromIntegral n) a
                  | otherwise = negate $ stackInterval (negate n) a

intervalDiff :: Interval -> Int
intervalDiff (Interval (c, d)) = c - diatonicToChromatic d

-- |
-- Creates an interval from a quality and number.
--
-- Given 'Perfect' with an number not indicating a perfect consonant, 'interval' returns a
-- major interval instead. Given 'Major' or 'Minor' with a number indicating a perfect
-- consonance, 'interval' returns a perfect or diminished interval respectively.
--
mkInterval' 
  :: Int        -- ^ Difference in chromatic steps (?).
  -> Int        -- ^ Number of diatonic steps (NOT interval number).
  -> Interval
mkInterval' diff diatonic = Interval (diatonicToChromatic diatonic + diff, diatonic)

basis_P1 = Interval (0, 0)
basis_A1 = Interval (1, 0)
basis_d2 = Interval (0, 1)
basis_P5 = Interval (7, 4)
basis_P8 = Interval (12, 7)


mkInterval :: Quality -> Number -> Interval

 -- our identity:
mkInterval Perfect 1 = basis_P1
 -- and our two basis vectors:
mkInterval (Augmented 1) 1 = basis_A1
mkInterval (Diminished 1) 2 = basis_d2

mkInterval Minor 2 = basis_d2 ^+^ basis_A1
mkInterval Major 2 = (mkInterval Minor 2) ^+^ basis_A1
mkInterval (Augmented 1) 2 = (mkInterval Major 2) ^+^ basis_A1

mkInterval (Diminished 1) 3 = (mkInterval Minor 3) ^-^ basis_A1
mkInterval Minor 3 = (mkInterval Major 2) ^+^ (mkInterval Minor 2)
mkInterval Major 3 = (mkInterval Major 2) ^+^ (mkInterval Major 2)
mkInterval (Augmented 1) 3 =  (mkInterval Major 3) ^+^ basis_A1

mkInterval (Diminished 1) 4 = (mkInterval Perfect 4) ^-^ basis_A1
mkInterval Perfect 4 = (mkInterval Major 3) ^+^ (mkInterval Minor 2)
mkInterval (Augmented 1) 4 =  (mkInterval Perfect 4) ^+^ basis_A1

mkInterval (Diminished 1) 5 = (mkInterval Perfect 5) ^-^ basis_A1
mkInterval Perfect 5 = (mkInterval Perfect 4) ^+^ (mkInterval Major 2)
mkInterval (Augmented 1) 5 =  (mkInterval Perfect 5) ^+^ basis_A1

mkInterval (Diminished 1) 6 = (mkInterval Minor 6) ^-^ basis_A1
mkInterval Minor 6 = (mkInterval Perfect 5) ^+^ (mkInterval Minor 2)
mkInterval Major 6 = (mkInterval Perfect 5) ^+^ (mkInterval Major 2)
mkInterval (Augmented 1) 6 =  (mkInterval Major 6) ^+^ basis_A1

mkInterval (Diminished 1) 7 = (mkInterval Minor 7) ^-^ basis_A1
mkInterval Minor 7 = (mkInterval Major 6) ^+^ (mkInterval Minor 2)
mkInterval Major 7 = (mkInterval Major 6) ^+^ (mkInterval Major 2)
mkInterval (Augmented 1) 7 =  (mkInterval Major 7) ^+^ basis_A1

mkInterval Minor 1 = error "invalid interval"
mkInterval Major 1 = error "invalid interval"
mkInterval Perfect 2 = error "invalid interval"
mkInterval Perfect 3 = error "invalid interval"
mkInterval Minor 4 = error "invalid interval"
mkInterval Major 4 = error "invalid interval"
mkInterval Minor 5 = error "invalid interval"
mkInterval Major 5 = error "invalid interval"
mkInterval Perfect 6 = error "invalid interval"
mkInterval Perfect 7 = error "invalid interval"

mkInterval (Diminished 0) n = error "(Diminished 0) is not a valid Quality"
mkInterval (Augmented 0) n = error  "(Augmented 0) is not a valid Quality"

mkInterval (Diminished q) n = (mkInterval (Diminished (q - 1)) n) ^-^ basis_A1
mkInterval (Augmented q) n = (mkInterval (Diminished (q - 1)) n) ^+^ basis_A1

mkInterval q (Number n) = if n > 0
                          then (mkInterval q (Number (n - 7))) ^+^ basis_P8
                          else (mkInterval q (Number (n + 7))) ^-^ basis_P8


-- |
-- Extracting the 'number' from an interval vector.
--
-- Note that (a, d) is a representation of the interval (a * A1) + (d
-- * d2), so the 'number' part of the interval must be stored entirely
-- in the d * d2 part (adding a unison, perfect or otherwise, can
-- never increase the number of the interval)
-- 
extractNumber :: Interval -> Number
extractNumber (Interval (a, d))
  | d >= 0    = Number (d + 1)
  | otherwise = Number (d - 1)


-- |
-- Extracting the 'quality' from an interval vector.
--
-- This is much more finicky, as the A1 and d2 intervals interact in a
-- complex way to produce the perfect/major/minor/etc. intervals that
-- we are used to reading.

extractQuality :: Interval -> Quality
extractQuality (Interval (a, d))
  | (a < 0) && (d == 0) = diminish (extractQuality (Interval ((a + 1), d)))
  | (a, d) == (0, 0) = Perfect
  | (a > 0) && (d == 0) = augment (extractQuality (Interval ((a - 1), d)))
  | (a < 1) && (d == 1) = diminish (extractQuality (Interval ((a + 1), d)))
  | (a, d) == (1, 1) = Minor
  | (a, d) == (2, 1) = Major
  | (a > 2) && (d == 1) = augment (extractQuality (Interval ((a - 1), d)))
  | (a < 3) && (d == 2) = diminish (extractQuality (Interval ((a + 1), d)))
  | (a, d) == (3, 2) = Minor
  | (a, d) == (4, 2) = Major
  | (a > 4) && (d == 2) = augment (extractQuality (Interval ((a - 1), d)))
  | (a < 5) && (d == 3) = diminish (extractQuality (Interval ((a + 1), d)))
  | (a, d) == (5, 3) = Perfect
  | (a > 5) && (d == 3) = augment (extractQuality (Interval ((a - 1), d)))
  | (a < 7) && (d == 4) = diminish (extractQuality (Interval ((a + 1), d)))
  | (a, d) == (7, 4) = Perfect
  | (a > 7) && (d == 4) = augment (extractQuality (Interval ((a - 1), d)))
  | (a < 8) && (d == 5) = diminish (extractQuality (Interval ((a + 1), d)))
  | (a, d) == (8, 5) = Minor
  | (a, d) == (9, 5) = Major
  | (a > 9) && (d == 5) = augment (extractQuality (Interval ((a - 1), d)))
  | (a < 10) && (d == 6) = diminish (extractQuality (Interval ((a + 1), d)))
  | (a, d) == (10, 6) = Minor
  | (a, d) == (11, 6) = Major
  | (a > 11) && (d == 6) = augment (extractQuality (Interval ((a - 1), d)))
  | (a < 12) && (d == 7) = diminish (extractQuality (Interval ((a + 1), d)))
  | (a, d) == (12, 7) = Perfect
  | (a > 12) && (d == 7) = augment (extractQuality (Interval ((a - 1), d)))
-- note: these last two cases *have* to be this way round, otherwise
-- infinite loop occurs.
  | (a > 12) || (d > 7) = extractQuality (Interval ((a - 12), (d - 7)))
  | (a < 0) || (d < 0) = extractQuality (Interval ((-a), (-d)))


-- | Creates a perfect interval.
--   If given an inperfect number, constructs a major interval.
perfect :: Number -> Interval
perfect = mkInterval Perfect

-- | Creates a major interval.
--   If given a perfect number, constructs a perfect interval.
major :: Number -> Interval
major = mkInterval Major

-- | Creates a minor interval.
--   If given a perfect number, constructs a diminished interval.
minor :: Number -> Interval
minor = mkInterval Minor

-- | Creates an augmented interval.
augmented :: Number -> Interval
augmented  = mkInterval (Augmented 1)

-- | Creates a diminished interval.
diminished :: Number -> Interval
diminished = mkInterval (Diminished 1)

-- | Creates a doubly augmented interval.
doublyAugmented :: Number -> Interval
doublyAugmented  = mkInterval (Augmented 2)

-- | Creates a doubly diminished interval.
doublyDiminished :: Number -> Interval
doublyDiminished = mkInterval (Diminished 2)

{-

Prelude Music.Prelude> separate (2*^_P8+m3)
(2,m3)
Prelude Music.Prelude> 
Prelude Music.Prelude> separate (3*^_P8+m3)
(3,m3)
Prelude Music.Prelude> 
Prelude Music.Prelude> separate (0*^_P8+m3)
(0,m3)
Prelude Music.Prelude> separate ((-1)*^_P8+m3)

-}
-- |
-- Separate a compound interval into octaves and a simple interval.
--
-- > (perfect octave)^*x + y = z  iff  (x, y) = separate z
--
separate :: Interval -> (Octaves, Interval)
separate i = (fromIntegral o, i ^-^ (fromIntegral o *^ basis_P8))
  where
    o = octaves i

-- |
-- Returns the non-simple part of an interval.
--
-- > _P8^*octaves x ^+^ simple x = x
--
octaves :: Interval -> Octaves
octaves i 
  | isNegative i && not (isOctaveMultiple i) = negate (octaves' i) - 1
  | isNegative i && isOctaveMultiple i       = negate (octaves' i)
  | otherwise                                = octaves' i

isOctaveMultiple (Interval (_,d)) = d `mod` 7 == 0

octaves' i = fromIntegral $ intervalDiv i basis_P8

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
-- one octave or more.
--
isCompound :: Interval -> Bool
isCompound x = octaves x /= 0

-- |
-- Returns whether the given interval is negative.
--
isNegative :: Interval -> Bool
isNegative (Interval (a, d)) = d < 0

-- |
-- Returns whether the given interval is positive.
--
isPositive :: Interval -> Bool
isPositive x@(Interval (a, d)) = d >= 0 && not (isPerfectUnison x)

-- |
-- Returns whether the given interval is non-negative. This implies that it is either positive or a perfect unison.
--
isNonNegative :: Interval -> Bool
isNonNegative (Interval (a, d)) = d >= 0

-- |
-- Returns whether the given interval a perfect unison.
--
isPerfectUnison :: Interval -> Bool
isPerfectUnison = (== perfect unison)

-- |
-- Returns whether the given interval is a step (a second or smaller).
--
-- Only diatonic 'number' is taken into account, so @_A2@ is considered
-- a step and @m3@ a leap, even though they have the same number of
-- semitones.
--
isStep :: Interval -> Bool
isStep (Interval (a, d)) = (abs d) <= 2

-- |
-- Returns whether the given interval is a leap (larger than a second).
--
-- Only the diatonic 'number' is taken into account, so @_A2@ is considered
-- a step and @m3@ a leap, even though they have the same number of
-- semitones.
--
isLeap :: Interval -> Bool
isLeap (Interval (a, d)) = (abs d) > 2


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

-- |
-- This is just the identity function, but is useful to fix the type of 'Interval'.
--
asInterval :: Interval -> Interval
asInterval = id

{-
isPerfectNumber :: Int -> Bool
isPerfectNumber 0 = True
isPerfectNumber 1 = False
isPerfectNumber 2 = False
isPerfectNumber 3 = True
isPerfectNumber 4 = True
isPerfectNumber 5 = False
isPerfectNumber 6 = False
-}

-- TODO more generic pattern here
diatonicToChromatic :: Int -> Int
diatonicToChromatic d = (octaves*12) + go restDia
    where
        -- restDia is always in [0..6]
        (octaves, restDia) = d `divMod` 7
        go = ([0,2,4,5,7,9,11] !!)

-- {-# DEPRECATED intervalDiff "This should be hidden" #-}
-- {-# DEPRECATED mkInterval'  "This should be hidden "#-}

replicate' n = replicate (fromIntegral n)




-- | Integer div of intervals: i / di = x, where x is an integer
intervalDiv :: Interval -> Interval -> Int
intervalDiv (Interval (a, d)) (Interval (1, 0)) = a
intervalDiv (Interval (a, d)) (Interval (0, 1)) = d
intervalDiv i di
  | (i > basis_P1) = intervalDivPos i di
  | (i < basis_P1) = intervalDivNeg i di
  | otherwise = 0 :: Int
  where 
    intervalDivPos i di
      | (i < basis_P1) = undefined
      | (i ^-^ di) < basis_P1 = 0
      | otherwise = 1 + (intervalDiv (i ^-^ di) di)
    intervalDivNeg i di
      | (i > basis_P1) = undefined
      | (i ^+^ di) > basis_P1 = 0
      | otherwise = 1 + (intervalDiv (i ^+^ di) di)

-- | Represent an interval i in a new basis (j, k).
-- 
-- We want x,y where i = x*j + y*k
--
-- e.g., convertBasis basis_d2 _P5 basis_P8 == Just (-12,7), as expected.

convertBasis
  :: Interval 
  -> Interval 
  -> Interval 
  -> Maybe (Int, Int)
convertBasis i j k
  | (p == 0) = Nothing
  | not $ p `divides` r = Nothing
  | not $ p `divides` q = Nothing
  | otherwise = Just (r `div` p, q `div` p)
  where Interval (m, n) = i
        Interval (a, b) = j
        Interval (c, d) = k
        p = (a*d - b*c)
        q = (a*n - b*m)
        r = (d*m - c*n)


-- | Same as above, but don't worry if new interval has non-integer
-- coefficients -- useful when getting a value to use as a frequency
-- ratio in a tuning system.
convertBasisFloat :: (Fractional t, Eq t)
  => Interval 
  -> Interval 
  -> Interval 
  -> Maybe (t, t)
convertBasisFloat i j k
  | (p == 0) = Nothing
  | otherwise = Just (r / p, q / p)
  where Interval (m, n) = i
        Interval (a, b) = j
        Interval (c, d) = k
        p = fromIntegral $ (a*d - b*c)
        q = fromIntegral $ (a*n - b*m)
        r = fromIntegral $ (d*m - c*n)


divides :: Integral a => a -> a -> Bool
x `divides` y = (y `rem` x) == 0

