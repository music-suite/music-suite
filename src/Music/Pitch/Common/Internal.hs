{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports
  -fno-warn-missing-signatures
  #-}

{-# OPTIONS_HADDOCK hide #-}

module Music.Pitch.Common.Internal where

import Control.Applicative
import Control.Lens hiding (simple)
import Control.Monad
import Data.AdditiveGroup
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.AffineSpace.Point (relative)
import Data.Basis
import Data.Either
import Data.Fixed (Fixed (..), HasResolution (..))
import Data.Functor.Couple
import Data.Maybe
import Data.Monoid (Ap (..))
import Data.Ratio
import Data.Semigroup
import Data.Typeable
import Data.VectorSpace
import Music.Pitch.Alterable
import Music.Pitch.Augmentable
import Music.Time.Transform (Transformable (..))
import Numeric.Positive
import qualified Data.Aeson
import qualified Data.Char as Char
import qualified Data.List as List

-- |
-- Number of chromatic steps.
-- May be negative, indicating a downward interval.
newtype ChromaticSteps = ChromaticSteps {getChromaticSteps :: Integer}
  deriving (Eq, Ord, Enum, Num, Real, Integral)

instance Show ChromaticSteps where
  show = show . getChromaticSteps

instance Show DiatonicSteps where
  show = show . getDiatonicSteps

-- |
-- Number of diatonic steps.
-- May be negative, indicating a downward interval.
newtype DiatonicSteps = DiatonicSteps {getDiatonicSteps :: Integer}
  deriving (Eq, Ord, Enum, Num, Real, Integral)

-- |
-- Number of octaves.
-- May be negative, indicating a downward interval.
newtype Octaves = Octaves {getOctaves :: Integer}
  deriving (Eq, Ord, Num, Enum, Real, Integral)

-- |
-- Number of semitones.
-- May be negative, indicating a downward interval.
type Semitones = ChromaticSteps

-- |
-- The /number/ component of an interval (fourth, fifth) etc.
-- May be negative, indicating a downward interval.
--
-- In keeping with music theory tradition, numbers are 1-indexed: the number 1
-- (or @unison@) represents an interval of 0 steps, 2 (or @second@) reprents an
-- interval of 1 step, and so on.
newtype Number = Number {getNumber :: Integer}
  deriving (Eq, Ord, Enum, Real, Integral)

instance Num Number where

  Number a + Number b = Number (a + b)

  Number a * Number b = Number (a * b)

  negate (Number a) = Number (negate a)

  abs (Number a) = Number (abs a)

  signum (Number a) = Number (signum a)

  fromInteger 0 = error "Invalid Number: zero"
  fromInteger n = Number n

-- |
-- The /quality/ component of an interval (minor, major, augmented).
-- Generalized from single\/double augmented\/diminished to any number of steps.
data Quality
  = Augmented Integer
  | Major
  | Perfect
  | Minor
  | Diminished Integer
  deriving (Eq, Ord, Show)

-- |
-- The actual alteration implied by a quality is dependent on whether it is attached
-- to a major\/minor vs. a perfect-style number. This type represents the two possibilities.
data QualityType = PerfectType | MajorMinorType
  deriving (Eq, Ord, Read, Show)

-- |
-- Accidental, represented as number of alterations.
-- Generalized from natural and single\/double sharp\/flat to any number of steps.
newtype Accidental = Accidental {getAccidental :: Integer}
  deriving (Eq, Ord, Num, Enum, Real, Integral)

-- |
-- Pitch name component.
data Name = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum)

-- |
-- This type represents standard basis for intervbals.
data IntervalBasis = Chromatic | Diatonic
  deriving (Eq, Ord, Show, Enum)

-- |
-- Interval type.
newtype Interval = Interval {getInterval :: (ChromaticSteps, DiatonicSteps)}
  deriving (Eq, Typeable)

-- |
-- Pitch type.
newtype Pitch = Pitch {getPitch :: Interval}
  deriving (Eq, Ord, Typeable)

instance Semigroup Interval where
  (<>) = (^+^)

instance Monoid Interval where

  mempty = _P1


instance VectorSpace Interval where

  type Scalar Interval = Integer

  (*^) = stackInterval
    where
      stackInterval :: Integer -> Interval -> Interval
      stackInterval n a
        | n >= 0 = mconcat $ replicate (fromIntegral n) a
        | otherwise = negateV $ stackInterval (negate n) a

instance AdditiveGroup Interval where

  zeroV = _P1

  (Interval (a1, d1)) ^+^ (Interval (a2, d2)) = Interval (a1 ^+^ a2, d1 ^+^ d2)

  negateV (Interval (a, d)) = Interval (- a, - d)

instance AffineSpace Pitch where

  type Diff Pitch = Interval

  Pitch a .-. Pitch b = a ^-^ b

  Pitch a .+^ b = Pitch (a ^+^ b)

instance Show Octaves where
  show = show . getOctaves

instance Show Number where
  show = show . getNumber

instance Show Accidental where
  show n
    | n == 0 = "natural"
    | n == 1 = "sharp"
    | n == (-1) = "flat"
    | n == 2 = "doubleSharp"
    | n == (-2) = "doubleFlat"
    | n > 0 = "sharp * " ++ show (getAccidental n)
    | n < 0 = "flat * " ++ show (negate $ getAccidental n)
    | otherwise = error "Impossible"

instance Alterable Accidental where

  sharpen = succ

  flatten = pred

-- | Lexicographical ordering, comparing the number component of the
--   interval first and the quality second.
instance Ord Interval where
  Interval a `compare` Interval b = swap a `compare` swap b
    where
      swap (x, y) = (y, x)

instance AdditiveGroup ChromaticSteps where

  zeroV = 0

  (^+^) = (+)

  negateV = negate

instance AdditiveGroup DiatonicSteps where

  zeroV = 0

  (^+^) = (+)

  negateV = negate

{-
The number portion of an interval (i.e. second, third, etc).

Note that the interval number is always one step larger than number of steps spanned by
the interval (i.e. a third spans two diatonic steps). Thus 'number' does not distribute
over addition:

> number (a + b) = number a + number b - 1
-}

{-
Common pitch representation.

Intervals and pitches can be added using '.+^'. To get the interval between
two pitches, use '.-.'.

Pitches are normally entered using the following literals.

> c d e f g a b

Notes with accidentals can be written by adding the @s@ or @b@ suffices
(or two for double sharps and flats).

> cs, ds, es ...    -- sharp
> cb, db, eb ...    -- flat
> css, dss, ess ... -- double sharp
> cbb, dbb, ebb ... -- double flat

There is also a convenience syntax for entering pitches one octave up or
down, using @'@ and @_@ respectively.

> g a b c'
> d c b_ c

Because of some overloading magic, we can actually write @sharp@ and
@flat@ as /postfix/ functions. This gives a better read:

> cs == c sharp
> db == c flat

You can of course use typical functional transformation of pitch as well.
For example 'sharpen' and 'flatten' are the ordinary (prefix) versions of
'sharp' and 'flat'

> sharpen c             == c sharp       == cs
> flatten d             == d flat        == ds
> (sharpen . sharpen) c == c doubleSharp == css
> (flatten . flatten) d == d doubleFlat  == dss

Note that there is no guarantee that your pitch representation use
enharmonic equivalence, so @cs == db@ may or may not hold.

> c .+^ minor third == eb
> f .-. c           == perfect fourth

Pitches are described by name, accidental and octave number.

> c   == fromIntegral 0
> _P4 == perfect fourth   == interval Perfect 5
> d5  == diminished fifth == diminish (perfect fifth)

-}

{-
A musical interval such as minor third, augmented fifth, duodecim etc.

We include direction in in this definition, so a downward minor third (written @-m3@)
is distinct from an upward minor third (written @m3@). Note that @_P1@ and @-P1@ are
synynoms.

Not to be confused with a mathematical inverval in pitch space, which is called
'Ambitus'. Intervals and pitches form an affine-vector space pair with intervals and
/vectors/ and pitches as /points/. To add an interval to a, use '.+^'. To get the
interval between two pitches, use '.-.'.

> c .+^ minor third == eb
> f .-. c           == perfect fourth

Adding intervals preserves spelling. For example:

> m3 ^+^ _M3 = _P5
> d5 ^+^ _M6 = m10

The scalar type of 'Interval' is 'Int', using '^*' to stack intervals of a certain type
on top of each other. For example @_P5 ^* 2@ is a stack of 2 perfect fifths, or a major
ninth. The 'Num' instance works as expected for '+', 'negate' and 'abs', and
(arbitrarily) uses minor seconds for multiplication. If you find yourself '*', or
'signum' on intervals, consider switching to '*^' or 'normalized'.

Intervals are generally described in terms of 'Quality' and 'Number'. To construct an
interval, use the 'interval' constructor, the utility constructors 'major', 'minor',
'augmented' and 'diminished', or the interval literals:

> m5  == minor   fifth    == interval Minor   5 > _P4 == perfect fourth   == interval
Perfect 5 > d5  == diminished fifth == diminish (perfect fifth)

-}

{-
An accidental is either flat, natural or sharp.

This representation allows for an arbitrary number of flats or sharps rather than just
single and double.

The 'Num' and 'Enum' instances treat 'Accidental' as the number of altered semitones,
i.e. a double flat is @-2@, natural @0@ and so on.

-}

{-
Interval quality is either perfect, major, minor, augmented, and
diminished. This representation allows for an arbitrary number of
augmentations or diminutions, so /augmented/ is represented by @Augmented
1@, /doubly augmented/ by @Augmented 2@ and so on.

The quality of a compound interval is the quality of the simple interval on
which it is based.

-}

{-
An interval represented as a number of octaves, including negative
intervals.

> octaves a = semitones a `div` 12
> steps   a = semitones a `mod` 12
-}


-- |
-- Returns whether the given interval is negative.
--
-- >>> isNegative m3
-- False
--
-- >>> isNegative (-m3)
-- True
isNegative :: Interval -> Bool
isNegative (Interval (a, d)) = d < 0

-- |
-- Returns whether the given interval is positive.
isPositive :: Interval -> Bool
isPositive x@(Interval (a, d)) = d >= 0 && not (isPerfectUnison x)

instance Show Interval where
  show a
    | isNegative a = "-" ++ showQuality (quality a) ++ show (abs $ number a)
    | otherwise = showQuality (quality a) ++ show (abs $ number a)
    where
      showQuality Major = "_M"
      showQuality Minor = "m"
      showQuality Perfect = "_P"
      showQuality (Augmented n) = "_" ++ replicate (fromIntegral n) 'A'
      showQuality (Diminished n) = replicate (fromIntegral n) 'd'

instance HasBasis Interval where

  type Basis Interval = IntervalBasis

  basisValue Chromatic = _A1
  basisValue Diatonic = d2

  decompose (Interval (c, d)) = [(Chromatic, fromIntegral c), (Diatonic, fromIntegral d)]

  decompose' (Interval (c, d)) Chromatic = fromIntegral c
  decompose' (Interval (c, d)) Diatonic = fromIntegral d

instance HasQuality Interval where
  quality :: Interval -> Quality
  quality = go
    where
      -- This is finicky, as the A1 and d2 intervals interact in a
      -- complex way to produce the perfect/major/minor/etc. intervals that
      -- we are used to reading.
      go (Interval (a, d))
        | (a < 0) && (d == 0) = diminish $ go (Interval ((a + 1), d))
        | (a, d) == (0, 0) = Perfect
        | (a > 0) && (d == 0) = augment $ go (Interval ((a - 1), d))
        | (a < 1) && (d == 1) = diminish $ go (Interval ((a + 1), d))
        | (a, d) == (1, 1) = Minor
        | (a, d) == (2, 1) = Major
        | (a > 2) && (d == 1) = augment $ go (Interval ((a - 1), d))
        | (a < 3) && (d == 2) = diminish $ go (Interval ((a + 1), d))
        | (a, d) == (3, 2) = Minor
        | (a, d) == (4, 2) = Major
        | (a > 4) && (d == 2) = augment $ go (Interval ((a - 1), d))
        | (a < 5) && (d == 3) = diminish $ go (Interval ((a + 1), d))
        | (a, d) == (5, 3) = Perfect
        | (a > 5) && (d == 3) = augment $ go (Interval ((a - 1), d))
        | (a < 7) && (d == 4) = diminish $ go (Interval ((a + 1), d))
        | (a, d) == (7, 4) = Perfect
        | (a > 7) && (d == 4) = augment $ go (Interval ((a - 1), d))
        | (a < 8) && (d == 5) = diminish $ go (Interval ((a + 1), d))
        | (a, d) == (8, 5) = Minor
        | (a, d) == (9, 5) = Major
        | (a > 9) && (d == 5) = augment $ go (Interval ((a - 1), d))
        | (a < 10) && (d == 6) = diminish $ go (Interval ((a + 1), d))
        | (a, d) == (10, 6) = Minor
        | (a, d) == (11, 6) = Major
        | (a > 11) && (d == 6) = augment $ go (Interval ((a - 1), d))
        | (a < 12) && (d == 7) = diminish $ go (Interval ((a + 1), d))
        | (a, d) == (12, 7) = Perfect
        | (a > 12) && (d == 7) = augment $ go (Interval ((a - 1), d))
        -- note: these last two cases *have* to be this way round, otherwise
        -- infinite loop occurs.
        | (a > 12) || (d > 7) = go (Interval ((a - 12), (d - 7)))
        | (a < 0) || (d < 0) = go (Interval ((- a), (- d)))
        | otherwise = error "Impossible (TODO prove)"

instance HasNumber Interval where
  number :: Interval -> Number
  number (Interval (_a, d))
    | d >= 0 = fromIntegral (d + 1)
    | otherwise = fromIntegral (d - 1)

instance Augmentable Interval where

  augment i = i ^+^ _A1

  diminish i = i ^-^ _A1

instance HasSemitones Interval where
  semitones (Interval (a, d)) = fromIntegral a -- assuming "semitone" == A1

instance ToJSON DiatonicSteps where
  toJSON = toJSON . toInteger

instance FromJSON DiatonicSteps where
  parseJSON = fmap fromInteger . parseJSON

instance ToJSON ChromaticSteps where
  toJSON = toJSON . toInteger

instance FromJSON ChromaticSteps where
  parseJSON = fmap fromInteger . parseJSON

instance ToJSON Interval where
  toJSON i = Data.Aeson.object [("steps", toJSON $ i ^. _steps), ("alteration", toJSON $ i ^. _alteration)]

instance FromJSON Interval where
  parseJSON (Data.Aeson.Object x) = liftA2 (curry (^. intervalAlterationSteps)) alteration steps
    where
      steps = x Data.Aeson..: "steps"
      alteration = x Data.Aeson..: "alteration"
  parseJSON _ = empty

intervalDiff :: Interval -> Int
intervalDiff (Interval (c, d)) = fromIntegral $ c - fromIntegral (diatonicToChromatic d)

-- |
-- Creates an interval from a quality and number.
--
-- Given 'Perfect' with an number not indicating a perfect consonant, 'interval' returns a
-- major interval instead. Given 'Major' or 'Minor' with a number indicating a perfect
-- consonance, 'interval' returns a perfect or diminished interval respectively.
mkInterval' ::
  -- | Difference in chromatic steps (?).
  Int ->
  -- | Number of diatonic steps (NOT interval number).
  Int ->
  Interval
mkInterval' diff diatonic = Interval (diatonicToChromatic (fromIntegral diatonic) + fromIntegral diff, fromIntegral diatonic)

-- | Creates a perfect interval.
--   If given an inperfect number, constructs a major interval.
perfect :: Number -> Maybe Interval
perfect = mkIntervalS Perfect

-- | Creates a major interval.
--   If given a perfect number, constructs a perfect interval.
major :: Number -> Maybe Interval
major = mkIntervalS Major

-- | Creates a minor interval.
--   If given a perfect number, constructs a diminished interval.
minor :: Number -> Maybe Interval
minor = mkIntervalS Minor

-- | Creates an augmented interval.
augmented :: Number -> Maybe Interval
augmented = mkIntervalS (Augmented 1)

-- | Creates a diminished interval.
diminished :: Number -> Maybe Interval
diminished = mkIntervalS (Diminished 1)

-- | Creates a doubly augmented interval.
doublyAugmented :: Number -> Maybe Interval
doublyAugmented = mkIntervalS (Augmented 2)

-- | Creates a doubly diminished interval.
doublyDiminished :: Number -> Maybe Interval
doublyDiminished = mkIntervalS (Diminished 2)

-- |
-- Separate a compound interval into octaves and a simple interval.
--
-- > x*^_P8 + y = z  iff  (x, y) = separate z
--
-- >>> separate (2*^_P8+m3)
-- (2,m3)
-- >>> separate (3*^_P8+m3)
-- (3,m3)
-- >>> separate (0*^_P8+m3)
-- (0,m3)
-- >>> separate ((-1)*^_P8+m3)
-- (-1,m3)
separate :: Interval -> (Octaves, Interval)
separate i = (fromIntegral o, i ^-^ (fromIntegral o *^ _P8))
  where
    o = octaves i

-- |
-- Returns the simple part of an interval.
--
-- > (perfect octave)^*x + y = z  iff  y = simple z
simple :: Interval -> Interval
simple = snd . separate

{-
  TODO
  Generalize simple like this:
    > (number (id @Interval (m9))-(fromIntegral $ signum (m9))) `mod` 7

-}

-- |
-- Returns whether the given interval is simple.
--
-- A simple interval is a non-negative interval spanning less than one octave.
--
-- >>> isSimple _P5
-- True
--
-- >>> filter isSimple [_P5, _M9, -m3, m3, _A7, _P8]
-- [_P5,m3,_A7]
isSimple :: Interval -> Bool
isSimple x = octaves x == 0

-- |
-- Returns whether the given interval is compound.
--
-- A compound interval is either a negative interval, or a positive interval spanning
-- one octave or more. Note that compound intervals may be smaller than an octave if
-- they are negative, so
--
-- >>> isCompound (-m3)
-- True
-- >>> isCompound $ abs (-m3)
-- False
isCompound :: Interval -> Bool
isCompound x = octaves x /= 0

-- |
-- Returns whether the given interval is non-negative. This implies that it is either positive or a perfect unison.
isNonNegative :: Interval -> Bool
isNonNegative (Interval (a, d)) = d >= 0

-- |
-- Returns whether the given interval a perfect unison.
--
-- >>> isPerfectUnison _P1
-- True
isPerfectUnison :: Interval -> Bool
isPerfectUnison (Interval (a, d)) = (a, d) == (0, 0)

-- |
-- Returns whether the given interval is a step (a second or smaller).
--
-- Only diatonic 'number' is taken into account, so @_A2@ is considered
-- a step and @m3@ a leap, even though they have the same number of
-- semitones.
--
-- >>> isLeap _A2
-- False
--
-- >>> isLeap m3
-- True
--
-- >>> isLeap m3
-- True
--
-- >>> isLeap _M2
-- False
--
-- >>> isLeap _P1
-- False
isStep :: Interval -> Bool
isStep (Interval (a, d)) = (abs d) <= 1

-- |
-- Returns whether the given interval is a leap (larger than a second).
--
-- Only the diatonic 'number' is taken into account, so @_A2@ is considered
-- a step and @m3@ a leap, even though they have the same number of
-- semitones.
--
-- >>> isLeap m3
-- True
--
-- >>> isLeap _M2
-- False
--
-- >>> isLeap _P1
-- False
isLeap :: Interval -> Bool
isLeap (Interval (a, d)) = (abs d) > 1

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
-- >>> invert _P4
-- _P5
--
-- >>> invert _A4
-- d5
--
-- >>> invert m3
-- _M6
--
-- >>> invert (_P8 ^+^ m3)
-- _M6
--
-- >>> invert (-m3)
-- m3
invert :: Interval -> Interval
invert = simple . negateV

-- | View or set the alteration (i.e. the number of chromatic steps differing from the excepted number) in an interval.
_alteration :: Lens' Interval ChromaticSteps
_alteration = from intervalAlterationSteps . _1

-- | View or set the number of chromatic steps in an interval.
_steps :: Lens' Interval DiatonicSteps
_steps = from intervalAlterationSteps . _2

-- | View an interval as a pair of quality and number or vice versa.
--
-- >>> (Major, third)^?interval
-- Just _M3
--
-- >>> (Perfect, 4)^?interval
-- Just _P4
--
-- >>> (Perfect, 12)^?interval
-- Just _P12
--
-- This is a 'Prism', as only certain combinations of quality and number
-- make sense.
--
-- >>> (Perfect, 3)^?interval
-- Nothing
--
-- >>> (Minor, 4)^?interval
-- Nothing
--
-- This being a 'Prism', you can also use it backwards:
--
-- >>> m3^.re interval
-- (Minor,3)
interval :: Prism' (Quality, Number) Interval
interval =
  prism'
    (\x -> (quality x, number x))
    (uncurry mkIntervalS)

-- | View an interval as a pair of alteration and diatonic steps or vice versa.
--
-- >>> _P5^.from intervalAlterationSteps
-- (0,4)
--
-- >>> d5^.from intervalAlterationSteps
-- (-1,4)
intervalAlterationSteps :: Iso' (ChromaticSteps, DiatonicSteps) Interval
intervalAlterationSteps =
  iso
    (\(d, s) -> mkInterval' (fromIntegral d) (fromIntegral s))

    (\x -> (qualityToDiff (number x) (expectedQualityType (number x)) (quality x), view diatonicSteps $ number x))
  where
    qualityToDiff n qt q = fromMaybe e $ qualityToAlteration (numberDirection n) qt q
      where
        e = error "Impossible (TODO prove)"

mkIntervalS :: Quality -> Number -> Maybe Interval
mkIntervalS q n = mkInterval' <$> (fromIntegral <$> diff) <*> (pure $ steps n)
  where
    diff = qualityToAlteration (numberDirection n) (expectedQualityType n) (q)


-- TODO rename numberToDiatonicSteps? Compare other Isos

-- |
-- >>> second^.diatonicSteps
-- 1
--
-- >>> 2^.diatonicSteps
-- 1
--
-- >>> 3^.from diatonicSteps
-- 4
diatonicSteps :: Iso' Number DiatonicSteps
diatonicSteps = iso steps d2n
  where

    d2n :: DiatonicSteps -> Number
    d2n n | n >= 0 = fromIntegral (n + 1)
          | n < 0  = fromIntegral (n - 1)
          | otherwise = error "Impossible"

steps :: Integral a => Number -> a
steps n = case fromIntegral n `compare` (0 :: Integer) of
  GT -> fromIntegral n - 1
  EQ -> error "Impossible: Number can not be 0"
  LT -> fromIntegral n + 1

numberDirection :: Number -> Direction
numberDirection n = case fromIntegral n `compare` (0 :: Integer) of
  GT -> Upward
  EQ -> error "Impossible: Number can not be 0"
  LT -> Downward

-- | View an interval as a pair of total number of chromatic and diatonic steps.
--
-- >>> _P5^.from intervalTotalSteps
-- (7,4)
--
-- >>> d5^.from intervalTotalSteps
-- (6,4)
intervalTotalSteps :: Iso' (ChromaticSteps, DiatonicSteps) Interval
intervalTotalSteps = iso Interval getInterval

diatonicToChromatic :: DiatonicSteps -> ChromaticSteps
diatonicToChromatic d = fromIntegral $ (octaves * 12) + go restDia
  where
    -- restDia is always in [0..6]
    (octaves, restDia) = fromIntegral d `divMod` 7
    go = ([0, 2, 4, 5, 7, 9, 11] !!)

-- | Integer div of intervals: i / di = x, where x is an integer
intervalDiv :: Interval -> Interval -> Int
intervalDiv (Interval (a, d)) (Interval (1, 0)) = fromIntegral a
intervalDiv (Interval (a, d)) (Interval (0, 1)) = fromIntegral d
intervalDiv i di
  | (i > _P1) = intervalDivPos i di
  | (i < _P1) = intervalDivNeg i di
  | otherwise = 0 :: Int
  where
    intervalDivPos i di
      | (i < _P1) = error "Impossible (TODO prove)"
      | (i ^-^ di) < _P1 = 0
      | otherwise = 1 + (intervalDiv (i ^-^ di) di)
    intervalDivNeg i di
      | (i > _P1) = error "Impossible (TODO prove)"
      | (i ^+^ di) > _P1 = 0
      | otherwise = 1 + (intervalDiv (i ^+^ di) di)

-- | Represent an interval i in a new basis (j, k).
--
-- We want x,y where i = x*j + y*k
--
-- e.g., convertBasis d2 _P5 _P8 == Just (-12,7), as expected.
convertBasis ::
  Interval ->
  Interval ->
  Interval ->
  Maybe (Int, Int)
convertBasis i j k
  | (p == 0) = Nothing
  | not $ p `divides` r = Nothing
  | not $ p `divides` q = Nothing
  | otherwise = Just (r `div` p, q `div` p)
  where
    Interval (fromIntegral -> m, fromIntegral -> n) = i
    Interval (fromIntegral -> a, fromIntegral -> b) = j
    Interval (fromIntegral -> c, fromIntegral -> d) = k
    p = (a * d - b * c)
    q = (a * n - b * m)
    r = (d * m - c * n)
    divides :: Integral a => a -> a -> Bool
    x `divides` y = (y `rem` x) == 0

-- | Same as above, but don't worry if new interval has non-integer
-- coefficients -- useful when getting a value to use as a frequency
-- ratio in a tuning system.
convertBasisFloat ::
  (Fractional t, Eq t) =>
  Interval ->
  Interval ->
  Interval ->
  Maybe (t, t)
convertBasisFloat i j k
  | (p == 0) = Nothing
  | otherwise = Just (r / p, q / p)
  where
    Interval (fromIntegral -> m, fromIntegral -> n) = i
    Interval (fromIntegral -> a, fromIntegral -> b) = j
    Interval (fromIntegral @_ @Integer -> c, fromIntegral -> d) = k
    p = fromIntegral $ (a * d - b * c)
    q = fromIntegral $ (a * n - b * m)
    r = fromIntegral $ (d * m - c * n)

instance HasOctaves Interval where
  octaves :: Interval -> Octaves
  octaves (Interval (_, d)) = fromIntegral $ d `div` 7

-- |
-- Class of intervals that has a number of 'Octaves'.
class HasOctaves a where
  -- |
  -- Returns the number of octaves spanned by an interval.
  --
  -- The number of octaves is negative if and only if the interval is
  -- negative.
  --
  -- Examples:
  --
  -- > octaves (perfect unison)  =  0
  -- > octaves (d5 ^* 4)         =  2
  -- > octaves (-_P8)            =  -1
  octaves :: a -> Octaves

instance HasOctaves Octaves where octaves = id

{-
-- |
-- Class of intervals that has a number of 'Steps'.
--
class HasSteps a where
  -- |
  -- The number of steps is always in the range /0 ≤ x < 12/.
  --
  -- Examples:
  --
  -- > octaves (perfect unison)  =  0
  -- > octaves (d5 ^* 4)         =  2
  -- > octaves (-m7)             =  -1
  --
  steps :: a -> Steps
-}

-- |
-- Class of intervals that can be converted to a number of 'Semitones'.
class HasSemitones a where
  -- |
  -- Returns the number of semitones spanned by an interval.
  --
  -- The number of semitones is negative if and only if the interval is
  -- negative.
  --
  -- >>> semitones (_P1 :: Interval)
  -- 0
  -- >>> semitones tritone
  -- 6
  -- >>> semitones (d5 :: Interval)
  -- 6
  -- >>> semitones (-_P8 :: Interval)
  -- -12
  semitones :: a -> Semitones

instance HasSemitones ChromaticSteps where semitones = id

semitone, tone, ditone, tritone :: Semitones

-- | Precisely one semitone.
semitone = 1

-- | Precisely one whole tone, or two semitones.
tone = 2

-- | Precisely two whole tones, or four semitones.
ditone = 4

-- | Precisely three whole tones, or six semitones.
tritone = 6

isTone, isSemitone, isTritone :: HasSemitones a => a -> Bool

-- | Returns true iff the given interval spans one semitone.
isSemitone = (== semitone) . abs . semitones

-- | Returns true iff the given interval spans one whole tone (two semitones).
isTone = (== tone) . abs . semitones

-- | Returns true iff the given interval spans three whole tones (six semitones).
isTritone = (== tritone) . abs . semitones

infix 4 =:=

infix 4 /:=

-- |
-- Enharmonic equivalence.
--
-- >>> id @Interval _A2 == m3
-- False
-- >>> id @Interval _A2 =:= m3
-- True
(=:=) :: HasSemitones a => a -> a -> Bool
a =:= b = semitones a == semitones b

-- |
-- Enharmonic non-equivalence.
--
-- >>> id @Interval _A2 /= m3
-- True
-- >>> id @Interval _A2 /:= m3
-- False
(/:=) :: HasSemitones a => a -> a -> Bool
a /:= b = semitones a /= semitones b

-- | Types of value that has an interval quality (mainly 'Interval' and 'Quality' itself).
class HasQuality a where
  -- >>> quality (m3 :: Interval)
  -- Minor
  --
  -- >>> quality $ diminish $ diminish m3
  -- Diminished 2
  --
  quality :: a -> Quality

-- |
-- Returns whether the given quality is perfect.
isPerfect :: HasQuality a => a -> Bool
isPerfect a = case quality a of Perfect -> True; _ -> False

-- |
-- Returns whether the given quality is major.
isMajor :: HasQuality a => a -> Bool
isMajor a = case quality a of Major -> True; _ -> False

-- |
-- Returns whether the given quality is minor.
isMinor :: HasQuality a => a -> Bool
isMinor a = case quality a of Minor -> True; _ -> False

-- |
-- Returns whether the given quality is augmented (including double augmented etc).
isAugmented :: HasQuality a => a -> Bool
isAugmented a = case quality a of Augmented _ -> True; _ -> False

-- |
-- Returns whether the given quality is diminished (including double diminished etc).
isDiminished :: HasQuality a => a -> Bool
isDiminished a = case quality a of Diminished _ -> True; _ -> False

-- |
-- Is this a quality used in standard notation
--
-- >>> filter isStandardQuality [Major,Minor,Perfect]
-- [Major,Minor,Perfect]
--
-- >>> filter isStandardQuality [Augmented n | n <- [1..4] ]
-- [Augmented 1,Augmented 2]
--
-- >>> filter isStandardQuality [Diminished n | n <- [1..4] ]
-- [Diminished 1,Diminished 2]
isStandardQuality :: Quality -> Bool
isStandardQuality (Augmented n) = n <= 2
isStandardQuality (Diminished n) = n <= 2
isStandardQuality _ = True

-- | Same as 'isStandardQuality' but also disallow doubly augmented/diminished.
--
-- >>> filter isSimpleQuality [Augmented n | n <- [1..4] ]
-- [Augmented 1]
--
-- >>> filter isSimpleQuality [Diminished n | n <- [1..4] ]
-- [Diminished 1]
isSimpleQuality :: Quality -> Bool
isSimpleQuality (Augmented n) = n <= 1
isSimpleQuality (Diminished n) = n <= 1
isSimpleQuality _ = True

instance HasQuality Quality where
  quality = id

-- | Augmentable Quality instance
--
-- This Augmentable instance exists solely for use of the quality
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
invertQuality :: Quality -> Quality
invertQuality = go
  where
    go Major = Minor
    go Minor = Major
    go Perfect = Perfect
    go (Augmented n) = Diminished n
    go (Diminished n) = Augmented n

-- |
-- The quality type expected for a given number, i.e. perfect for unisons, fourths,
-- fifths and their compounds; major/minor for everything else.
expectedQualityType :: Number -> QualityType
expectedQualityType x =
  if ((abs x - 1) `mod` 7) + 1 `elem` [1, 4, 5]
    then PerfectType
    else MajorMinorType

-- |
-- Return all possible quality types for a given quality.
qualityTypes :: Quality -> [QualityType]
qualityTypes Perfect = [PerfectType]
qualityTypes Major = [MajorMinorType]
qualityTypes Minor = [MajorMinorType]
qualityTypes _ = [PerfectType, MajorMinorType]

-- |
-- Return whether the given combination of quality and number forms a valid interval
-- expression.
isValidQualityNumber :: Quality -> Number -> Bool
isValidQualityNumber q n = expectedQualityType n `elem` qualityTypes q

data Direction = Upward | Downward
  deriving (Eq, Ord, Show)

-- |
-- Return the alteration in implied by the given quality to a number of a given quality type.
--
-- >>> qualityToAlteration Upward MajorMinorType (Augmented 1)
-- Just 1
--
-- >>> qualityToAlteration Upward MajorMinorType Minor
-- Just -1
--
-- >>> qualityToAlteration Upward MajorMinorType (Diminished 1)
-- Just -2
--
-- >>> qualityToAlteration Upward PerfectType (Augmented 1)
-- Just 1
--
-- >>> qualityToAlteration Upward PerfectType Perfect
-- Just 0
--
-- >>> qualityToAlteration Upward PerfectType (Diminished 1)
-- Just -1
--
-- >>> qualityToAlteration Upward PerfectType Major
-- Nothing
qualityToAlteration :: Direction -> QualityType -> Quality -> Maybe ChromaticSteps
qualityToAlteration d qt q = fmap fromIntegral $ go d qt q
  where
    go Upward MajorMinorType (Augmented n) = Just $ 0 + n
    go Upward MajorMinorType Major = Just $ 0
    go Upward MajorMinorType Minor = Just $ (-1)
    go Upward MajorMinorType (Diminished n) = Just $ - (1 + n)
    go Downward MajorMinorType (Augmented n) = Just $ - (1 + n)
    go Downward MajorMinorType Major = Just $ -1
    go Downward MajorMinorType Minor = Just $ 0
    go Downward MajorMinorType (Diminished n) = Just $ 0 + n
    go Upward PerfectType (Augmented n) = Just $ 0 + n
    go Upward PerfectType Perfect = Just $ 0
    go Upward PerfectType (Diminished n) = Just $ 0 - n
    go Downward PerfectType (Augmented n) = Just $ 0 - n
    go Downward PerfectType Perfect = Just $ 0
    go Downward PerfectType (Diminished n) = Just $ 0 + n
    go _ qt q = Nothing

-- TODO get rid of HasNumber, make (number :: Interval -> Number)
-- TODO get rid of HasQuality, make (quality :: Interval -> Quality)
instance HasNumber Number where number = id

-- | A synonym for @1@.
unison :: Number
unison = 1

-- | A synonym for @1@.
prime :: Number
prime = 1

-- | A synonym for @2@.
second :: Number
second = 2

-- | A synonym for @3@.
third :: Number
third = 3

-- | A synonym for @4@.
fourth :: Number
fourth = 4

-- | A synonym for @5@.
fifth :: Number
fifth = 5

-- | A synonym for @6@.
sixth :: Number
sixth = 6

-- | A synonym for @7@.
seventh :: Number
seventh = 7

-- | A synonym for @8@.
octave :: Number
octave = 8

-- | A synonym for @9@.
ninth :: Number
ninth = 9

-- | A synonym for @10@.
tenth :: Number
tenth = 10

-- | A synonym for @11@.
eleventh :: Number
eleventh = 11

-- | A synonym for @12@.
twelfth :: Number
twelfth = 12

-- | A synonym for @12@.
duodecim :: Number
duodecim = 12

-- | A synonym for @13@.
thirteenth :: Number
thirteenth = 13

-- | A synonym for @14@.
fourteenth :: Number
fourteenth = 14

-- | A synonym for @15@.
fifteenth :: Number
fifteenth = 15

class HasNumber a where
  -- |
  -- Returns the number portion of an interval.
  --
  -- The interval number is negative if and only if the interval is negative.
  --
  -- See also 'quality', 'octaves' and 'semitones'.
  --
  -- >>> number (m3 :: Interval)
  -- 3
  number :: a -> Number


-- | Whether the given interval is a (harmonic) dissonance.
isDissonance :: Interval -> Bool
isDissonance x = case number (simple x) of
  2 -> True
  7 -> True
  _ -> False

-- | Whether the given interval is a (harmonic) consonance.
isConsonance :: Interval -> Bool
isConsonance x = isPerfectConsonance x || isImperfectConsonance x

-- | Whether the given interval is a perfect (harmonic) consonance.
isPerfectConsonance :: Interval -> Bool
isPerfectConsonance x = case number (simple x) of
  1 -> True
  4 -> True
  5 -> True
  _ -> False

-- | Whether the given interval is an imperfect (harmonic) consonance.
isImperfectConsonance :: Interval -> Bool
isImperfectConsonance x = case number (simple x) of
  3 -> True
  6 -> True
  _ -> False

-- | Whether the given interval is a melodic dissonance.
isMelodicDissonance :: Interval -> Bool
isMelodicDissonance x = not $ isMelodicConsonance x

-- | Whether an interval is melodic consonance.
isMelodicConsonance :: Interval -> Bool
isMelodicConsonance x = quality x `elem` [Perfect, Major, Minor]

-- $semitonesAndSpellings
--
-- TODO document better
--
-- The `semitones` function retrieves the number of Semitones in a pitch, for example
--
-- > semitones :: Interval -> Semitones
-- > semitones major third = 4
--
-- Note that semitones is surjetive. We can define a non-deterministic function `spellings`
--
-- > spellings :: Semitones -> [Interval]
-- > spellings 4 = [majorThird, diminishedFourth]
--
-- /Law/
--
-- > map semitones (spellings a) = replicate n a    for all n > 0
--
-- /Lemma/
--
-- > map semitones (spellings a)

-- |
-- A spelling provide a way of notating a semitone interval such as 'tritone'.
--
--
-- >>> spell usingSharps tritone
-- _A4
--
-- >>> spell usingFlats  tritone
-- d5
--
-- >>> spell modally     tone
-- _M2
type Spelling = Semitones -> Number

-- |
-- Spell an interval using the given 'Spelling'.
spell :: HasSemitones a => Spelling -> a -> Interval
spell spelling x =
  let -- TODO use Steps etc to remove fromIntegral
      (octaves, steps) = semitones x `divMod` 12
      num = fromIntegral (spelling steps)
      diff = fromIntegral steps - fromInteger (diatonicToChromatic num)
   in (\a b -> (fromInteger a, fromInteger b) ^. intervalAlterationSteps) diff num ^+^ _P8 ^* (fromIntegral octaves)
  where
    diatonicToChromatic = go
      where
        go 0 = 0
        go 1 = 2
        go 2 = 4
        go 3 = 5
        go 4 = 7
        go 5 = 9
        go 6 = 11
        go _ = error "TODO: Use mod-12 arith type for argument to Spelling"

type Tonic = Pitch

spellPitchRelative :: Tonic -> Spelling -> Pitch -> Pitch
spellPitchRelative tonic s p = tonic .+^ spell s (p .-. tonic)

-- |
-- Flipped version of 'spell'. To be used infix, as in:
--
-- > d5 `spelled` usingSharps
spelled :: HasSemitones a => a -> Spelling -> Interval
spelled = flip spell

-- |
-- Spell using the most the most common accidentals. Double sharps and flats are not
-- preserved.
--
-- This spelling is particularly useful for modal music where the tonic is C.
--
-- > c cs d eb e f fs g gs a bb b
modally :: Spelling
modally = go
  where
    go 0 = 0
    go 1 = 0
    go 2 = 1
    go 3 = 2
    go 4 = 2
    go 5 = 3
    go 6 = 3
    go 7 = 4
    go 8 = 4
    go 9 = 5
    go 10 = 6
    go 11 = 6
    go _ = error "TODO: Use mod-12 arith type for argument to Spelling"

-- |
-- Spell using sharps. Double sharps and flats are not preserved.
--
-- > c cs d ds e f fs g gs a as b
usingSharps :: Spelling
usingSharps = go
  where
    go 0 = 0
    go 1 = 0
    go 2 = 1
    go 3 = 1
    go 4 = 2
    go 5 = 3
    go 6 = 3
    go 7 = 4
    go 8 = 4
    go 9 = 5
    go 10 = 5
    go 11 = 6
    go _ = error "TODO: Use mod-12 arith type for argument to Spelling"

-- |
-- Spell using flats. Double sharps and flats are not preserved.
--
-- > c db d eb e f gb g ab a bb b
usingFlats :: Spelling
usingFlats = go
  where
    go 0 = 0
    go 1 = 1
    go 2 = 1
    go 3 = 2
    go 4 = 2
    go 5 = 3
    go 6 = 4
    go 7 = 4
    go 8 = 5
    go 9 = 5
    go 10 = 6
    go 11 = 6
    go _ = error "TODO: Use mod-12 arith type for argument to Spelling"

{-
Respell preserving general augmented/diminished diretion, but disallow all qualities
except the standard ones.

Standard qualities include major, minor, perfect, augmented/diminished or
doubly augmented/diminished.
-}
useStandardQualities :: Interval -> Interval
useStandardQualities i
  | quality i > Perfect && not (ok i) = spell usingSharps i
  | quality i < Perfect && not (ok i) = spell usingFlats i
  | otherwise = i
  where
    ok i = isStandardQuality (quality i)

{-
Same as 'useStandardQualities' but disallow doubly augmented/diminished.
-}
useSimpleQualities :: Interval -> Interval
useSimpleQualities i
  | quality i > Perfect && not (ok i) = spell usingSharps i
  | quality i < Perfect && not (ok i) = spell usingFlats i
  | otherwise = i
  where
    ok i = isSimpleQuality (quality i)

{-
Respell preserving general sharp/flat diretion, but disallow all qualities
except the standard ones.

Standard qualities include natural, sharp, flat, double sharp and double flat.
-}
useStandardAlterations :: Tonic -> Pitch -> Pitch
useStandardAlterations tonic p
  | quality i > Perfect && not (ok i) = spellPitchRelative tonic usingSharps p
  | quality i < Perfect && not (ok i) = spellPitchRelative tonic usingFlats p
  | otherwise = p
  where
    i = p .-. tonic
    ok i = isStandardQuality (quality i)

{-
Same as 'useStandardAlterations' but disallow double sharp/flat.
-}
useSimpleAlterations :: Tonic -> Pitch -> Pitch
useSimpleAlterations tonic p
  | quality i > Perfect && not (ok i) = spellPitchRelative tonic usingSharps p
  | quality i < Perfect && not (ok i) = spellPitchRelative tonic usingFlats p
  | otherwise = p
  where
    i = p .-. tonic
    ok i = isSimpleQuality (quality i)

sharp, flat, natural, doubleFlat, doubleSharp :: Accidental

-- | The double sharp accidental.
doubleSharp = 2

-- | The sharp accidental.
sharp = 1

-- | The natural accidental.
natural = 0

-- | The flat accidental.
flat = -1

-- | The double flat accidental.
doubleFlat = -2

isNatural, isSharpened, isFlattened :: Accidental -> Bool

-- | Returns whether this is a natural accidental.
isNatural = (== 0)

-- | Returns whether this is a sharp, double sharp etc.
isSharpened = (> 0)

-- | Returns whether this is a flat, double flat etc.
isFlattened = (< 0)

-- | Returns whether this is a standard accidental, i.e.
--   either a double flat, flat, natural, sharp or double sharp.
isStandardAccidental :: Accidental -> Bool
isStandardAccidental a = abs a < 2

-- was: isStandard

-- instance IsPitch Pitch where
--   fromPitch (PitchL (c, a, o)) =
--     Pitch $ (\a b -> (fromIntegral a, fromIntegral b)^.intervalTotalSteps) (qual a) c ^+^ (_P8^* fromIntegral o)
--     where
--       qual Nothing  = 0
--       qual (Just n) = round n

-- |
-- Enum instance using diatonic pitch.
--
-- >>> toEnum 0 :: Pitch
-- c
--
-- >>> toEnum 8 :: Pitch
-- d'
--
--
-- >>> [c..g] :: [Pitch]
-- [c,d,e,f,g]
instance Enum Pitch where

  toEnum = Pitch . view intervalAlterationSteps . (0,) . fromIntegral

  fromEnum = fromIntegral . snd . view (from intervalAlterationSteps) . getPitch

instance Alterable Pitch where

  sharpen (Pitch a) = Pitch (augment a)

  flatten (Pitch a) = Pitch (diminish a)

instance Show Pitch where
  show p = showName (name p) ++ showAccidental (accidental p) ++ showOctave (octaves $ getPitch p)
    where
      showName = fmap Char.toLower . show
      showOctave n
        | n > 0 = replicate (fromIntegral n) '\''
        | otherwise = replicate (negate $ fromIntegral n) '_'
      showAccidental n
        | n > 0 = replicate (fromIntegral n) 's'
        | otherwise = replicate (negate $ fromIntegral n) 'b'

-- |
-- This instance exists only for the syntactic convenience of writing @-m3@, rather
-- than @negateV m3@.
--
-- Avoid using '(*)', 'signum' or `fromInteger` on intervals. For multiplication,
-- see the `VectorSpace` instance.
instance Num Interval where

  (+) = (^+^)

  -- TODO negate/abs should not be used
  negate = negateV

  abs a = if isNegative a then negate a else a

  (*) = error "Music.Pitch.Common.Interval: no overloading for (*)"

  signum = error "Music.Pitch.Common.Interval: no overloading for signum"

  fromInteger = error "Music.Pitch.Common.Interval: no overloading for fromInteger"

instance ToJSON Pitch where
  toJSON = toJSON . (.-. c)

instance IsPitch Int where
  fromPitch x = fromIntegral (fromPitch x :: Integer)

instance IsPitch Word where
  fromPitch x = fromIntegral (fromPitch x :: Integer)

instance IsPitch Float where
  fromPitch x = realToFrac (fromPitch x :: Double)

instance HasResolution a => IsPitch (Fixed a) where
  fromPitch x = realToFrac (fromPitch x :: Double)

instance Integral a => IsPitch (Ratio a) where
  fromPitch x = realToFrac (fromPitch x :: Double)

instance IsPitch Double where
  fromPitch p = fromIntegral . semitones $ (p .-. c)

instance IsPitch Integer where
  fromPitch p = fromIntegral . semitones $ (p .-. c)

instance IsPitch Pitch where
  fromPitch = id

instance FromJSON Pitch where
  parseJSON = fmap (c .+^) . parseJSON

instance Transformable Pitch where
  transform _ = id

-- |
-- Creates a pitch from name accidental.
mkPitch :: Name -> Accidental -> Pitch
mkPitch name acc = Pitch $ (\a b -> (fromIntegral a, fromIntegral b) ^. intervalAlterationSteps) acc (fromEnum name)

{-
-- TODO name
-- TODO use this to define pitch-class equivalence
toFirstOctave :: Pitch -> Pitch
toFirstOctave p = case (name p, accidental p) of
  (n, a) -> mkPitch n a
-}

-- TODO rename (and flip Iso!)
--
--    intervalTotalSteps -> totalSteps
--    intervalAlterationSteps -> alterationSteps
--
-- TODO add similar Iso to replace mkPitch:
--
--    nameAccidental :: Iso' Pitch (Name, Accidental)
--

-- |
-- Returns the name of a pitch.
--
-- To convert a pitch to a numeric type, use 'octaves', 'steps' or 'semitones'
-- on the relevant interval type, for example:
--
-- @
-- semitones ('a\'' .-. 'c')
-- @
name :: Pitch -> Name
name x =
  -- Should always be in [0..6], but 7 arises for flat C etc.
  toEnum (i `mod` 7)
  where
    i = (fromIntegral . pred . number . simple . getPitch) x

-- |
-- Returns the accidental of a pitch.
--
-- See also 'octaves', and 'steps' and 'semitones'.
accidental :: Pitch -> Accidental
accidental = fromIntegral . view _alteration . simple . getPitch
  where

upChromaticP :: Pitch -> ChromaticSteps -> Pitch -> Pitch
upChromaticP origin n = relative origin $ (_alteration +~ n)

downChromaticP :: Pitch -> ChromaticSteps -> Pitch -> Pitch
downChromaticP origin n = relative origin $ (_alteration -~ n)

upDiatonicP :: Pitch -> DiatonicSteps -> Pitch -> Pitch
upDiatonicP origin n = relative origin $ (_steps +~ n)

downDiatonicP :: Pitch -> DiatonicSteps -> Pitch -> Pitch
downDiatonicP origin n = relative origin $ (_steps -~ n)

invertDiatonicallyP :: Pitch -> Pitch -> Pitch
invertDiatonicallyP origin = relative origin $ (_steps %~ negate)

invertChromaticallyP :: Pitch -> Pitch -> Pitch
invertChromaticallyP origin = relative origin $ (_alteration %~ negate)

-- Pitch literal, defined as @(class, alteration, octave)@, where
--
--     * @class@      is a pitch class number in @[0..6]@, starting from C.
--
--     * @alteration@ is the number of semitones, i.e. 0 is natural, 1 for sharp 2 for double sharp, -1 for flat and -2 for double flat.
--       Alteration is in 'Maybe' because some pitch representations differ between explicit and explicit accidentals, i.e. a diatonic
--       pitch type may assume @(0,Nothing,...)@ to mean C sharp rather than C.
--
--     * @octave@     is octave number in scientific pitch notation - 4.
--
-- Middle C is represented by the pitch literal @(0, Nothing, 0)@.
--
-- newtype PitchL = PitchL { getPitchL :: (Int, Maybe Double, Int) }
-- deriving (Eq, Show, Ord)

class IsPitch a where
  fromPitch :: Pitch -> a

instance IsPitch a => IsPitch (Maybe a) where
  fromPitch = pure . fromPitch

instance IsPitch a => IsPitch (First a) where
  fromPitch = pure . fromPitch

instance IsPitch a => IsPitch (Last a) where
  fromPitch = pure . fromPitch

instance IsPitch a => IsPitch [a] where
  fromPitch = pure . fromPitch

instance (Monoid b, IsPitch a) => IsPitch (b, a) where
  fromPitch = pure . fromPitch

deriving instance IsPitch a => IsPitch (Data.Semigroup.Sum a)

deriving instance IsPitch a => IsPitch (Data.Semigroup.Product a)

deriving instance (Monoid b, IsPitch a) => IsPitch (Couple b a)

-- TODO clean by inlining this whole thing or similar
viaPitchL :: (Int, Int, Int) -> Pitch
viaPitchL (pc, sem, oct) = Pitch $ mkInterval' sem (oct * 7 + pc)
  where
    mkInterval' diff diatonic = Interval (diatonicToChromatic (fromIntegral diatonic) + fromIntegral diff, fromIntegral diatonic)
    diatonicToChromatic :: DiatonicSteps -> ChromaticSteps
    diatonicToChromatic d = fromIntegral $ (octaves * 12) + go restDia
      where
        -- restDia is always in [0..6]
        (octaves, restDia) = fromIntegral d `divMod` 7
        go = ([0, 2, 4, 5, 7, 9, 11] !!)

cs'''' = fromPitch $ viaPitchL (0, 1, 4)

ds'''' = fromPitch $ viaPitchL (1, 1, 4)

es'''' = fromPitch $ viaPitchL (2, 1, 4)

fs'''' = fromPitch $ viaPitchL (3, 1, 4)

gs'''' = fromPitch $ viaPitchL (4, 1, 4)

as'''' = fromPitch $ viaPitchL (5, 1, 4)

bs'''' = fromPitch $ viaPitchL (6, 1, 4)

c'''' = fromPitch $ viaPitchL (0, 0, 4)

d'''' = fromPitch $ viaPitchL (1, 0, 4)

e'''' = fromPitch $ viaPitchL (2, 0, 4)

f'''' = fromPitch $ viaPitchL (3, 0, 4)

g'''' = fromPitch $ viaPitchL (4, 0, 4)

a'''' = fromPitch $ viaPitchL (5, 0, 4)

b'''' = fromPitch $ viaPitchL (6, 0, 4)

cb'''' = fromPitch $ viaPitchL (0, (-1), 4)

db'''' = fromPitch $ viaPitchL (1, (-1), 4)

eb'''' = fromPitch $ viaPitchL (2, (-1), 4)

fb'''' = fromPitch $ viaPitchL (3, (-1), 4)

gb'''' = fromPitch $ viaPitchL (4, (-1), 4)

ab'''' = fromPitch $ viaPitchL (5, (-1), 4)

bb'''' = fromPitch $ viaPitchL (6, (-1), 4)

cs''' = fromPitch $ viaPitchL (0, 1, 3)

ds''' = fromPitch $ viaPitchL (1, 1, 3)

es''' = fromPitch $ viaPitchL (2, 1, 3)

fs''' = fromPitch $ viaPitchL (3, 1, 3)

gs''' = fromPitch $ viaPitchL (4, 1, 3)

as''' = fromPitch $ viaPitchL (5, 1, 3)

bs''' = fromPitch $ viaPitchL (6, 1, 3)

c''' = fromPitch $ viaPitchL (0, 0, 3)

d''' = fromPitch $ viaPitchL (1, 0, 3)

e''' = fromPitch $ viaPitchL (2, 0, 3)

f''' = fromPitch $ viaPitchL (3, 0, 3)

g''' = fromPitch $ viaPitchL (4, 0, 3)

a''' = fromPitch $ viaPitchL (5, 0, 3)

b''' = fromPitch $ viaPitchL (6, 0, 3)

cb''' = fromPitch $ viaPitchL (0, (-1), 3)

db''' = fromPitch $ viaPitchL (1, (-1), 3)

eb''' = fromPitch $ viaPitchL (2, (-1), 3)

fb''' = fromPitch $ viaPitchL (3, (-1), 3)

gb''' = fromPitch $ viaPitchL (4, (-1), 3)

ab''' = fromPitch $ viaPitchL (5, (-1), 3)

bb''' = fromPitch $ viaPitchL (6, (-1), 3)

cs'' = fromPitch $ viaPitchL (0, 1, 2)

ds'' = fromPitch $ viaPitchL (1, 1, 2)

es'' = fromPitch $ viaPitchL (2, 1, 2)

fs'' = fromPitch $ viaPitchL (3, 1, 2)

gs'' = fromPitch $ viaPitchL (4, 1, 2)

as'' = fromPitch $ viaPitchL (5, 1, 2)

bs'' = fromPitch $ viaPitchL (6, 1, 2)

c'' = fromPitch $ viaPitchL (0, 0, 2)

d'' = fromPitch $ viaPitchL (1, 0, 2)

e'' = fromPitch $ viaPitchL (2, 0, 2)

f'' = fromPitch $ viaPitchL (3, 0, 2)

g'' = fromPitch $ viaPitchL (4, 0, 2)

a'' = fromPitch $ viaPitchL (5, 0, 2)

b'' = fromPitch $ viaPitchL (6, 0, 2)

cb'' = fromPitch $ viaPitchL (0, (-1), 2)

db'' = fromPitch $ viaPitchL (1, (-1), 2)

eb'' = fromPitch $ viaPitchL (2, (-1), 2)

fb'' = fromPitch $ viaPitchL (3, (-1), 2)

gb'' = fromPitch $ viaPitchL (4, (-1), 2)

ab'' = fromPitch $ viaPitchL (5, (-1), 2)

bb'' = fromPitch $ viaPitchL (6, (-1), 2)

cs' = fromPitch $ viaPitchL (0, 1, 1)

ds' = fromPitch $ viaPitchL (1, 1, 1)

es' = fromPitch $ viaPitchL (2, 1, 1)

fs' = fromPitch $ viaPitchL (3, 1, 1)

gs' = fromPitch $ viaPitchL (4, 1, 1)

as' = fromPitch $ viaPitchL (5, 1, 1)

bs' = fromPitch $ viaPitchL (6, 1, 1)

c' = fromPitch $ viaPitchL (0, 0, 1)

d' = fromPitch $ viaPitchL (1, 0, 1)

e' = fromPitch $ viaPitchL (2, 0, 1)

f' = fromPitch $ viaPitchL (3, 0, 1)

g' = fromPitch $ viaPitchL (4, 0, 1)

a' = fromPitch $ viaPitchL (5, 0, 1)

b' = fromPitch $ viaPitchL (6, 0, 1)

cb' = fromPitch $ viaPitchL (0, (-1), 1)

db' = fromPitch $ viaPitchL (1, (-1), 1)

eb' = fromPitch $ viaPitchL (2, (-1), 1)

fb' = fromPitch $ viaPitchL (3, (-1), 1)

gb' = fromPitch $ viaPitchL (4, (-1), 1)

ab' = fromPitch $ viaPitchL (5, (-1), 1)

bb' = fromPitch $ viaPitchL (6, (-1), 1)

cs = fromPitch $ viaPitchL (0, 1, 0)

ds = fromPitch $ viaPitchL (1, 1, 0)

es = fromPitch $ viaPitchL (2, 1, 0)

fs = fromPitch $ viaPitchL (3, 1, 0)

gs = fromPitch $ viaPitchL (4, 1, 0)

as = fromPitch $ viaPitchL (5, 1, 0)

bs = fromPitch $ viaPitchL (6, 1, 0)

c = fromPitch $ viaPitchL (0, 0, 0)

d = fromPitch $ viaPitchL (1, 0, 0)

e = fromPitch $ viaPitchL (2, 0, 0)

f = fromPitch $ viaPitchL (3, 0, 0)

g = fromPitch $ viaPitchL (4, 0, 0)

a = fromPitch $ viaPitchL (5, 0, 0)

b = fromPitch $ viaPitchL (6, 0, 0)

cb = fromPitch $ viaPitchL (0, (-1), 0)

db = fromPitch $ viaPitchL (1, (-1), 0)

eb = fromPitch $ viaPitchL (2, (-1), 0)

fb = fromPitch $ viaPitchL (3, (-1), 0)

gb = fromPitch $ viaPitchL (4, (-1), 0)

ab = fromPitch $ viaPitchL (5, (-1), 0)

bb = fromPitch $ viaPitchL (6, (-1), 0)

cs_ = fromPitch $ viaPitchL (0, 1, -1)

ds_ = fromPitch $ viaPitchL (1, 1, -1)

es_ = fromPitch $ viaPitchL (2, 1, -1)

fs_ = fromPitch $ viaPitchL (3, 1, -1)

gs_ = fromPitch $ viaPitchL (4, 1, -1)

as_ = fromPitch $ viaPitchL (5, 1, -1)

bs_ = fromPitch $ viaPitchL (6, 1, -1)

c_ = fromPitch $ viaPitchL (0, 0, -1)

d_ = fromPitch $ viaPitchL (1, 0, -1)

e_ = fromPitch $ viaPitchL (2, 0, -1)

f_ = fromPitch $ viaPitchL (3, 0, -1)

g_ = fromPitch $ viaPitchL (4, 0, -1)

a_ = fromPitch $ viaPitchL (5, 0, -1)

b_ = fromPitch $ viaPitchL (6, 0, -1)

cb_ = fromPitch $ viaPitchL (0, (-1), -1)

db_ = fromPitch $ viaPitchL (1, (-1), -1)

eb_ = fromPitch $ viaPitchL (2, (-1), -1)

fb_ = fromPitch $ viaPitchL (3, (-1), -1)

gb_ = fromPitch $ viaPitchL (4, (-1), -1)

ab_ = fromPitch $ viaPitchL (5, (-1), -1)

bb_ = fromPitch $ viaPitchL (6, (-1), -1)

cs__ = fromPitch $ viaPitchL (0, 1, -2)

ds__ = fromPitch $ viaPitchL (1, 1, -2)

es__ = fromPitch $ viaPitchL (2, 1, -2)

fs__ = fromPitch $ viaPitchL (3, 1, -2)

gs__ = fromPitch $ viaPitchL (4, 1, -2)

as__ = fromPitch $ viaPitchL (5, 1, -2)

bs__ = fromPitch $ viaPitchL (6, 1, -2)

c__ = fromPitch $ viaPitchL (0, 0, -2)

d__ = fromPitch $ viaPitchL (1, 0, -2)

e__ = fromPitch $ viaPitchL (2, 0, -2)

f__ = fromPitch $ viaPitchL (3, 0, -2)

g__ = fromPitch $ viaPitchL (4, 0, -2)

a__ = fromPitch $ viaPitchL (5, 0, -2)

b__ = fromPitch $ viaPitchL (6, 0, -2)

cb__ = fromPitch $ viaPitchL (0, (-1), -2)

db__ = fromPitch $ viaPitchL (1, (-1), -2)

eb__ = fromPitch $ viaPitchL (2, (-1), -2)

fb__ = fromPitch $ viaPitchL (3, (-1), -2)

gb__ = fromPitch $ viaPitchL (4, (-1), -2)

ab__ = fromPitch $ viaPitchL (5, (-1), -2)

bb__ = fromPitch $ viaPitchL (6, (-1), -2)

cs___ = fromPitch $ viaPitchL (0, 1, -3)

ds___ = fromPitch $ viaPitchL (1, 1, -3)

es___ = fromPitch $ viaPitchL (2, 1, -3)

fs___ = fromPitch $ viaPitchL (3, 1, -3)

gs___ = fromPitch $ viaPitchL (4, 1, -3)

as___ = fromPitch $ viaPitchL (5, 1, -3)

bs___ = fromPitch $ viaPitchL (6, 1, -3)

c___ = fromPitch $ viaPitchL (0, 0, -3)

d___ = fromPitch $ viaPitchL (1, 0, -3)

e___ = fromPitch $ viaPitchL (2, 0, -3)

f___ = fromPitch $ viaPitchL (3, 0, -3)

g___ = fromPitch $ viaPitchL (4, 0, -3)

a___ = fromPitch $ viaPitchL (5, 0, -3)

b___ = fromPitch $ viaPitchL (6, 0, -3)

cb___ = fromPitch $ viaPitchL (0, (-1), -3)

db___ = fromPitch $ viaPitchL (1, (-1), -3)

eb___ = fromPitch $ viaPitchL (2, (-1), -3)

fb___ = fromPitch $ viaPitchL (3, (-1), -3)

gb___ = fromPitch $ viaPitchL (4, (-1), -3)

ab___ = fromPitch $ viaPitchL (5, (-1), -3)

bb___ = fromPitch $ viaPitchL (6, (-1), -3)

cs____ = fromPitch $ viaPitchL (0, 1, -4)

ds____ = fromPitch $ viaPitchL (1, 1, -4)

es____ = fromPitch $ viaPitchL (2, 1, -4)

fs____ = fromPitch $ viaPitchL (3, 1, -4)

gs____ = fromPitch $ viaPitchL (4, 1, -4)

as____ = fromPitch $ viaPitchL (5, 1, -4)

bs____ = fromPitch $ viaPitchL (6, 1, -4)

c____ = fromPitch $ viaPitchL (0, 0, -4)

d____ = fromPitch $ viaPitchL (1, 0, -4)

e____ = fromPitch $ viaPitchL (2, 0, -4)

f____ = fromPitch $ viaPitchL (3, 0, -4)

g____ = fromPitch $ viaPitchL (4, 0, -4)

a____ = fromPitch $ viaPitchL (5, 0, -4)

b____ = fromPitch $ viaPitchL (6, 0, -4)

cb____ = fromPitch $ viaPitchL (0, (-1), -4)

db____ = fromPitch $ viaPitchL (1, (-1), -4)

eb____ = fromPitch $ viaPitchL (2, (-1), -4)

fb____ = fromPitch $ viaPitchL (3, (-1), -4)

gb____ = fromPitch $ viaPitchL (4, (-1), -4)

ab____ = fromPitch $ viaPitchL (5, (-1), -4)

bb____ = fromPitch $ viaPitchL (6, (-1), -4)


class IsInterval a where
  fromInterval :: Interval -> a

instance IsInterval Interval where
  fromInterval = id

instance IsInterval a => IsInterval (Maybe a) where
  fromInterval = pure . fromInterval

instance IsInterval a => IsInterval (First a) where
  fromInterval = pure . fromInterval

instance IsInterval a => IsInterval (Last a) where
  fromInterval = pure . fromInterval

instance IsInterval a => IsInterval [a] where
  fromInterval = pure . fromInterval

instance (Monoid b, IsInterval a) => IsInterval (b, a) where
  fromInterval = pure . fromInterval

deriving instance (Monoid b, IsInterval a) => IsInterval (Couple b a)

instance IsInterval Int where
  fromInterval x = fromIntegral (fromInterval x :: Integer)

instance IsInterval Word where
  fromInterval x = fromIntegral (fromInterval x :: Integer)

instance IsInterval Integer where
  fromInterval (Interval (ChromaticSteps c, _d)) = c

-- |
-- @
-- (octaves, diatonic steps, chromatic steps)
-- @
newtype IntervalL = IntervalL (Integer, Integer, Integer)

-- TODO factor out this and IntervalL
fromIntervalL :: IsInterval a => IntervalL -> a
fromIntervalL = fromInterval . go
  where
    go :: IntervalL -> Interval
    go (IntervalL (o, d, c)) = (_P8 ^* o) ^+^ (_A1 ^* c) ^+^ (d2 ^* d)






d1 = fromIntervalL $ IntervalL (0, 0, -1)

_P1 = fromInterval $ Interval (0, 0)

_A1 = fromInterval $ Interval (1, 0)

d2 = fromInterval $ Interval (0, 1)

m2 = fromIntervalL $ IntervalL (0, 1, 1)

_M2 = fromIntervalL $ IntervalL (0, 1, 2)

_A2 = fromIntervalL $ IntervalL (0, 1, 3)

d3 = fromIntervalL $ IntervalL (0, 2, 2)

m3 = fromIntervalL $ IntervalL (0, 2, 3)

_M3 = fromIntervalL $ IntervalL (0, 2, 4)

_A3 = fromIntervalL $ IntervalL (0, 2, 5)

d4 = fromIntervalL $ IntervalL (0, 3, 4)

_P4 = fromIntervalL $ IntervalL (0, 3, 5)

_A4 = fromIntervalL $ IntervalL (0, 3, 6)

d5 = fromIntervalL $ IntervalL (0, 4, 6)

_P5 = fromInterval $ Interval (7, 4)

_A5 = fromIntervalL $ IntervalL (0, 4, 8)

d6 = fromIntervalL $ IntervalL (0, 5, 7)

m6 = fromIntervalL $ IntervalL (0, 5, 8)

_M6 = fromIntervalL $ IntervalL (0, 5, 9)

_A6 = fromIntervalL $ IntervalL (0, 5, 10)

d7 = fromIntervalL $ IntervalL (0, 6, 9)

m7 = fromIntervalL $ IntervalL (0, 6, 10)

_M7 = fromIntervalL $ IntervalL (0, 6, 11)

_A7 = fromIntervalL $ IntervalL (0, 6, 12)

d8 = fromIntervalL $ IntervalL (1, 0, -1)

_P8 = fromInterval $ Interval (12, 7)

_A8 = fromIntervalL $ IntervalL (1, 0, 1)

d9 = fromIntervalL $ IntervalL (1, 1, 0)

m9 = fromIntervalL $ IntervalL (1, 1, 1)

_M9 = fromIntervalL $ IntervalL (1, 1, 2)

_A9 = fromIntervalL $ IntervalL (1, 1, 3)

d10 = fromIntervalL $ IntervalL (1, 2, 2)

m10 = fromIntervalL $ IntervalL (1, 2, 3)

_M10 = fromIntervalL $ IntervalL (1, 2, 4)

_A10 = fromIntervalL $ IntervalL (1, 2, 5)

d11 = fromIntervalL $ IntervalL (1, 3, 4)

_P11 = fromIntervalL $ IntervalL (1, 3, 5)

_A11 = fromIntervalL $ IntervalL (1, 3, 6)

d12 = fromIntervalL $ IntervalL (1, 4, 6)

_P12 = fromIntervalL $ IntervalL (1, 4, 7)

_A12 = fromIntervalL $ IntervalL (1, 4, 8)

d13 = fromIntervalL $ IntervalL (1, 5, 7)

m13 = fromIntervalL $ IntervalL (1, 5, 8)

_M13 = fromIntervalL $ IntervalL (1, 5, 9)

_A13 = fromIntervalL $ IntervalL (1, 5, 10)

d14 = fromIntervalL $ IntervalL (1, 6, 9)

m14 = fromIntervalL $ IntervalL (1, 6, 10)

_M14 = fromIntervalL $ IntervalL (1, 6, 11)

_A14 = fromIntervalL $ IntervalL (1, 6, 12)

d15 = fromIntervalL $ IntervalL (2, 0, -1)

_P15 = fromIntervalL $ IntervalL (2, 0, 0)

_A15 = fromIntervalL $ IntervalL (2, 0, 1)
