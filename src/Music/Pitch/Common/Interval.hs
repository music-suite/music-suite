-- | Common intervals.
module Music.Pitch.Common.Interval
  ( -- * Intervals
    Interval,

    -- ** Creating intervals
    interval,
    interval',
    interval'',
    _number,
    _quality,
    _steps,
    _alteration,

    -- ** Synonyms
    perfect,
    major,
    minor,
    augmented,
    diminished,
    doublyAugmented,
    doublyDiminished,

    -- ** Inspecting intervals
    isNegative,
    isPositive,
    isNonNegative,
    isPerfectUnison,
    isStep,
    isLeap,

    -- ** Simple and compound intervals
    isSimple,
    isCompound,
    separate,
    simple,
    octaves,

    -- *** Inversion
    invert,

    -- * Basis values
    IntervalBasis (..),

    -- ** Converting basis
    convertBasis,
    convertBasisFloat,
    intervalDiv,
  )
where

import Control.Applicative
import Control.Lens hiding (simple)
import Control.Monad
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson
import Data.AffineSpace.Point (relative)
import Data.Basis
import Data.Either
import qualified Data.List as List
import Data.Maybe
import Data.Semigroup
import Data.Typeable
import Data.VectorSpace
import Music.Pitch.Absolute
import Music.Pitch.Augmentable
import Music.Pitch.Common.Number
import Music.Pitch.Common.Quality
import Music.Pitch.Common.Semitones
import Music.Pitch.Common.Types
import Music.Pitch.Literal
import Numeric.Positive

-- | Avoid using '(*)', or 'signum' on intervals.
instance Num Interval where

  (+) = (^+^)

  negate = negateV

  abs a = if isNegative a then negate a else a

  (*) = error "Music.Pitch.Common.Interval: no overloading for (*)"

  signum = error "Music.Pitch.Common.Interval: no overloading for signum"

  fromInteger = error "Music.Pitch.Common.Interval: no overloading for fromInteger"

instance Show Interval where
  show a
    | isNegative a = "-" ++ showQuality (extractQuality a) ++ show (abs $ extractNumber a)
    | otherwise = showQuality (extractQuality a) ++ show (abs $ extractNumber a)
    where
      showQuality Major = "_M"
      showQuality Minor = "m"
      showQuality Perfect = "_P"
      showQuality (Augmented n) = "_" ++ replicate (fromIntegral n) 'A'
      showQuality (Diminished n) = replicate (fromIntegral n) 'd'

instance Semigroup Interval where
  (<>) = (^+^)

instance Monoid Interval where

  mempty = basis_P1

  mappend = (^+^)

instance VectorSpace Interval where

  type Scalar Interval = Integer

  (*^) = stackInterval
    where
      stackInterval n a
        | n >= 0 = mconcat $ replicate (fromIntegral n) a
        | otherwise = negate $ stackInterval (negate n) a

instance HasBasis Interval where

  type Basis Interval = IntervalBasis

  basisValue Chromatic = basis_A1
  basisValue Diatonic = basis_d2

  decompose (Interval (c, d)) = [(Chromatic, fromIntegral c), (Diatonic, fromIntegral d)]

  decompose' (Interval (c, d)) Chromatic = fromIntegral c
  decompose' (Interval (c, d)) Diatonic = fromIntegral d

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
  fromInterval (IntervalL (o, d, c)) = (basis_P8 ^* o) ^+^ (basis_A1 ^* c) ^+^ (basis_d2 ^* d)

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
  parseJSON (Data.Aeson.Object x) = liftA2 (curry (^. interval')) alteration steps
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

basis_P1 = Interval (0, 0)

basis_A1 = Interval (1, 0)

basis_d2 = Interval (0, 1)

basis_P5 = Interval (7, 4)

basis_P8 = Interval (12, 7)

-- |
-- Extracting the 'number' from an interval vector.
--
-- Note that (a, d) is a representation of the interval (a * A1) + (d
-- * d2), so the 'number' part of the interval must be stored entirely
-- in the d * d2 part (adding a unison, perfect or otherwise, can
-- never increase the number of the interval)
extractNumber :: Interval -> Number
extractNumber (Interval (a, d))
  | d >= 0 = fromIntegral (d + 1)
  | otherwise = fromIntegral (d - 1)

-- |
-- Extracting the 'quality' from an interval vector.
--
-- This is much more finicky, as the A1 and d2 intervals interact in a
-- complex way to produce the perfect/major/minor/etc. intervals that
-- we are used to reading.
extractQuality :: Interval -> Quality
extractQuality (Interval (a, d))
  | (a < 0) && (d == 0) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (0, 0) = Perfect
  | (a > 0) && (d == 0) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 1) && (d == 1) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (1, 1) = Minor
  | (a, d) == (2, 1) = Major
  | (a > 2) && (d == 1) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 3) && (d == 2) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (3, 2) = Minor
  | (a, d) == (4, 2) = Major
  | (a > 4) && (d == 2) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 5) && (d == 3) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (5, 3) = Perfect
  | (a > 5) && (d == 3) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 7) && (d == 4) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (7, 4) = Perfect
  | (a > 7) && (d == 4) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 8) && (d == 5) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (8, 5) = Minor
  | (a, d) == (9, 5) = Major
  | (a > 9) && (d == 5) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 10) && (d == 6) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (10, 6) = Minor
  | (a, d) == (11, 6) = Major
  | (a > 11) && (d == 6) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 12) && (d == 7) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (12, 7) = Perfect
  | (a > 12) && (d == 7) = augment $ extractQuality (Interval ((a - 1), d))
  -- note: these last two cases *have* to be this way round, otherwise
  -- infinite loop occurs.
  | (a > 12) || (d > 7) = extractQuality (Interval ((a - 12), (d - 7)))
  | (a < 0) || (d < 0) = extractQuality (Interval ((- a), (- d)))

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
augmented = mkInterval (Augmented 1)

-- | Creates a diminished interval.
diminished :: Number -> Interval
diminished = mkInterval (Diminished 1)

-- | Creates a doubly augmented interval.
doublyAugmented :: Number -> Interval
doublyAugmented = mkInterval (Augmented 2)

-- | Creates a doubly diminished interval.
doublyDiminished :: Number -> Interval
doublyDiminished = mkInterval (Diminished 2)

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
separate :: Interval -> (Octaves, Interval)
separate i = (fromIntegral o, i ^-^ (fromIntegral o *^ basis_P8))
  where
    o = octaves i

-- |
-- Returns the non-simple part of an interval.
--
-- > _P8^*octaves x ^+^ simple x = x
octaves :: Interval -> Octaves
octaves (Interval (_, d)) = fromIntegral $ d `div` 7

-- |
-- Returns the simple part of an interval.
--
-- > (perfect octave)^*x + y = z  iff  y = simple z
simple :: Interval -> Interval
simple = snd . separate

-- |
-- Returns whether the given interval is simple.
--
-- A simple interval is a non-negative interval spanning less than one octave.
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
-- Returns whether the given interval is negative.
isNegative :: Interval -> Bool
isNegative (Interval (a, d)) = d < 0

-- |
-- Returns whether the given interval is positive.
isPositive :: Interval -> Bool
isPositive x@(Interval (a, d)) = d >= 0 && not (isPerfectUnison x)

-- |
-- Returns whether the given interval is non-negative. This implies that it is either positive or a perfect unison.
isNonNegative :: Interval -> Bool
isNonNegative (Interval (a, d)) = d >= 0

-- |
-- Returns whether the given interval a perfect unison.
isPerfectUnison :: Interval -> Bool
isPerfectUnison (Interval (a, d)) = (a, d) == (0, 0)

-- |
-- Returns whether the given interval is a step (a second or smaller).
--
-- Only diatonic 'number' is taken into account, so @_A2@ is considered
-- a step and @m3@ a leap, even though they have the same number of
-- semitones.
isStep :: Interval -> Bool
isStep (Interval (a, d)) = (abs d) <= 1

-- |
-- Returns whether the given interval is a leap (larger than a second).
--
-- Only the diatonic 'number' is taken into account, so @_A2@ is considered
-- a step and @m3@ a leap, even though they have the same number of
-- semitones.
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
invert :: Interval -> Interval
invert = simple . negate

mkInterval :: Quality -> Number -> Interval
mkInterval q n = mkInterval' (fromIntegral diff) (fromIntegral steps)
  where
    diff = qualityToDiff (n > 0) (expectedQualityType n) (q)
    steps = case n `compare` 0 of
      GT -> n - 1
      EQ -> error "diatonicSteps: Invalid number 0"
      LT -> n + 1

-- steps = n^.diatonicSteps

-- | View or set the alteration (i.e. the number of chromatic steps differing from the excepted number) in an interval.
_alteration :: Lens' Interval ChromaticSteps
_alteration = from interval' . _1

-- | View or set the number of chromatic steps in an interval.
_steps :: Lens' Interval DiatonicSteps
_steps = from interval' . _2

-- | View or set the quality of an interval.
_quality :: Lens' Interval Quality
_quality = from interval . _1

-- | View or set the number component of an interval.
_number :: Lens' Interval Number
_number = from interval . _2

-- | View an interval as a pair of quality and number or vice versa.
interval :: Iso' (Quality, Number) Interval
interval = iso (uncurry mkInterval) (\x -> (quality x, number x))

-- | View an interval as a pair of alteration and diatonic steps or vice versa.
interval' :: Iso' (ChromaticSteps, DiatonicSteps) Interval
interval' =
  iso
    (\(d, s) -> mkInterval' (fromIntegral d) (fromIntegral s))
    (\x -> (qualityToDiff (number x >= 0) (expectedQualityType (number x)) (quality x), (number x) ^. diatonicSteps))

-- | View an interval as a pair of total number of chromatic and diatonic steps.
interval'' :: Iso' (ChromaticSteps, DiatonicSteps) Interval
interval'' = iso Interval getInterval

{-
Note: This is *not* the same as wrapping/unwrapping, as the number of chromatic steps viewed here is
an *alteration*, rather than the total number of chromatic steps (basis d2).

E.g. d5 is internally represented as (6,4) but _P5^.from interval' == (-1,4).
-}

-- |
-- >>> m3 & _number %~ pred
-- m2
-- >>> m3 & _number %~ succ
-- d4
-- >>> _M3 & _number %~ succ
-- _P4
--
--
-- >>> m3 & _number +~ 1
-- d4
-- >>> m3 & _number +~ 2
-- d5
-- >>> m3 & _number +~ 3
-- m6
-- >>> m3 & _number +~ 4
--
--
-- >>> m3 & _quality .~ Minor
-- m3
-- >>> _P5 & _quality .~ Minor
-- d5
-- >>> (-d5) & _quality %~ diminish
--
--
-- TODO only obeys lens laws up to quality normalization
--
-- >>> _P5 & _quality .~ Minor
-- d5
-- >>> _P5 & _quality .~ (Diminished 1)
-- d5

-- Internal stuff

-- TODO more generic pattern here
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
  | (i > basis_P1) = intervalDivPos i di
  | (i < basis_P1) = intervalDivNeg i di
  | otherwise = 0 :: Int
  where
    intervalDivPos i di
      | (i < basis_P1) = error "Impossible"
      | (i ^-^ di) < basis_P1 = 0
      | otherwise = 1 + (intervalDiv (i ^-^ di) di)
    intervalDivNeg i di
      | (i > basis_P1) = error "Impossible"
      | (i ^+^ di) > basis_P1 = 0
      | otherwise = 1 + (intervalDiv (i ^+^ di) di)

-- | Represent an interval i in a new basis (j, k).
--
-- We want x,y where i = x*j + y*k
--
-- e.g., convertBasis basis_d2 _P5 basis_P8 == Just (-12,7), as expected.
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
    Interval (fromIntegral -> c, fromIntegral -> d) = k
    p = fromIntegral $ (a * d - b * c)
    q = fromIntegral $ (a * n - b * m)
    r = fromIntegral $ (d * m - c * n)
