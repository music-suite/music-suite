{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-
 - TODO get rid of UndecidableInstances by tracking both pitch and interval in the type
 - params of Mode/Scale etc.
 -}
{-# LANGUAGE UndecidableInstances #-}

-- | Scales and chords.
--
-- Semantically is little distinction between a Scale and a Chord. Thus the 'Chord' and 'Scale'
-- types are synonyms. We use a newtype wrapper to get a more idiomatic 'Inspectable' instance.
--
-- Semantically @Chord p@ and @Scale p@ are countable subsets of some pitch space p.
--
-- A 'Mode' (or 'ChordType) is like a Chord/Scale that has forgotten its origin.
--
module Music.Pitch.Scale
  (
    -- * TODO move
    Countable(..),
    Generated(..),

    -- * Modes and Chord types
    Mode(..),
    modeFromSteps,
    modeIntervals,

    ChordType,
    functionFromSteps,
    functionIntervals,

    -- * Scales
    Scale,
    scaleTonic,
    scaleMode,

    leadingInterval,
    invertMode,
    modeToScale,
    scaleToList,

    -- * Chords
    Chord,
    chordTonic,
    chordType,

    complementInterval,
    invertChord,
    functionToChord,
    chordToList,



    -- * Common modes, scales and chords

    -- ** Major-Minor/Common practice modes
    majorScale,
    pureMinorScale,
    harmonicMinorScale,
    melodicMinorScaleUp,

    -- ** Church/Gregorian modes
    aeolian,
    locrian,
    ionian,
    dorian,
    phrygian,
    lydian,
    mixolydian,

    -- ** Other modes
    majorPentaTonic,
    minorPentaTonic,
    bluesMinor,
    bluesMajor,
    bebopScale,
    wholeTone,
    octatonic,

    -- ** Modes of limited transposition
    firstMode,
    secondMode,
    thirdMode,
    fourthMode,
    fifthMode,
    sixthMode,
    seventhMode,

    -- ** Triadic/Common practice chord types
    majorTriad,
    minorTriad,
    augmentedChord,
    diminishedChord,
    halfDiminishedChord,
    majorMinorSeventhChord,
    majorMajorSeventhChord,
    minorMinorSeventhChord,
    minorMajorSeventhChord,
    frenchSixthChord,
    germanSixthChord,

    -- ** Non-triadic chord types
    repeating,
    chromaticCluster,
    wholeToneCluster,
    quartal,
    quintal,

    -- * Voiced chords/scales
    Voiced(..),
    getVoiced,
    basicVoicing,
  )
where

import Data.Foldable
import Data.Stream.Infinite (Stream)
import qualified Data.Stream.Infinite as Stream
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Control.Lens (Lens, Lens', coerced)
import Data.AffineSpace
import Data.VectorSpace
import Music.Pitch.Common hiding (Mode)
import Music.Pitch.Literal
import Music.Pitch.Literal
import Data.AffineSpace.Point.Offsets
  ( offsetPoints, offsetPointsS )

-- |  A mode is a list of intervals and a characteristic repeating interval.
data Mode a = Mode { getMode :: NonEmpty (Diff a) }

deriving instance Eq (Diff a) => Eq (Mode a)
deriving instance Ord (Diff a) => Ord (Mode a)
deriving instance Show (Diff a) => Show (Mode a)

-- |
-- @
-- majorScale = modeFromSteps [M2,M2,m3,M2,M2,M2,m3]
--
-- > [Interval] -> Mode Pitch
modeFromSteps :: NonEmpty (Diff a) -> Mode a
modeFromSteps = Mode

-- |
-- > Lens' (Mode Pitch) [Interval]
modeIntervals :: Lens' (Mode a) (NonEmpty (Diff a))
modeIntervals f (Mode is) = fmap (\is -> Mode is) $ f is

-- |  A scale is a mode with a specified tonic.
data Scale a = Scale a (Mode a) -- root, mode

deriving instance (Eq a, Eq (Diff a)) => Eq (Scale a)
deriving instance (Ord a, Ord (Diff a)) => Ord (Scale a)
deriving instance (Show a, Show (Diff a)) => Show (Scale a)

modeToScale :: AffineSpace a => a -> Mode a -> Scale a
modeToScale = Scale

-- |
-- > Lens' (Scale Pitch) Pitch
scaleTonic :: Lens' (Scale a) a
scaleTonic f (Scale t xs) = fmap (\t -> Scale t xs) $ f t

-- |
-- > Lens' (Scale Pitch) (Mode Pitch)
scaleMode :: Lens' (Scale a) (Mode a)
scaleMode f (Scale t xs) = fmap (\xs -> Scale t xs) $ f xs


-- |
--
-- >>> repeatingInterval majorScale
-- _P8
repeatingInterval :: AffineSpace a => Mode a -> Diff a
repeatingInterval (Mode xs) = sumV xs

-- |
--
-- >>> leadingInterval majorScale
-- m2
-- >>> leadingInterval harmonicMinorScale
-- m2
-- >>> leadingInterval pureMinorScale
-- _M2
leadingInterval :: AffineSpace a => Mode a -> Diff a
leadingInterval (Mode xs) = NonEmpty.last xs


isHeadOf :: Eq a => a -> Stream a -> Bool
isHeadOf a (b Stream.:> _) = a == b

-- |
-- @
-- length (generator x) == n -> invertMode n x = x
-- @
invertMode :: AffineSpace a => Integer -> Mode a -> Mode a
invertMode 0 = id
invertMode n = invertMode (n - 1) . invertMode1
  where
    invertMode1 :: AffineSpace a => Mode a -> Mode a
    invertMode1 = Mode . rotate . getMode

-- TODO move
rotate :: NonEmpty a -> NonEmpty a
rotate (x :| [])     = x :| []
rotate (x :| y : rs) = y :| (rs ++ [x])

-- TODO semantically suspect!
scaleToList :: AffineSpace a => Scale a -> [a]
scaleToList (Scale tonic (Mode leaps)) = init $ offsetPoints tonic $ toList leaps

-- TODO lawless class. Also add rotate and other Mode/ChordType ops to here?
class Generated f where
  -- |
  -- Returns the interval sequenc that generates the given mode.
  --
  -- The length of this sequence gives the number of notes in the scale,
  -- e.g. for a pentatonic (5-note) scale @s@, @length (generator s) == 5@,
  -- for a heptatonic (7-note) scale it is 7, and so on.
  generator :: f a -> NonEmpty (Diff a)

instance Generated Chord where
  generator (Chord (Scale _ (Mode x))) = x

instance Generated Scale where
  generator (Scale _ (Mode x)) = x


class Countable f where
  -- | Convert to a countably infinite set (represented as a tuple
  -- of "negative", "zero" and "positive" components.
  scaleToSet :: AffineSpace a => f a -> (Stream a, a, Stream a)

  index :: AffineSpace p => f p -> Integer -> p
  member :: (Ord p, AffineSpace p) => f p -> p -> Bool

  index s n = case fromIntegral n of
    n | n >  0 -> pos Stream.!! (n - 1)
      | n == 0 -> z
      | n <  0 -> neg Stream.!! negate (n + 1)
    where
      (neg, z, pos) = scaleToSet s

  member s p = case p of
    p | p >  z -> p `isHeadOf` Stream.dropWhile (< p) pos
      | p == z -> True
      | p <  z -> p `isHeadOf` Stream.dropWhile (> p) neg
    where
      (neg, z, pos) = scaleToSet s

instance Countable Scale where
  scaleToSet :: AffineSpace a => Scale a -> (Stream a, a, Stream a)
  scaleToSet (Scale tonic (Mode leaps)) =
    ( Stream.tail $ offsetPointsS tonic $ fmap negateV $ Stream.cycle $ NonEmpty.reverse leaps
    , tonic
    , Stream.tail $ offsetPointsS tonic $ Stream.cycle leaps
    )

instance Countable Chord where
  scaleToSet = scaleToSet . getChord

-- TODO rename to ChordType or similar
type ChordType a = Mode a

-- |
-- > [Interval] -> Interval -> ChordType Pitch
functionFromSteps :: NonEmpty (Diff a) -> ChordType a
functionFromSteps = modeFromSteps

-- |
-- > Lens' (ChordType Pitch) [Interval]
functionIntervals :: Lens' (ChordType a) (NonEmpty (Diff a))
functionIntervals = modeIntervals



-- Note: The only difference between a chord and a Scale is the Inspectable instance
newtype Chord a = Chord { getChord :: Scale a }

deriving instance (Eq a, Eq (Diff a)) => Eq (Chord a)
deriving instance (Ord a, Ord (Diff a)) => Ord (Chord a)
deriving instance (Show a, Show (Diff a)) => Show (Chord a)

functionToChord :: AffineSpace a => a -> ChordType a -> Chord a
functionToChord x xs = Chord $ modeToScale x xs

-- |
-- > Lens' (Chord Pitch) Pitch
chordTonic :: Lens' (Chord a) a
chordTonic = coerced . scaleTonic

-- |
-- > Lens' (Chord Pitch) (ChordType Pitch)
chordType :: Lens' (Chord a) (ChordType a)
chordType = coerced . scaleMode

-- |
--
-- >>> complementInterval majorTriad
-- _P4
-- >>> complementInterval minorTriad
-- _P4
-- >>> complementInterval majorMinorSeventhChord
-- _M2
--
-- > Lens' (ChordType Pitch) Interval
complementInterval :: AffineSpace a => ChordType a -> Diff a
complementInterval = leadingInterval

invertChord :: AffineSpace a => Integer -> ChordType a -> ChordType a
invertChord = invertMode

{-# DEPRECATED chordToList "Use (getVoiced . basicVoicing) instead" #-}
{-# DEPRECATED scaleToList "Use (getVoiced . basicVoicing) instead" #-}
-- | Returns a single inversion of the given chord (no repeats!).
chordToList :: AffineSpace a => Chord a -> [a]
chordToList = scaleToList . getChord



-- Common scales

majorScale :: Mode Pitch
majorScale = Mode [_M2, _M2, m2, _M2, _M2, _M2, m2]

-- |
-- @
-- pureMinorScale = invertMode 5 majorScale
-- majorScale     = invertMode 2 pureMinorScale
-- @
pureMinorScale :: Mode Pitch
pureMinorScale = Mode [_M2, m2, _M2, _M2, m2, _M2, _M2]

harmonicMinorScale :: Mode Pitch
harmonicMinorScale = Mode [_M2, m2, _M2, _M2, m2, _A2, m2]

melodicMinorScaleUp :: Mode Pitch
melodicMinorScaleUp = Mode [_M2, m2, _M2, _M2, _M2, _M2, m2]

ionian :: Mode Pitch
ionian = invertMode 0 majorScale

dorian :: Mode Pitch
dorian = invertMode 1 majorScale

phrygian :: Mode Pitch
phrygian = invertMode 2 majorScale

lydian :: Mode Pitch
lydian = invertMode 3 majorScale

mixolydian :: Mode Pitch
mixolydian = invertMode 4 majorScale

aeolian :: Mode Pitch
aeolian = invertMode 5 majorScale

locrian :: Mode Pitch
locrian = invertMode 6 majorScale

majorPentaTonic :: Mode Pitch
majorPentaTonic = Mode [_M2, _M2, m3, _M2, m3]

bluesMinor :: Mode Pitch
bluesMinor = invertMode 2 majorPentaTonic

bluesMajor :: Mode Pitch
bluesMajor = invertMode 3 majorPentaTonic

minorPentaTonic :: Mode Pitch
minorPentaTonic = invertMode 4 majorPentaTonic

-- a.k.a. "bebop major"
bebopScale :: Mode Pitch
bebopScale = Mode [_M2, _M2, m2, _M2, m2, m2, _M2, m2]

wholeTone :: Mode Pitch
wholeTone = firstMode

octatonic :: Mode Pitch
octatonic = secondMode

firstMode :: Mode Pitch
firstMode = Mode [_M2, _M2, _M2, _M2, _M2, _M2]

secondMode :: Mode Pitch
secondMode = Mode [m2, _M2, _A1, _M2, m2, _M2, m2, _M2]

thirdMode :: Mode Pitch
thirdMode = Mode [_M2, m2, m2, _M2, m2, m2, _M2, m2, m2]

fourthMode :: Mode Pitch
fourthMode = Mode [m2, m2, m3, m2, m2, m2, m3, m2]

fifthMode :: Mode Pitch
fifthMode = Mode [m2, _M3, m2, m2, _M3, m2]

sixthMode :: Mode Pitch
sixthMode = Mode [_M2, _M2, m2, m2, _M2, _M2, m2, m2]

seventhMode :: Mode Pitch
seventhMode = Mode [m2, m2, m2, _M2, m2, m2, m2, m2, _M2, m2]

-- Common chords

majorTriad :: ChordType Pitch
majorTriad = Mode [_M3, m3, _P4]

minorTriad :: ChordType Pitch
minorTriad = Mode [m3, _M3, _P4]

augmentedChord :: ChordType Pitch
augmentedChord = Mode [_M3, _M3, _M3]

diminishedChord :: ChordType Pitch
diminishedChord = Mode [m3, m3, m3, m3]

halfDiminishedChord :: ChordType Pitch
halfDiminishedChord = Mode [m3, m3, _M3, _M2]

-- | Also known as "dominant seventh".
majorMinorSeventhChord :: ChordType Pitch
majorMinorSeventhChord = Mode [_M3, m3, m3, _M2]

-- | Also known as "major seventh".
majorMajorSeventhChord :: ChordType Pitch
majorMajorSeventhChord = Mode [_M3, m3, _M3, m2]

-- | Also known as a "ii7". First inversion of major sixth.
minorMinorSeventhChord :: ChordType Pitch
minorMinorSeventhChord = Mode [m3, _M3, m3, _M2]

-- | Also known as "major seventh".
minorMajorSeventhChord :: ChordType Pitch
minorMajorSeventhChord = Mode [m3, _M3, _M3, m2]

-- TODO ninth chords, sixths chords, 6/9 (pentatonic field), Guido's hexachord?

germanSixthChord :: ChordType Pitch
germanSixthChord = majorMinorSeventhChord

frenchSixthChord :: ChordType Pitch
frenchSixthChord = Mode [_M3, d3, _M3, _M2]


-- | Build a harmonic filed from repeating a single interval.
--
-- Morally, we have:
--
-- @
-- repeating _m2 = chromaticCluster
-- repeating M2  = wholeToneCluster
-- repeating m3  = diminishedChord
-- repeating P4  = quartal
-- @
repeating :: Diff p -> ChordType p
repeating x = Mode (x NonEmpty.:| [])

chromaticCluster :: ChordType Pitch
chromaticCluster = repeating m2

wholeToneCluster :: ChordType Pitch
wholeToneCluster = repeating _M2

quartal :: ChordType Pitch
quartal = repeating _P4

quintal :: ChordType Pitch
quintal = repeating _P5


data Voiced f p = Voiced { getChordScale :: f p, getSteps :: NonEmpty Integer }

deriving instance Eq (f a) => Eq (Voiced f a)
deriving instance Ord (f a) => Ord (Voiced f a)
deriving instance Show (f a) => Show (Voiced f a)


getVoiced :: (AffineSpace p, Countable f) => Voiced f p -> NonEmpty p
getVoiced x = index (getChordScale x) <$> getSteps x

basicVoicing :: Generated f => f p -> Voiced f p
basicVoicing x = Voiced x [0..fromIntegral (length (generator x)) - 1]


