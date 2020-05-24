{-# LANGUAGE OverloadedLists #-}

-- | Scales and chords.
--
-- Semantically is little distinction between a Scale and a Chord. Thus the 'Chord' and 'Scale'
-- types are synonyms. We use a newtype wrapper to get a more idiomatic 'Inspectable' instance.
--
-- Semantically @Chord p@ and @Scale p@ are countable subsets of some pitch space p.
--
-- A 'Mode' (or 'Function - TODO rename) is like a Chord/Scale that has forgotten its origin.
module Music.Pitch.Scale
  ( -- * Modes and Chord types
    Mode,
    modeFromSteps,
    modeIntervals,
    Function,
    functionFromSteps,
    functionIntervals,

    -- * Scales
    Scale,
    scaleTonic,
    scaleMode,
    leadingInterval,
    invertMode,
    modeToScale,
    scaleToSet,
    index,
    member,
    scaleToList,

    -- * Chords
    Chord,
    chordTonic,
    chordFunction,
    complementInterval,
    invertChord,
    functionToChord,
    chordToList,

    -- * Common modes, scales and chords

    -- ** Common practice modes
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

    -- ** Common practice chord types
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
    quartal,
    quintal,
  )
where

import Control.Lens (Lens, Lens', coerced)
import Data.AffineSpace
import Data.AffineSpace.Point.Offsets
  ( offsetPoints,
    offsetPointsS,
  )
import Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Stream.Infinite (Stream)
import qualified Data.Stream.Infinite as Stream
import Data.VectorSpace
import Music.Pitch.Common hiding (Mode)
import Music.Pitch.Literal
import Music.Pitch.Literal

-- |  A mode is a list of intervals and a characteristic repeating interval.
data Mode a = Mode {getMode :: NonEmpty (Diff a)}

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

index :: AffineSpace p => Scale p -> Integer -> p
index s n = case fromIntegral n of
  n
    | n > 0 -> pos Stream.!! (n - 1)
    | n == 0 -> z
    | n < 0 -> neg Stream.!! negate (n + 1)
  where
    (neg, z, pos) = scaleToSet s

member :: (Ord p, AffineSpace p) => Scale p -> p -> Bool
member s p = case p of
  p
    | p > z -> p `isHeadOf` Stream.dropWhile (< p) pos
    | p == z -> True
    | p < z -> p `isHeadOf` Stream.dropWhile (> p) neg
  where
    (neg, z, pos) = scaleToSet s

isHeadOf :: Eq a => a -> Stream a -> Bool
isHeadOf a (b Stream.:> _) = a == b

invertMode :: AffineSpace a => Integer -> Mode a -> Mode a
invertMode 0 = id
invertMode n = invertMode (n - 1) . invertMode1
  where
    invertMode1 :: AffineSpace a => Mode a -> Mode a
    invertMode1 = Mode . rotate . getMode

-- TODO move
rotate :: NonEmpty a -> NonEmpty a
rotate (x :| []) = x :| []
rotate (x :| y : rs) = y :| (rs ++ [x])

-- TODO semantically suspect!
scaleToList :: AffineSpace a => Scale a -> [a]
scaleToList (Scale tonic (Mode leaps)) = init $ offsetPoints tonic $ toList leaps

-- | Convert a scale to a countably infinite set (represented as a tuple
-- of "negative", "zero" and "positive" components.
scaleToSet :: AffineSpace a => Scale a -> (Stream a, a, Stream a)
scaleToSet (Scale tonic (Mode leaps)) =
  ( Stream.tail $ offsetPointsS tonic $ fmap negateV $ Stream.cycle $ NonEmpty.reverse leaps,
    tonic,
    Stream.tail $ offsetPointsS tonic $ Stream.cycle leaps
  )

-- TODO rename to ChordType or similar
type Function a = Mode a

-- |
-- > [Interval] -> Interval -> Function Pitch
functionFromSteps :: NonEmpty (Diff a) -> Function a
functionFromSteps = modeFromSteps

-- |
-- > Lens' (Function Pitch) [Interval]
functionIntervals :: Lens' (Function a) (NonEmpty (Diff a))
functionIntervals = modeIntervals

-- Note: The only difference between a chord and a Scale is the Inspectable instance
newtype Chord a = Chord {getChord :: Scale a}

functionToChord :: AffineSpace a => a -> Function a -> Chord a
functionToChord x xs = Chord $ modeToScale x xs

-- |
-- > Lens' (Chord Pitch) Pitch
chordTonic :: Lens' (Chord a) a
chordTonic = coerced . scaleTonic

-- |
-- > Lens' (Chord Pitch) (Function Pitch)
chordFunction :: Lens' (Chord a) (Function a)
chordFunction = coerced . scaleMode

-- |
--
-- >>> complementInterval majorTriad
-- _P4
-- >>> complementInterval minorTriad
-- _P4
-- >>> complementInterval majorMinorSeventhChord
-- _M2
--
-- > Lens' (Function Pitch) Interval
complementInterval :: AffineSpace a => Function a -> Diff a
complementInterval = leadingInterval

invertChord :: AffineSpace a => Integer -> Function a -> Function a
invertChord = invertMode

{-# DEPRECATED chordToList "TODO alternative?" #-}

-- | Returns a single inversion of the given chord (no repeats!).
chordToList :: AffineSpace a => Chord a -> [a]
chordToList = scaleToList . getChord

-- Common scales

majorScale :: Mode Pitch
majorScale = Mode [_M2, _M2, m2, _M2, _M2, _M2, m2]

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

majorTriad :: Function Pitch
majorTriad = Mode [_M3, m3, _P4]

minorTriad :: Function Pitch
minorTriad = Mode [m3, _M3, _P4]

augmentedChord :: Function Pitch
augmentedChord = Mode [_M3, _M3, _M3]

diminishedChord :: Function Pitch
diminishedChord = Mode [m3, m3, m3, m3]

halfDiminishedChord :: Function Pitch
halfDiminishedChord = Mode [m3, m3, _M3, _M2]

-- | Also known as "dominant seventh".
majorMinorSeventhChord :: Function Pitch
majorMinorSeventhChord = Mode [_M3, m3, m3, _M2]

-- | Also known as "major seventh".
majorMajorSeventhChord :: Function Pitch
majorMajorSeventhChord = Mode [_M3, m3, _M3, m2]

-- | Also known as a "ii7". First inversion of major sixth.
minorMinorSeventhChord :: Function Pitch
minorMinorSeventhChord = Mode [m3, _M3, m3, _M2]

-- | Also known as "major seventh".
minorMajorSeventhChord :: Function Pitch
minorMajorSeventhChord = Mode [m3, _M3, _M3, m2]

-- TODO ninth chords, sixths chords, 6/9 (pentatonic field), Guido's hexachord?

germanSixthChord :: Function Pitch
germanSixthChord = majorMinorSeventhChord

frenchSixthChord :: Function Pitch
frenchSixthChord = Mode [_M3, d3, _M3, _M2]

-- TODO generalize this to "repeating"
--
-- repeating _m2 = chromaticCluster
-- repeating M2  = wholeToneCluster
-- repeating m3  = diminishedChord
-- repeating P4  = quartal
repeating x = Mode (x NonEmpty.:| [])

-- etc.

quartal :: Function Pitch
quartal = repeating _P4

quintal :: Function Pitch
quintal = repeating _P5
