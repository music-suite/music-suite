-- | Scales and chords.
module Music.Pitch.Scale
  ( -- * Modes and scales
    Mode,
    modeFromSteps,
    modeIntervals,
    modeRepeat,
    Scale,
    scaleTonic,
    scaleMode,
    leadingInterval,
    invertMode,
    modeToScale,
    scaleToList,

    -- * Chord types and chords
    Function,
    functionFromSteps,
    functionIntervals,
    functionRepeat,
    Chord,
    chordTonic,
    chordFunction,
    complementInterval,
    invertChord,
    functionToChord,
    chordToList,

    -- * Common modes

    -- ** Classical modes
    majorScale,
    pureMinorScale,
    harmonicMinorScale,
    melodicMinorScaleUp,

    -- ** Church modes
    aeolian,
    locrian,
    ionian,
    dorian,
    phrygian,
    lydian,
    mixolydian,
    majorPentaTonic,
    minorPentaTonic,
    bluesMinor,
    bluesMajor,
    bebopScale,

    -- ** Miscellaneous modes
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

    -- * Common chords
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

import Control.Lens
import Data.AffineSpace
import Data.VectorSpace
import Music.Pitch.Common hiding (Mode)
import Music.Pitch.Literal
import Music.Pitch.Literal

-- |  A mode is a list of intervals and a characteristic repeating interval.
data Mode a = Mode [Diff a] (Diff a) -- intervals, repeat (usually octave)

-- |
-- > [Interval] -> Interval -> Mode Pitch
modeFromSteps :: [Diff a] -> Diff a -> Mode a
modeFromSteps = Mode

-- |
-- > Lens' (Mode Pitch) [Interval]
modeIntervals :: Lens' (Mode a) [Diff a]
modeIntervals f (Mode is r) = fmap (\is -> Mode is r) $ f is

-- |
-- > Lens' (Mode Pitch) Interval
modeRepeat :: Lens' (Mode a) (Diff a)
modeRepeat f (Mode is r) = fmap (\r -> Mode is r) $ f r

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
-- >>> leadingInterval majorScale
-- m2
-- >>> leadingInterval harmonicMinorScale
-- m2
-- >>> leadingInterval pureMinorScale
-- _M2
leadingInterval :: AffineSpace a => Mode a -> Diff a
leadingInterval (Mode steps repeating) = repeating ^-^ sumV steps

invertMode :: AffineSpace a => Int -> Mode a -> Mode a
invertMode 0 = id
invertMode n = invertMode (n -1) . invertMode1
  where
    invertMode1 :: AffineSpace a => Mode a -> Mode a
    invertMode1 mode@(Mode steps repeating) = Mode (tail steps ++ [leadingInterval mode]) repeating

scaleToList :: AffineSpace a => Scale a -> [a]
scaleToList (Scale tonic mode@(Mode steps repeating)) = offsetPoints tonic steps
  where
    -- TODO consolidate
    offsetPoints :: AffineSpace p => p -> [Diff p] -> [p]
    offsetPoints = scanl (.+^)

data Function a = Function [Diff a] (Diff a) -- intervals, repeat, repeat (usually octave)

-- |
-- > [Interval] -> Interval -> Function Pitch
functionFromSteps :: [Diff a] -> Diff a -> Function a
functionFromSteps = Function

-- |
-- > Lens' (Function Pitch) [Interval]
functionIntervals :: Lens' (Function a) [Diff a]
functionIntervals f (Function is r) = fmap (\is -> Function is r) $ f is

-- |
-- > Lens' (Function Pitch) Interval
functionRepeat :: Lens' (Function a) (Diff a)
functionRepeat f (Function is r) = fmap (\r -> Function is r) $ f r

data Chord a = Chord a (Function a) -- root, function

functionToChord :: AffineSpace a => a -> Function a -> Chord a
functionToChord = Chord

-- |
-- > Lens' (Chord Pitch) Pitch
chordTonic :: Lens' (Chord a) a
chordTonic f (Chord t xs) = fmap (\t -> Chord t xs) $ f t

-- |
-- > Lens' (Chord Pitch) (Function Pitch)
chordFunction :: Lens' (Chord a) (Function a)
chordFunction f (Chord t xs) = fmap (\xs -> Chord t xs) $ f xs

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
complementInterval (Function leaps repeating) = repeating ^-^ sumV leaps

invertChord :: AffineSpace a => Int -> Function a -> Function a
invertChord 0 = id
invertChord n = invertChord (n -1) . invertChord1
  where
    invertChord1 :: AffineSpace a => Function a -> Function a
    invertChord1 function@(Function leaps repeating) = Function (tail leaps ++ [complementInterval function]) repeating

-- | Returns a single inversion of the given chord (no repeats!).
chordToList :: AffineSpace a => Chord a -> [a]
chordToList (Chord tonic mode@(Function leaps repeating)) = offsetPoints tonic leaps
  where
    -- TODO inversion?

    -- TODO consolidate
    offsetPoints :: AffineSpace p => p -> [Diff p] -> [p]
    offsetPoints = scanl (.+^)

-- Common scales

majorScale :: Mode Pitch
majorScale = modeFromSteps [_M2, _M2, m2, _M2, _M2, _M2] _P8

pureMinorScale :: Mode Pitch
pureMinorScale = modeFromSteps [_M2, m2, _M2, _M2, m2, _M2] _P8

harmonicMinorScale :: Mode Pitch
harmonicMinorScale = modeFromSteps [_M2, m2, _M2, _M2, m2, _A2] _P8

melodicMinorScaleUp :: Mode Pitch
melodicMinorScaleUp = modeFromSteps [_M2, m2, _M2, _M2, _M2, _M2] _P8

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
majorPentaTonic = modeFromSteps [_M2, _M2, m3, _M2] _P8

bluesMinor :: Mode Pitch
bluesMinor = invertMode 2 majorPentaTonic

bluesMajor :: Mode Pitch
bluesMajor = invertMode 3 majorPentaTonic

minorPentaTonic :: Mode Pitch
minorPentaTonic = invertMode 4 majorPentaTonic

bebopScale :: Mode Pitch
bebopScale = modeFromSteps [_M2, _M2, m2, _M2, m2, m2, _M2] _P8

wholeTone :: Mode Pitch
wholeTone = firstMode

octatonic :: Mode Pitch
octatonic = secondMode

firstMode :: Mode Pitch
firstMode = modeFromSteps [_M2, _M2, _M2, _M2, _M2] _P8

secondMode :: Mode Pitch
secondMode = modeFromSteps [m2, _M2, _A1, _M2, m2, _M2, m2] _P8

thirdMode :: Mode Pitch
thirdMode = modeFromSteps [_M2, m2, m2, _M2, m2, m2, _M2, m2] _P8

fourthMode :: Mode Pitch
fourthMode = modeFromSteps [m2, m2, m3, m2, m2, m2, m3] _P8

fifthMode :: Mode Pitch
fifthMode = modeFromSteps [m2, _M3, m2, m2, _M3] _P8

sixthMode :: Mode Pitch
sixthMode = modeFromSteps [_M2, _M2, m2, m2, _M2, _M2, m2] _P8

seventhMode :: Mode Pitch
seventhMode = modeFromSteps [m2, m2, m2, _M2, m2, m2, m2, m2, _M2] _P8

-- Common chords

majorTriad :: Function Pitch
majorTriad = Function [_M3, m3] _P8

minorTriad :: Function Pitch
minorTriad = Function [m3, _M3] _P8

augmentedChord :: Function Pitch
augmentedChord = Function [_M3, _M3] _P8

diminishedChord :: Function Pitch
diminishedChord = Function [m3, m3, _A2] _P8

halfDiminishedChord :: Function Pitch
halfDiminishedChord = Function [m3, m3, _M3] _P8

-- | Also known as "dominant seventh".
majorMinorSeventhChord :: Function Pitch
majorMinorSeventhChord = Function [_M3, m3, m3] _P8

-- | Also known as "major seventh".
majorMajorSeventhChord :: Function Pitch
majorMajorSeventhChord = Function [_M3, m3, _M3] _P8

-- | Also known as a "ii7". First inversion of major sixth.
minorMinorSeventhChord :: Function Pitch
minorMinorSeventhChord = Function [m3, _M3, m3] _P8

-- | Also known as "major seventh".
minorMajorSeventhChord :: Function Pitch
minorMajorSeventhChord = Function [m3, _M3, _M3] _P8

-- TODO ninth chords, sixths chords, 6/9 (pentatonic field), Guido's hexachord?

germanSixthChord :: Function Pitch
germanSixthChord = majorMinorSeventhChord

frenchSixthChord :: Function Pitch
frenchSixthChord = Function [_M3, d3, _M3] _P8


-- TODO generalize this to "repeating"
--
-- repeating _m2 = chromaticCluster
-- repeating M2  = wholeToneCluster
-- repeating m3  = diminishedChord
-- repeating P4  = quartal
-- etc.
--
quartal :: Function Pitch
quartal = Function [_P4] _P4

quintal :: Function Pitch
quintal = Function [_P5] _P5
