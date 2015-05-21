
-- | Scales and chords.
module Music.Pitch.Scale
(
        Mode,
        modeFromSteps,
        modeIntervals,
        modeRepeat,
        Function,
        functionFromSteps,
        functionIntervals,
        functionRepeat,
        
        Scale,
        scaleTonic,
        scaleMode,
        Chord,
        chordTonic,
        chordFunction,

        complementInterval,        
        invertChord,               
        functionToChord,           
        chordToList,               

        leadingInterval,           
        invertMode,                
        modeToScale,
        scaleToList,

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
  
) where

import Data.VectorSpace
import Data.AffineSpace
import Music.Pitch.Literal
import Control.Lens
import Music.Pitch.Literal
import Music.Pitch.Common hiding (Mode)

-- | A scale is a mode with a specified tonic.
data Scale a = Scale a (Mode a)      -- root, mode

-- | A mode is a list of intervals and a characteristic repeating inverval. 
data Mode a = Mode [Diff a] (Diff a) -- intervals, repeat (usually octave)

scaleTonic :: Lens' (Scale a) a
scaleTonic f (Scale t xs) = fmap (\t -> Scale t xs) $ f t

scaleMode :: Lens' (Scale a) (Mode a)
scaleMode f (Scale t xs) = fmap (\xs -> Scale t xs) $ f xs

modeIntervals :: Lens' (Mode a) [Diff a]
modeIntervals f (Mode is r) = fmap (\is -> Mode is r) $ f is

modeRepeat :: Lens' (Mode a) (Diff a)
modeRepeat f (Mode is r) = fmap (\r -> Mode is r) $ f r

-- |
-- > [Interval] -> Interval -> Mode Pitch
modeFromSteps :: [Diff a] -> Diff a -> Mode a
modeFromSteps = Mode


data Chord a = Chord a (Function a)          -- root, function

data Function a = Function [Diff a] (Diff a) -- intervals, repeat, repeat (usually octave)

chordTonic :: Lens' (Chord a) a
chordTonic f (Chord t xs) = fmap (\t -> Chord t xs) $ f t

chordFunction :: Lens' (Chord a) (Function a)
chordFunction f (Chord t xs) = fmap (\xs -> Chord t xs) $ f xs

functionIntervals :: Lens' (Function a) [Diff a]
functionIntervals f (Function is r) = fmap (\is -> Function is r) $ f is

functionRepeat :: Lens' (Function a) (Diff a)
functionRepeat f (Function is r) = fmap (\r -> Function is r) $ f r
-- |
-- > [Interval] -> Interval -> Function Pitch
functionFromSteps :: [Diff a] -> Diff a -> Function a
functionFromSteps = Function

{-
_pitches :: Lens' (Chord a) [a]
_pitches f (Chord t xs) = fmap (\xs -> Chord t (fmap (.-. t) xs)) $ f (fmap (.+^ t) $ xs) . _intervals
-}




{-
>>> complementInterval majorScale 
m2
>>> complementInterval harmonicMinorScale 
m2
>>> complementInterval pureMinorScale 
_M2
-}
complementInterval :: AffineSpace a => Function a -> Diff a
complementInterval (Function leaps repeating) = repeating ^-^ sumV leaps

invertChord :: AffineSpace a => Int -> Function a -> Function a
invertChord 0 = id
invertChord n = invertChord (n-1) . invertChord1
  where
    invertChord1 :: AffineSpace a => Function a -> Function a
    invertChord1 function@(Function leaps repeating) = Function (tail leaps ++ [complementInterval function]) repeating

functionToChord :: AffineSpace a => a -> Function a -> Chord a
functionToChord = Chord

-- | Returns a single inversion of the given chord (no repeats!).
chordToList :: AffineSpace a => Chord a -> [a]
chordToList (Chord tonic mode@(Function leaps repeating)) = offsetPoints tonic leaps
-- TODO inversion?
  where
    -- TODO consolidate
    offsetPoints :: AffineSpace p => p -> [Diff p] -> [p]
    offsetPoints = scanl (.+^)

{-
>>> leadingInterval majorScale 
m2
>>> leadingInterval harmonicMinorScale 
m2
>>> leadingInterval pureMinorScale 
_M2
-}
leadingInterval :: AffineSpace a => Mode a -> Diff a
leadingInterval (Mode steps repeating) = repeating ^-^ sumV steps

invertMode :: AffineSpace a => Int -> Mode a -> Mode a
invertMode 0 = id
invertMode n = invertMode (n-1) . invertMode1
  where
    invertMode1 :: AffineSpace a => Mode a -> Mode a
    invertMode1 mode@(Mode steps repeating) = Mode (tail steps ++ [leadingInterval mode]) repeating

modeToScale :: AffineSpace a => a -> Mode a -> Scale a
modeToScale = Scale

scaleToList :: AffineSpace a => Scale a -> [a]
scaleToList (Scale tonic mode@(Mode steps repeating)) = offsetPoints tonic steps
  where
    -- TODO consolidate
    offsetPoints :: AffineSpace p => p -> [Diff p] -> [p]
    offsetPoints = scanl (.+^)




-- Common scales

majorScale :: Mode Pitch
majorScale = modeFromSteps [_M2,_M2,m2,_M2,_M2,_M2] _P8

pureMinorScale :: Mode Pitch
pureMinorScale = modeFromSteps [_M2,m2,_M2,_M2,m2,_M2] _P8

harmonicMinorScale :: Mode Pitch
harmonicMinorScale = modeFromSteps [_M2,m2,_M2,_M2,m2,_A2] _P8

melodicMinorScaleUp :: Mode Pitch
melodicMinorScaleUp = modeFromSteps [_M2,m2,_M2,_M2,_M2,_M2] _P8

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
majorPentaTonic = modeFromSteps [_M2,_M2,m3,_M2] _P8

bluesMinor :: Mode Pitch
bluesMinor = invertMode 2 majorPentaTonic

bluesMajor :: Mode Pitch
bluesMajor = invertMode 3 majorPentaTonic

minorPentaTonic :: Mode Pitch
minorPentaTonic = invertMode 4 majorPentaTonic

bebopScale :: Mode Pitch
bebopScale = modeFromSteps [_M2,_M2,m2,_M2,m2,m2,_M2] _P8

wholeTone :: Mode Pitch
wholeTone = firstMode

octatonic :: Mode Pitch
octatonic = secondMode

firstMode :: Mode Pitch
firstMode = modeFromSteps [_M2,_M2,_M2,_M2,_M2] _P8

secondMode :: Mode Pitch
secondMode = modeFromSteps [m2,_M2,_A1,_M2,m2,_M2,m2] _P8

thirdMode :: Mode Pitch
thirdMode = modeFromSteps [_M2,m2,m2,_M2,m2,m2,_M2,m2] _P8

fourthMode :: Mode Pitch
fourthMode = modeFromSteps [m2,m2,m3,m2,m2,m2,m3] _P8

fifthMode :: Mode Pitch
fifthMode = modeFromSteps [m2,_M3,m2,m2,_M3] _P8

sixthMode :: Mode Pitch
sixthMode = modeFromSteps [_M2,_M2,m2,m2,_M2,_M2,m2] _P8

seventhMode :: Mode Pitch
seventhMode = modeFromSteps [m2,m2,m2,_M2,m2,m2,m2,m2,_M2] _P8

-- Common chords

majorTriad :: Function Pitch
majorTriad = Function [_M3,m3] _P8

minorTriad :: Function Pitch
minorTriad = Function [m3,_M3] _P8

augmentedChord :: Function Pitch
augmentedChord = Function [_M3,_M3] _P8

diminishedChord :: Function Pitch
diminishedChord = Function [m3,m3,_A2] _P8

halfDiminishedChord :: Function Pitch
halfDiminishedChord = Function [m3,m3,_M3] _P8

majorMinorSeventhChord :: Function Pitch
majorMinorSeventhChord = Function [_M3,m3,m3] _P8

majorMajorSeventhChord :: Function Pitch
majorMajorSeventhChord = Function [_M3,m3,_M3] _P8


