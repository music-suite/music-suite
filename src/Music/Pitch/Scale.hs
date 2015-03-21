
-- | Scales and chords.
module Music.Pitch.Scale
(
        Mode,
        Scale,
        Function,
        _intervals,
        _repeat,
        Chord,
        _tonic,

        majorScale,
        pureMinorScale,
        harmonicMinorScale,
        melodicMinorScaleUp,
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
        wholeTone,
        octatonic,
        firstMode,
        secondMode,
        thirdMode,
        fourthMode,
        fifthMode,
        sixthMode,
        seventhMode,

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
import Music.Pitch hiding (Mode) -- Test

data Mode a = Mode [Diff a] (Diff a) -- intervals, repeat (usually octave)
data Scale a = Scale a (Mode a) -- root, mode

data Function a = Function [Diff a] (Diff a) -- intervals, repeat, repeat (usually octave)
data Chord a = Chord a (Function a) -- root, function 

_intervals :: Lens' (Function a) [Diff a]
_intervals f (Function is r) = fmap (\is -> Function is r) $ f is

_repeat :: Lens' (Function a) (Diff a)
_repeat f (Function is r) = fmap (\r -> Function is r) $ f r

{-
_pitches :: Lens' (Chord a) [a]
_pitches f (Chord t xs) = fmap (\xs -> Chord t (fmap (.-. t) xs)) $ f (fmap (.+^ t) $ xs) . _intervals
-}

_tonic :: Lens' (Chord a) a
_tonic f (Chord t xs) = fmap (\t -> Chord t xs) $ f t

majorScale :: Mode Pitch
majorScale = Mode [_M2,_M2,m2,_M2,_M2,_M2] _P8

pureMinorScale :: Mode Pitch
pureMinorScale = Mode [_M2,m2,_M2,_M2,m2,_M2] _P8

harmonicMinorScale :: Mode Pitch
harmonicMinorScale = Mode [_M2,m2,_M2,_M2,m2,_A2] _P8

melodicMinorScaleUp :: Mode Pitch
melodicMinorScaleUp = Mode [_M2,m2,_M2,_M2,_M2,_M2] _P8

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

shiftModeUp1 :: AffineSpace a => Mode a -> Mode a
shiftModeUp1 mode@(Mode steps repeating) = Mode (tail steps ++ [leadingInterval mode]) repeating

shiftMode :: AffineSpace a => Int -> Mode a -> Mode a
shiftMode 0 = id
shiftMode n = shiftMode (n-1) . shiftModeUp1

modeToScale :: AffineSpace a => a -> Mode a -> Scale a
modeToScale = Scale

scaleToList :: AffineSpace a => Scale a -> [a]
scaleToList (Scale tonic mode@(Mode steps repeating)) = offsetPoints tonic steps

  
ionian :: Mode Pitch
ionian = shiftMode 0 majorScale

dorian :: Mode Pitch
dorian = shiftMode 1 majorScale

phrygian :: Mode Pitch
phrygian = shiftMode 2 majorScale

lydian :: Mode Pitch
lydian = shiftMode 3 majorScale

mixolydian :: Mode Pitch
mixolydian = shiftMode 4 majorScale

aeolian :: Mode Pitch
aeolian = shiftMode 5 majorScale

locrian :: Mode Pitch
locrian = shiftMode 6 majorScale

majorPentaTonic :: Mode Pitch
majorPentaTonic = Mode [_M2,_M2,m3,_M2] _P8

bluesMinor :: Mode Pitch
bluesMinor = shiftMode 2 majorPentaTonic

bluesMajor :: Mode Pitch
bluesMajor = shiftMode 3 majorPentaTonic

minorPentaTonic :: Mode Pitch
minorPentaTonic = shiftMode 4 majorPentaTonic



bebopScale :: Mode Pitch
bebopScale = undefined

wholeTone :: Mode Pitch
wholeTone = Mode [_M2,_M2,_M2,_M2,_M2] _P8

octatonic :: Mode Pitch
octatonic = Mode [m2,_M2,_A1,_M2,m2,_M2,m2{-M2-}] _P8

-- What is this?
-- Mode [_A1,m3,m2,m3,_A1{-m3-}] _P8

firstMode :: Mode Pitch
firstMode = wholeTone

secondMode :: Mode Pitch
secondMode = octatonic

-- tone, semitone, semitone, tone, semitone, semitone, tone, semitone, semitone
thirdMode :: Mode Pitch
thirdMode = undefined

-- semitone, semitone, minor third, semitone, semitone, semitone, minor third, semitone
fourthMode :: Mode Pitch
fourthMode = undefined

-- semitone, major third, semitone, semitone, major third, semitone
fifthMode :: Mode Pitch
fifthMode = undefined

-- tone, tone, semitone, semitone, tone, tone, semitone, semitone
sixthMode :: Mode Pitch
sixthMode = undefined

--  semitone, semitone, semitone, tone, semitone, semitone, semitone, semitone, tone, semitone 
seventhMode :: Mode Pitch
seventhMode = undefined



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

-- TODO name?
characteristicInterval :: AffineSpace a => Function a -> Diff a
characteristicInterval (Function leaps repeating) = sumV leaps

invertChordUp1 :: AffineSpace a => Function a -> Function a
invertChordUp1 function@(Function leaps repeating) = Function (tail leaps ++ [complementInterval function]) repeating

invertChord :: AffineSpace a => Int -> Function a -> Function a
invertChord 0 = id
invertChord n = invertChord (n-1) . invertChordUp1


functionToChord :: AffineSpace a => a -> Function a -> Chord a
functionToChord = Chord

chordToList :: AffineSpace a => Chord a -> [a]
chordToList (Chord tonic mode@(Function leaps repeating)) = offsetPoints tonic leaps
-- TODO inversion?

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

-- 




-- TODO consolidate
offsetPoints :: AffineSpace p => p -> [Diff p] -> [p]
offsetPoints = scanl (.+^)

