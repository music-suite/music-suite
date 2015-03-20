
-- | Scales and chords.
module Music.Pitch.Scale
(
        Mode,
        Scale,
        majorScale,
        pureMinorScale,
        harmonicMinorScale,
        melodicMinorScale,
        aeolian,
        locrian,
        ionian,
        dorian,
        phrygian,
        lydian,
        mixolydian,
        majorPentaTonic,
        minorPentaTonic,
        bluesScale,
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

        Function,
        _intervals,
        _repeat,
        Chord,
        _tonic,

        majorTriad,
        minorTriad,
        augmentedChord,
        diminishedChord,
        halfDiminishedChord,
        majorSeventhChord,
  
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
majorScale = Mode [_M2,_M2] _P8

pureMinorScale :: Mode Pitch
pureMinorScale = undefined

harmonicMinorScale :: Mode Pitch
harmonicMinorScale = undefined

melodicMinorScale :: Mode Pitch
melodicMinorScale = undefined

aeolian :: Mode Pitch
aeolian = Mode [_M2,_M2,m2,_M2,_M2,m2,_M2] _P8

locrian :: Mode Pitch
locrian = undefined

ionian :: Mode Pitch
ionian = undefined

dorian :: Mode Pitch
dorian = undefined

phrygian :: Mode Pitch
phrygian = undefined

lydian :: Mode Pitch
lydian = undefined

mixolydian :: Mode Pitch
mixolydian = undefined

majorPentaTonic :: Mode Pitch
majorPentaTonic = undefined

minorPentaTonic :: Mode Pitch
minorPentaTonic = undefined

bluesScale :: Mode Pitch
bluesScale = undefined

bebopScale :: Mode Pitch
bebopScale = undefined

wholeTone :: Mode Pitch
wholeTone = undefined

octatonic :: Mode Pitch
octatonic = undefined

firstMode :: Mode Pitch
firstMode = undefined

secondMode :: Mode Pitch
secondMode = undefined

thirdMode :: Mode Pitch
thirdMode = undefined

fourthMode :: Mode Pitch
fourthMode = undefined

fifthMode :: Mode Pitch
fifthMode = undefined

sixthMode :: Mode Pitch
sixthMode = undefined

seventhMode :: Mode Pitch
seventhMode = undefined




majorTriad :: Function Pitch
majorTriad = undefined

minorTriad :: Function Pitch
minorTriad = undefined

augmentedChord :: Function Pitch
augmentedChord = undefined

diminishedChord :: Function Pitch
diminishedChord = undefined

halfDiminishedChord :: Function Pitch
halfDiminishedChord = undefined

majorSeventhChord :: Function Pitch
majorSeventhChord = undefined






