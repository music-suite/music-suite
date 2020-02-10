{-# LANGUAGE DataKinds #-}

-- | Pitch class theory (also known as Musical Set Theory).
module Music.Pitch.Class where


-- import TypeUnary.Nat

import qualified Data.List as List
import Data.Modular
import Data.Set (Set)
import qualified Data.Set as Set
import Music.Prelude

{-
type PitchClass = Semitones `Mod` 12

type IntervalClass = Semitones `Mod` 6

pitchClassToPitch :: PitchClass -> Pitch
pitchClassToPitch = (c .+^) . spell modally . toSemitones
  where
    toSemitones :: Integral a => a -> Semitones
    toSemitones = fromIntegral

type PitchSet = [PitchClass]

empt, full :: PitchSet
empt = mempty
full = [0 .. 11]

complement = (List.\\) full

-- showPitchSet :: PitchSet -> Score
showPitchSet = asScore . pseq . map (fromPitch' . pure . pitchClassToPitch) . List.nub

-- showPitchSetV :: PitchSet -> IO ()
showPitchSetV = asScore . ppar . map (fromPitch' . pure . pitchClassToPitch) . List.nub

-}
