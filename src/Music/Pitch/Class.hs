
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Pitch class theory (also known as "Musical Set Theory").
module Music.Pitch.Class where

-- TODO
import Music.Prelude
-- import TypeUnary.Nat
import Data.Modular
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List

-- newtype Modulo
type PitchClass    = Semitones `Mod` 12
type IntervalClass = Semitones `Mod` 6

pitchClassToPitch :: PitchClass -> Pitch
pitchClassToPitch = (c .+^) . spell modally . toSemitones
  where
    toSemitones :: Integral a => a -> Semitones
    toSemitones = fromIntegral

type PitchSet = [PitchClass]

empt, full :: PitchSet
empt = mempty
full = [0..11]
complement = (List.\\) full


-- showPitchSet :: PitchSet -> Score
showPitchSet = asScore . scat . map (fromPitch' . pure . pitchClassToPitch) . List.nub

-- showPitchSetV :: PitchSet -> IO ()
showPitchSetV = asScore . pcat . map (fromPitch' . pure . pitchClassToPitch) . List.nub

