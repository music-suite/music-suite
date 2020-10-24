{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

module Music.Parts.Instrument.Strings
  ( StringInstrument,
    stringInstrument,
    isStringInstrument,
    StringTuning,
    standardTuning,
    allowedTunings,
    HarmonicPosition,
    naturalHarmonicPositions,
  )
where

import Control.Lens
import Data.List (isPrefixOf)
import Data.Set (Set)
import Music.Parts.Instrument
import Music.Pitch (Ambitus, Clef)
import Music.Pitch.Common (Interval, Pitch)

newtype StringInstrument = StringInstrument {getStringInstrument :: Instrument}

stringInstrument :: Prism' Instrument StringInstrument
stringInstrument = prism' getStringInstrument (fmap StringInstrument . partial isStringInstrument)

-- TODO move/consolidate
partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

isStringInstrument :: Instrument -> Bool
isStringInstrument x = case toMusicXmlSoundId x of
  Nothing -> False
  Just i -> Data.List.isPrefixOf "strings" i

type StringTuning = [Pitch]

standardTuning :: StringInstrument -> StringTuning
standardTuning = error "No standardTuning"

allowedTunings :: StringInstrument -> Set StringTuning
allowedTunings = error "No allowedTunings"

type HarmonicPosition = Integer

naturalHarmonicPositions :: StringInstrument -> Set HarmonicPosition
naturalHarmonicPositions = error "No naturalHarmonicPositions"
