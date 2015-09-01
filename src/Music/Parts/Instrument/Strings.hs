
module Music.Parts.Instrument.Strings (
        StringInstrument,
        stringInstrument,
        isStringInstrument,
        
        StringTuning,
        standardTuning,
        allowedTunings,
        
        isAllowedTuning,
        isStandardTuning,
        isNonStandardTuning,
        
        HarmonicPosition,
        
        naturalHarmonicPositions,        
  ) where

import Control.Lens
import Control.Monad.Plus (partial)
import Data.List (isPrefixOf)
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)

newtype StringInstrument = StringInstrument { getStringInstrument :: Instrument}

stringInstrument :: Prism' Instrument StringInstrument
stringInstrument = prism' getStringInstrument (fmap StringInstrument . partial isStringInstrument)

isStringInstrument x = case toMusicXmlSoundId x of 
  Nothing -> False
  Just i  -> Data.List.isPrefixOf "strings" i

type StringTuning = [Pitch]

standardTuning :: StringInstrument -> StringTuning
standardTuning = error "No standardTuning"

-- TODO various tunings
allowedTunings :: StringInstrument -> Set StringTuning
allowedTunings = error "No allowedTunings"


isAllowedTuning :: StringInstrument -> StringTuning -> Bool
isAllowedTuning = error "No isAllowedTuning"

isStandardTuning :: StringInstrument -> StringTuning -> Bool
isStandardTuning = error "No isStandardTuning"

isNonStandardTuning :: StringInstrument -> StringTuning -> Bool
isNonStandardTuning = error "No isNonStandardTuning"


type HarmonicPosition = Integer

naturalHarmonicPositions :: StringInstrument -> Set HarmonicPosition
naturalHarmonicPositions = error "No naturalHarmonicPositions"

