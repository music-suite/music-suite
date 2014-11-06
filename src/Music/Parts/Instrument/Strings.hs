
module Music.Parts.Instrument.Strings (
        StringInstrument,
        stringInstrument,
        
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
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)

newtype StringInstrument = StringInstrument Instrument

stringInstrument :: Prism' Instrument StringInstrument
stringInstrument = undefined
-- TODO

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

