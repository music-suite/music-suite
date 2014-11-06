
module Music.Parts.Instrument.Percussion (
        PercussionInstrument,
        percussionInstrument,
  ) where

import Control.Lens
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)

newtype PercussionInstrument = PercussionInstrument Instrument

percussionInstrument :: Prism' Instrument PercussionInstrument
percussionInstrument = undefined
-- TODO
