
module Music.Parts.Instrument.Percussion (
        PercussionInstrument,
        percussionInstrument,
  ) where

import Control.Lens
import Music.Parts.Instrument

newtype PercussionInstrument = PercussionInstrument Instrument

percussionInstrument :: Prism' Instrument PercussionInstrument
percussionInstrument = undefined
-- TODO
