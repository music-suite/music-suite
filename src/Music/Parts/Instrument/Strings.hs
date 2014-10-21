
module Music.Parts.Instrument.Strings (
        StringInstrument,
        stringInstrument,
  ) where

import Control.Lens
import Music.Parts.Instrument

newtype StringInstrument = StringInstrument Instrument

stringInstrument :: Prism' Instrument StringInstrument
stringInstrument = undefined
-- TODO
