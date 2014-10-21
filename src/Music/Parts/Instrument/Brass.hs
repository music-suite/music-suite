
module Music.Parts.Instrument.Brass (
        BrassInstrument,
        brassInstrument,
  ) where

import Control.Lens
import Music.Parts.Instrument

newtype BrassInstrument = BrassInstrument Instrument

brassInstrument :: Prism' Instrument BrassInstrument
brassInstrument = undefined
-- TODO
