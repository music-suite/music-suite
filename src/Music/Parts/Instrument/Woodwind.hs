
module Music.Parts.Instrument.Woodwind (
        WoodwindInstrument,
        woodwindInstrument,
  ) where

import Control.Lens
import Music.Parts.Instrument

newtype WoodwindInstrument = WoodwindInstrument Instrument

woodwindInstrument :: Prism' Instrument WoodwindInstrument
woodwindInstrument = undefined
-- TODO
