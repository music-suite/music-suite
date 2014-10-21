
module Music.Parts.Instrument.Keyboard (
        KeyboardInstrument,
        keyboardInstrument,
  ) where

import Control.Lens
import Music.Parts.Instrument

newtype KeyboardInstrument = KeyboardInstrument Instrument

keyboardInstrument :: Prism' Instrument KeyboardInstrument
keyboardInstrument = undefined
-- TODO
