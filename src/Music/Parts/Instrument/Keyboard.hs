
module Music.Parts.Instrument.Keyboard (
        KeyboardInstrument,
        keyboardInstrument,
  ) where

import Control.Lens
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)

newtype KeyboardInstrument = KeyboardInstrument Instrument

keyboardInstrument :: Prism' Instrument KeyboardInstrument
keyboardInstrument = undefined
-- TODO
