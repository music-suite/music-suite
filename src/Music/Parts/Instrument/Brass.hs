
module Music.Parts.Instrument.Brass (
        BrassInstrument,
        brassInstrument,
  ) where

import Control.Lens
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)

newtype BrassInstrument = BrassInstrument Instrument

brassInstrument :: Prism' Instrument BrassInstrument
brassInstrument = undefined
-- TODO
