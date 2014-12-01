
-- | Vocal part "instruments".
-- Of course voices are not instruments, but for the sake of consistency.
module Music.Parts.Instrument.Vocal (
        VocalInstrument,
        vocalInstrument,
  ) where

import Control.Lens
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)

newtype VocalInstrument = VocalInstrument Instrument

vocalInstrument :: Prism' Instrument VocalInstrument
vocalInstrument = undefined
-- TODO
