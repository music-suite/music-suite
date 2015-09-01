
-- | Vocal part "instruments".
-- Of course voices are not instruments, but for the sake of consistency.
module Music.Parts.Instrument.Vocal (
        VocalInstrument,
        vocalInstrument,
  ) where

import Control.Lens
import Control.Monad.Plus (partial)
import Data.List (isPrefixOf)
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)

newtype VocalInstrument = VocalInstrument { getVocalInstrument :: Instrument}

vocalInstrument :: Prism' Instrument VocalInstrument
vocalInstrument = prism' getVocalInstrument (fmap VocalInstrument . partial isVoice)
  where
    isVoice x = case toMusicXmlSoundId x of 
      Nothing -> False
      Just i  -> Data.List.isPrefixOf "voice" i 
    