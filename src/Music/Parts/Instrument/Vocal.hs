-- | Vocal part "instruments".
-- Of course voices are not instruments, but for the sake of consistency.
module Music.Parts.Instrument.Vocal
  ( VocalInstrument,
    vocalInstrument,
    isVocalInstrument,
  )
where

import Control.Lens
import Data.List (isPrefixOf)
import Data.Set (Set)
import Music.Parts.Instrument
import Music.Pitch (Ambitus, Clef)
import Music.Pitch.Common (Interval, Pitch)

newtype VocalInstrument = VocalInstrument {getVocalInstrument :: Instrument}

partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

vocalInstrument :: Prism' Instrument VocalInstrument
vocalInstrument = prism' getVocalInstrument (fmap VocalInstrument . partial isVocalInstrument)

isVocalInstrument :: Instrument -> Bool
isVocalInstrument x = case toMusicXmlSoundId x of
  Nothing -> False
  Just i -> Data.List.isPrefixOf "voice" i
