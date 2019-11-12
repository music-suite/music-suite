
module Music.Parts.Instrument.Brass (
        BrassInstrument,
        brassInstrument,
        isBrassInstrument,
  ) where

import Control.Lens
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)
import qualified Data.List

newtype BrassInstrument = BrassInstrument { getBrassInstrument :: Instrument }

brassInstrument :: Prism' Instrument BrassInstrument
brassInstrument = prism' getBrassInstrument (fmap BrassInstrument . partial isBrassInstrument)
  where

partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

isBrassInstrument :: Instrument -> Bool
isBrassInstrument x = case toMusicXmlSoundId x of
  Nothing -> False
  Just i  -> Data.List.isPrefixOf "brass" i