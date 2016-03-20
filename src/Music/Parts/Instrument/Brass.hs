
module Music.Parts.Instrument.Brass (
        BrassInstrument,
        brassInstrument,
        isBrassInstrument,
  ) where

import Control.Lens
import Control.Monad.Plus (partial)
import Data.List (isPrefixOf)
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)

newtype BrassInstrument = BrassInstrument { getBrassInstrument :: Instrument }

brassInstrument :: Prism' Instrument BrassInstrument
brassInstrument = prism' getBrassInstrument (fmap BrassInstrument . partial isBrassInstrument)
  where

isBrassInstrument :: Instrument -> Bool
isBrassInstrument x = case toMusicXmlSoundId x of 
  Nothing -> False
  Just i  -> Data.List.isPrefixOf "brass" i

-- TODO
