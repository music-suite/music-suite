
module Music.Parts.Instrument.Percussion (
        PercussionInstrument,
        percussionInstrument,
        isPercussionInstrument,
  ) where

import Control.Lens
import Control.Monad.Plus (partial)
import Data.List (isPrefixOf)
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)

newtype PercussionInstrument = PercussionInstrument { getPercussionInstrument :: Instrument }

percussionInstrument :: Prism' Instrument PercussionInstrument
percussionInstrument = prism' getPercussionInstrument (fmap PercussionInstrument . partial isPercussionInstrument)

isPercussionInstrument x = case toMusicXmlSoundId x of 
  Nothing -> False
  Just i  -> any (== True) $ fmap (`Data.List.isPrefixOf` i) 
                     ["pitched-percussion", "drum", "wood", "metal"]
