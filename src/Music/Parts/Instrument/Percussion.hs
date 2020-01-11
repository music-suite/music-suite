module Music.Parts.Instrument.Percussion
  ( PercussionInstrument,
    percussionInstrument,
    isPercussionInstrument,
  )
where

import Control.Lens
import Data.List (isPrefixOf)
import Data.Set (Set)
import Music.Parts.Instrument
import Music.Pitch (Ambitus, Clef)
import Music.Pitch.Common (Interval, Pitch)

newtype PercussionInstrument = PercussionInstrument {getPercussionInstrument :: Instrument}

percussionInstrument :: Prism' Instrument PercussionInstrument
percussionInstrument = prism' getPercussionInstrument (fmap PercussionInstrument . partial isPercussionInstrument)

-- TODO move/consolidate
partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

isPercussionInstrument :: Instrument -> Bool
isPercussionInstrument x = case toMusicXmlSoundId x of
  Nothing -> False
  Just i ->
    any (== True) $
      fmap
        (`Data.List.isPrefixOf` i)
        ["pitched-percussion", "drum", "wood", "metal"]
