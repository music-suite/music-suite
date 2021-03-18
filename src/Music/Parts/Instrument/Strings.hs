{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

module Music.Parts.Instrument.Strings
  ( StringInstrument,
    stringInstrument,
    isStringInstrument,
  )
where

import Control.Lens
import Data.List (isPrefixOf)
import Data.Set (Set)
import Music.Parts.Instrument
import Music.Pitch (Ambitus, Clef)
import Music.Pitch.Common (Interval, Pitch)

newtype StringInstrument = StringInstrument {getStringInstrument :: Instrument}

stringInstrument :: Prism' Instrument StringInstrument
stringInstrument = prism' getStringInstrument (fmap StringInstrument . partial isStringInstrument)

-- TODO move/consolidate
partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

isStringInstrument :: Instrument -> Bool
isStringInstrument x = case toMusicXmlSoundId x of
  Nothing -> False
  Just i -> Data.List.isPrefixOf "strings" i

