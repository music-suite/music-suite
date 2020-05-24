{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

module Music.Parts.Instrument.Brass
  ( BrassInstrument,
    brassInstrument,
    isBrassInstrument,
  )
where

import Control.Lens
import qualified Data.List
import Data.Set (Set)
import Music.Parts.Instrument
import Music.Pitch (Ambitus, Clef)
import Music.Pitch.Common (Interval, Pitch)

newtype BrassInstrument = BrassInstrument {getBrassInstrument :: Instrument}

brassInstrument :: Prism' Instrument BrassInstrument
brassInstrument = prism' getBrassInstrument (fmap BrassInstrument . partial isBrassInstrument)
  where

-- TODO move/consolidate
partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

isBrassInstrument :: Instrument -> Bool
isBrassInstrument x = case toMusicXmlSoundId x of
  Nothing -> False
  Just i -> Data.List.isPrefixOf "brass" i
