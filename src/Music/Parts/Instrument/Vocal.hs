{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

-- | Vocal part "instruments".
-- Of course voices are not instruments, but for the sake of consistency.
module Music.Parts.Instrument.Vocal
  ( Vocalist,
    vocalInstrument,
    isVocalist,
  )
where

import Control.Lens
import Data.List (isPrefixOf)
import Data.Set (Set)
import Music.Parts.Instrument
import Music.Pitch (Ambitus, Clef)
import Music.Pitch.Common (Interval, Pitch)

newtype Vocalist = Vocalist {getVocalist :: Instrument}

-- TODO move/consolidate
partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

vocalInstrument :: Prism' Instrument Vocalist
vocalInstrument = prism' getVocalist (fmap Vocalist . partial isVocalist)

isVocalist :: Instrument -> Bool
isVocalist x = case toMusicXmlSoundId x of
  Nothing -> False
  Just i -> Data.List.isPrefixOf "voice" i
