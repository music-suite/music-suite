
module Music.Parts.Instrument.Keyboard (
        KeyboardInstrument,
        keyboardInstrument,
        isKeyboardInstrument,
  ) where

import Control.Lens
import Data.List (isPrefixOf)
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)

newtype KeyboardInstrument = KeyboardInstrument { getKeyboardInstrument :: Instrument }

keyboardInstrument :: Prism' Instrument KeyboardInstrument
keyboardInstrument = prism' getKeyboardInstrument (fmap KeyboardInstrument . partial isKeyboardInstrument)
-- TODO

partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

isKeyboardInstrument :: Instrument -> Bool
isKeyboardInstrument x = case toMusicXmlSoundId x of
  Nothing -> False
  Just i  -> Data.List.isPrefixOf "keyboard" i