
module Music.Parts.Instrument.Keyboard (
        KeyboardInstrument,
        keyboardInstrument,
        isKeyboardInstrument,
  ) where

import Control.Lens
import Control.Monad.Plus (partial)
import Data.List (isPrefixOf)
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)

newtype KeyboardInstrument = KeyboardInstrument { getKeyboardInstrument :: Instrument }

keyboardInstrument :: Prism' Instrument KeyboardInstrument
keyboardInstrument = prism' getKeyboardInstrument (fmap KeyboardInstrument . partial isKeyboardInstrument)
-- TODO

isKeyboardInstrument :: Instrument -> Bool
isKeyboardInstrument x = case toMusicXmlSoundId x of 
  Nothing -> False
  Just i  -> Data.List.isPrefixOf "keyboard" i
