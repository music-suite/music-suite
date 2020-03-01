{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints
  #-}
module Music.Parts.Instrument.Keyboard
  ( KeyboardInstrument,
    keyboardInstrument,
    isKeyboardInstrument,
  )
where

import Control.Lens
import Data.List (isPrefixOf)
import Data.Set (Set)
import Music.Parts.Instrument
import Music.Pitch (Ambitus, Clef)
import Music.Pitch.Common (Interval, Pitch)

newtype KeyboardInstrument = KeyboardInstrument {getKeyboardInstrument :: Instrument}

keyboardInstrument :: Prism' Instrument KeyboardInstrument
keyboardInstrument = prism' getKeyboardInstrument (fmap KeyboardInstrument . partial isKeyboardInstrument)

-- TODO move/consolidate
partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

isKeyboardInstrument :: Instrument -> Bool
isKeyboardInstrument x = case toMusicXmlSoundId x of
  Nothing -> False
  Just i -> Data.List.isPrefixOf "keyboard" i
