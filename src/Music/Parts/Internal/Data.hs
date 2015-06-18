
{-# LANGUAGE TypeSynonymInstances #-}  -- because clef
{-# LANGUAGE FlexibleInstances #-}  -- because clef
{-# LANGUAGE CPP #-}

module Music.Parts.Internal.Data where

import Music.Pitch.Clef
import Music.Pitch.Ambitus
import Music.Pitch
-- type Clef = Int
type StandardSoundId = String

#ifndef GHCI
instance Num Clef where
  fromInteger 0 = trebleClef
  fromInteger 1 = altoClef
  fromInteger 2 = bassClef
#endif

data InstrumentTopCategory
  = Woodwind
  | Brass
  | Keyboard
  | Fretted
  | Percussion
  | Vocal
  | Strings
  | Other
  deriving (Show)


data InstrumentDef = InstrumentDef {
    _midiProgram          :: Int,
    _defaultMidiChannel   :: Int,
    _soundId              :: StandardSoundId,
    _scoreOrder           :: Double,
    _defaultClef          :: Clef,
    _allowedClefs         :: [Clef],
    _topCategory          :: InstrumentTopCategory,

    _midiName             :: String,
    _longName             :: String,
    _shortName            :: String,

    -- Map from range -> dynamic
    -- _comfortableRange     :: Ambitus Pitch,
    _allowedRange         :: Ambitus Pitch
    }
    deriving (Show)

