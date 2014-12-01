
{-# LANGUAGE TypeSynonymInstances #-}  -- because clef
{-# LANGUAGE FlexibleInstances #-}  -- because clef

module Music.Parts.Internal.Data where

import Music.Pitch.Clef
-- type Clef = Int
type StandardSoundId = String

instance Num Clef where
  fromInteger 0 = trebleClef
  fromInteger 1 = altoClef
  fromInteger 2 = bassClef

data InstrumentTopCategory
  = Woodwind
  | Brass
  | Keyboard
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
    _longName             :: String,
    _midiName             :: String,
    _shortName            :: String
    }
    deriving (Show)

defs = [
    InstrumentDef {
      _soundId            = "keyboard.piano",
      _midiProgram        = 0,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Piano"
    },
    InstrumentDef {
      _soundId            = "keyboard.piano",
      _midiProgram        = 1,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Bright Acoustic Piano"
    },
    InstrumentDef {
      _soundId            = "keyboard.piano.electric",
      _midiProgram        = 2,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Electric Grand Piano" 
    },
    InstrumentDef {
      _soundId            = "keyboard.piano.honky-tonk",
      _midiProgram        = 3,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Honky-tonk Piano"
    },
    InstrumentDef {
      _soundId            = "keyboard.piano.electric",
      _midiProgram        = 4,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Electric Piano 1"
    },
    InstrumentDef {
      _soundId            = "keyboard.piano.electric",
      _midiProgram        = 5,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Electric Piano 2"
    },
    InstrumentDef {
      _soundId            = "keyboard.harpsichord",
      _midiProgram        = 6,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Harpsichord"
    },
    InstrumentDef {
      _soundId            = "keyboard.electric",
      _midiProgram        = 7,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Clavinet"
    },
    InstrumentDef {
      _soundId            = "keyboard.celesta",
      _midiProgram        = 8,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Celesta"
    },
    InstrumentDef {
      _midiProgram        = 9,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Glockenspiel"
    },
    InstrumentDef {
      _midiProgram        = 9,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Music Box"
    },
    InstrumentDef {
      _soundId            = "pitched-percussion.vibraphone",
      _midiProgram        = 11,
      _defaultMidiChannel = 0,
      _scoreOrder         = 4.0,
      _defaultClef        = 0,
      _longName           = "Vibraphone"
    },
    InstrumentDef {
      _soundId            = "pitched-percussion.marimba",
      _midiProgram        = 12,
      _defaultMidiChannel = 0,
      _scoreOrder         = 4.0,
      _defaultClef        = 0,
      _longName           = "Marimba"
    },
    InstrumentDef {
      _soundId            = "pitched-percussion.xylophone",
      _midiProgram        = 13,
      _defaultMidiChannel = 0,
      _scoreOrder         = 4.0,
      _defaultClef        = 0,
      _longName           = "Xylophone"
    },
    InstrumentDef {
      _soundId            = "pitched-percussion.tubular-bells",
      _midiProgram        = 14,
      _defaultMidiChannel = 0,
      _scoreOrder         = 4.0,
      _defaultClef        = 0,
      _longName           = "Tubular Bells"
    },
    InstrumentDef {
      _midiProgram        = 15,
      _defaultMidiChannel = 0,
      _scoreOrder         = 4.0,
      _defaultClef        = 0,
      _longName           = "Dulcimer"
    },
    InstrumentDef {
      _midiProgram        = 16,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Drawbar Organ"
    },
    InstrumentDef {
      _midiProgram        = 17,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Percussive Organ"
    },
    InstrumentDef {
      _midiProgram        = 18,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Rock Organ"
    },
    InstrumentDef {
      _midiProgram        = 19,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Church Organ"
    },
    InstrumentDef {
      _midiProgram        = 20,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Reed Organ"
    },
    InstrumentDef {
      _midiProgram        = 21,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Accordion"
    },
    InstrumentDef {
      _midiProgram        = 22,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Harmonica"
    },
    InstrumentDef {
      _midiProgram        = 23,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Tango Accordion"
    },
    InstrumentDef {
      _midiProgram        = 24,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Acoustic Guitar (nylon)" 
    },
    InstrumentDef {
      _midiProgram        = 25,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Acoustic Guitar (steel)"
    },
    InstrumentDef {
      _midiProgram        = 26,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Electric Guitar (jazz)"
    },
    InstrumentDef {
      _midiProgram        = 27,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Electric Guitar (clean)"
    },
    InstrumentDef {
      _midiProgram        = 28,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Electric Guitar (muted)"
    },
    InstrumentDef {
      _midiProgram        = 29,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Overdriven Guitar"
    },
    InstrumentDef {
      _midiProgram        = 30,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Distortion Guitar"
    },
    InstrumentDef {
      _midiProgram        = 31,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Guitar Harmonics"
    },
    InstrumentDef {
      _midiProgram        = 32,
      _defaultMidiChannel = 0,
      _scoreOrder         = 8.0,
      _defaultClef        = 2,
      _longName           = "Acoustic Bass"
    },
    InstrumentDef {
      _midiProgram        = 33,
      _defaultMidiChannel = 0,
      _scoreOrder         = 8.0,
      _defaultClef        = 2,
      _longName           = "Electric Bass (finger)"
    },
    InstrumentDef {
      _midiProgram        = 34,
      _defaultMidiChannel = 0,
      _scoreOrder         = 8.0,
      _defaultClef        = 2,
      _longName           = "Electric Bass (pick)"
    },
    InstrumentDef {
      _midiProgram        = 35,
      _defaultMidiChannel = 0,
      _scoreOrder         = 8.0,
      _defaultClef        = 2,
      _longName           = "Fretless Bass"
    },
    InstrumentDef {
      _midiProgram        = 36,
      _defaultMidiChannel = 0,
      _scoreOrder         = 8.0,
      _defaultClef        = 2,
      _longName           = "Slap Bass 1"
    },
    InstrumentDef {
      _midiProgram        = 37,
      _defaultMidiChannel = 0,
      _scoreOrder         = 8.0,
      _defaultClef        = 2,
      _longName           = "Slap Bass 2"
    },
    InstrumentDef {
      _midiProgram        = 38,
      _defaultMidiChannel = 0,
      _scoreOrder         = 8.0,
      _defaultClef        = 2,
      _longName           = "Synth Bass 1"
    },
    InstrumentDef {
      _midiProgram        = 39,
      _defaultMidiChannel = 0,
      _scoreOrder         = 8.0,
      _defaultClef        = 2,
      _longName           = "Synth Bass 2"
    },
    InstrumentDef {
      _midiProgram        = 40,
      _defaultMidiChannel = 12,
      _scoreOrder         = 7.1,
      _defaultClef        = 0,
      _longName           = "Violin"
    },
    InstrumentDef {
      _midiProgram        = 41,
      _defaultMidiChannel = 13,
      _scoreOrder         = 7.2,
      _defaultClef        = 1,
      _longName           = "Viola"
    },
    InstrumentDef {
      _midiProgram        = 42,
      _defaultMidiChannel = 14,
      _scoreOrder         = 7.3,
      _defaultClef        = 2,
      _longName           = "Violoncello"
    },
    InstrumentDef {
      _midiProgram        = 43,
      _defaultMidiChannel = 15,
      _scoreOrder         = 7.4,
      _defaultClef        = 2,
      _longName           = "Contrabass"
    },
    --
    InstrumentDef {
      _midiProgram        = 44,
      _defaultMidiChannel = 0,
      _scoreOrder         = 7.0,
      _defaultClef        = 0,
      _longName           = "Tremolo Strings"
    },
    InstrumentDef {
      _midiProgram        = 45,
      _defaultMidiChannel = 0,
      _scoreOrder         = 7.0,
      _defaultClef        = 0,
      _longName           = "Pizzicato Strings"
    },
    InstrumentDef {
      _midiProgram        = 46,
      _defaultMidiChannel = 11,
      _scoreOrder         = 5.9,
      _defaultClef        = 0,
      _longName           = "Orchestral Harp"
    },
    -- 
    InstrumentDef {
      _midiProgram        = 47,
      _defaultMidiChannel = 8,
      _scoreOrder         = 2.5,
      _defaultClef        = 2,
      _longName           = "Timpani"
    },
    -- 
    InstrumentDef {
      _midiProgram        = 48,
      _defaultMidiChannel = 0,
      _scoreOrder         = 7.0,
      _defaultClef        = 0,
      _longName           = "String Ensemble 1"
    },
    InstrumentDef {
      _midiProgram        = 49,
      _defaultMidiChannel = 0,
      _scoreOrder         = 7.0,
      _defaultClef        = 0,
      _longName           = "String Ensemble 2"
    },
    InstrumentDef {
      _midiProgram        = 50,
      _defaultMidiChannel = 0,
      _scoreOrder         = 7.0,
      _defaultClef        = 0,
      _longName           = "Synth Strings 1"
    },
    InstrumentDef {
      _midiProgram        = 51,
      _defaultMidiChannel = 0,
      _scoreOrder         = 7.0,
      _defaultClef        = 0,
      _longName           = "Synth Strings 2"
    },
    -- 
    InstrumentDef {
      _midiProgram        = 52,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Choir Aahs"
    },
    InstrumentDef {
      _midiProgram        = 53,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Voice Oohs"
    },
    InstrumentDef {
      _midiProgram        = 54,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Synth Choir"
    },
    InstrumentDef {
      _midiProgram        = 55,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Orchestra Hit"
    },
    -- 
    InstrumentDef {
      _midiProgram        = 56,
      _defaultMidiChannel = 5,
      _scoreOrder         = 2.2,
      _defaultClef        = 0,
      _longName           = "Trumpet in Bb"
    },
    InstrumentDef {
      _midiProgram        = 57,
      _defaultMidiChannel = 6,
      _scoreOrder         = 2.3,
      _defaultClef        = 2,
      _longName           = "Trombone"
    },
    InstrumentDef {
      _midiProgram        = 58,
      _defaultMidiChannel = 7,
      _scoreOrder         = 2.4,
      _defaultClef        = 2,
      _longName           = "Tuba"
    },
    InstrumentDef {
      _midiProgram        = 59,
      _defaultMidiChannel = 0,
      _scoreOrder         = 2.2,
      _defaultClef        = 0,
      _longName           = "Muted Trumpet"
    },
    InstrumentDef {
      _midiProgram        = 60,
      _defaultMidiChannel = 4,
      _scoreOrder         = 2.1,
      _defaultClef        = 0,
      _midiName           = "French Horn",
      _longName           = "Horn in F"
    },
    InstrumentDef {
      _midiProgram        = 61,
      _defaultMidiChannel = 0,
      _scoreOrder         = 2.0,
      _defaultClef        = 0,
      _longName           = "Brass Section"
    },
    InstrumentDef {
      _midiProgram        = 62,
      _defaultMidiChannel = 0,
      _scoreOrder         = 2.0,
      _defaultClef        = 0,
      _longName           = "Synth Brass 1"
    },
    InstrumentDef {
      _midiProgram        = 63,
      _defaultMidiChannel = 0,
      _scoreOrder         = 2.0,
      _defaultClef        = 0,
      _longName           = "Synth Brass 2"
    },
    -- 
    InstrumentDef {
      _midiProgram        = 64,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.51,
      _defaultClef        = 0,
      _longName           = "Soprano Sax"
    },
    InstrumentDef {
      _midiProgram        = 65,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.52,
      _defaultClef        = 0,
      _longName           = "Alto Sax"
    },
    InstrumentDef {
      _midiProgram        = 66,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.53,
      _defaultClef        = 0,
      _longName           = "Tenor Sax"
    },
    InstrumentDef {
      _midiProgram        = 67,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.54,
      _defaultClef        = 0,
      _longName           = "Baritone Sax"
    },
    InstrumentDef {
      _midiProgram        = 68,
      _defaultMidiChannel = 1,
      _scoreOrder         = 1.3,
      _defaultClef        = 0,
      _longName           = "Oboe"
    },
    InstrumentDef {
      _midiProgram        = 69,
      _defaultMidiChannel = 1,
      _scoreOrder         = 1.4,
      _defaultClef        = 0,
      _longName           = "English Horn"
    },
    InstrumentDef {
      _midiProgram        = 70,
      _defaultMidiChannel = 3,
      _scoreOrder         = 1.7,
      _defaultClef        = 2,
      _longName           = "Bassoon"
    },
    InstrumentDef {
      _midiProgram        = 71,
      _defaultMidiChannel = 2,
      _scoreOrder         = 1.6,
      _defaultClef        = 0,
      _longName           = "Clarinet in Bb"
    },
    -- 
    InstrumentDef {
      _midiProgram        = 72,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.1,
      _defaultClef        = 0,
      _longName           = "Piccolo"
    },
    InstrumentDef {
      _midiProgram        = 73,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.2,
      _defaultClef        = 0,
      _longName           = "Flute"
    },
    InstrumentDef {
      _midiProgram        = 74,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Recorder"
    },
    InstrumentDef {
      _midiProgram        = 75,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Pan Flute"
    },
    InstrumentDef {
      _midiProgram        = 76,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Blown bottle"
    },
    InstrumentDef {
      _midiProgram        = 77,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Shakuhachi"
    },
    InstrumentDef {
      _midiProgram        = 78,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Whistle"
    },
    InstrumentDef {
      _midiProgram        = 79,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Ocarina"
    },
    -- 
    InstrumentDef {
      _midiProgram        = 80,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Lead 1 (square)"
    },
    InstrumentDef {
      _midiProgram        = 81,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Lead 2 (sawtooth)"
    },
    InstrumentDef {
      _midiProgram        = 82,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Lead 3 (calliope)"
    },
    InstrumentDef {
      _midiProgram        = 83,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Lead 4 (chiff)"
    },
    InstrumentDef {
      _midiProgram        = 84,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Lead 5 (charang)"
    },
    InstrumentDef {
      _midiProgram        = 85,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Lead 6 (voice)"
    },
    InstrumentDef {
      _midiProgram        = 86,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Lead 7 (fifths)"
    },
    InstrumentDef {
      _midiProgram        = 87,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Lead 8 (bass + lead)"
    },
    -- 
    InstrumentDef {
      _midiProgram        = 88,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Pad 1 (new age)"
    },
    InstrumentDef {
      _midiProgram        = 89,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Pad 2 (warm)"
    },
    InstrumentDef {
      _midiProgram        = 90,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Pad 3 (polysynth)"
    },
    InstrumentDef {
      _midiProgram        = 91,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Pad 4 (choir)"
    },
    InstrumentDef {
      _midiProgram        = 92,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Pad 5 (bowed)"
    },
    InstrumentDef {
      _midiProgram        = 93,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Pad 6 (metallic)"
    },
    InstrumentDef {
      _midiProgram        = 94,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Pad 7 (halo)"
    },
    InstrumentDef {
      _midiProgram        = 95,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Pad 8 (sweep)"
    },
    -- 
    InstrumentDef {
      _midiProgram        = 96,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "FX 1 (rain)"
    },
    InstrumentDef {
      _midiProgram        = 97,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "FX 2 (soundtrack)"
    },
    InstrumentDef {
      _midiProgram        = 98,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "FX 3 (crystal)"
    },
    InstrumentDef {
      _midiProgram        = 99,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "FX 4 (atmosphere)"
    },
    InstrumentDef {
      _midiProgram        = 100,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "FX 5 (brightness)"
    },
    InstrumentDef {
      _midiProgram        = 101,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "FX 6 (goblins)"
    },
    InstrumentDef {
      _midiProgram        = 102,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "FX 7 (echoes)"
    },
    InstrumentDef {
      _midiProgram        = 103,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "FX 8 (sci-fi)"
    },
    InstrumentDef {
      _midiProgram        = 104,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Sitar"
    },
    InstrumentDef {
      _midiProgram        = 105,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Banjo"
    },
    InstrumentDef {
      _midiProgram        = 106,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Shamisen"
    },
    InstrumentDef {
      _midiProgram        = 107,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Koto"
    },
    InstrumentDef {
      _midiProgram        = 108,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Kalimba"
    },
    InstrumentDef {
      _midiProgram        = 109,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Bagpipe"
    },
    InstrumentDef {
      _midiProgram        = 110,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Fiddle"
    },
    InstrumentDef {
      _midiProgram        = 111,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Shanai"
    },
    InstrumentDef {
      _midiProgram        = 112,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Tinkle Bell"
    },
    InstrumentDef {
      _midiProgram        = 113,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Agogo"
    },
    InstrumentDef {
      _midiProgram        = 114,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel        = 0,
      _longName           = "Steel Drums"
    },
    InstrumentDef {
      _midiProgram        = 115,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Woodblock"
    },
    InstrumentDef {
      _midiProgram        = 116,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Taiko Drum"
    },
    InstrumentDef {
      _midiProgram        = 117,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Melodic Tom"
    },
    InstrumentDef {
      _midiProgram        = 118,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Synth Drum"
    },
    InstrumentDef {
      _midiProgram        = 119,
      _defaultClef        = 0,
      _scoreOrder         = 1.0,
      _defaultMidiChannel = 0,
      _longName           = "Reverse Cymbal"
    },
    InstrumentDef {
      _midiProgram        = 120,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Guitar Fret Noise"
    },
    InstrumentDef {
      _midiProgram        = 121,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Breath Noise"
    },
    InstrumentDef {
      _midiProgram        = 122,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Seashore"
    },
    InstrumentDef {
      _midiProgram        = 123,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Bird Tweet"
    },
    InstrumentDef {
      _midiProgram        = 124,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Telephone Ring"
    },
    InstrumentDef {
      _midiProgram        = 125,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Helicopter"
    },
    InstrumentDef {
      _midiProgram        = 126,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Applause"
    },
    InstrumentDef {
      _midiProgram        = 127,
      _defaultMidiChannel = 0,
      _scoreOrder         = 1.0,
      _defaultClef        = 0,
      _longName           = "Gunshot"
    }
    ]





{-
gmPerc :: [(Int, String)]
gmPerc = [
    (35, "Bass Drum 2"),
    (36, "Bass Drum 1"),
    (37, "Side Stick/Rimshot"),
    (38, "Snare Drum 1"),
    (39, "Hand Clap"),
    (40, "Snare Drum 2"),
    (41, "Low Tom 2"),
    (42, "Closed Hi-hat"),
    (43, "Low Tom 1"),
    (44, "Pedal Hi-hat"),
    (45, "Mid Tom 2"),
    (46, "Open Hi-hat"),
    (47, "Mid Tom 1"),
    (48, "High Tom 2"),
    (49, "Crash Cymbal 1"),
    (50, "High Tom 1"),
    (51, "Ride Cymbal 1"),
    (52, "Chinese Cymbal"),
    (53, "Ride Bell"),
    (54, "Tambourine"),
    (55, "Splash Cymbal"),
    (56, "Cowbell"),
    (57, "Crash Cymbal 2"),
    (58, "Vibra Slap"),
    (59, "Ride Cymbal 2"),
    (60, "High Bongo"),
    (61, "Low Bongo"),
    (62, "Mute High Conga"),
    (63, "Open High Conga"),
    (64, "Low Conga"),
    (65, "High Timbale"),
    (66, "Low Timbale"),
    (67, "High Agogô"),
    (68, "Low Agogô"),
    (69, "Cabasa"),
    (70, "Maracas"),
    (71, "Short Whistle"),
    (72, "Long Whistle"),
    (73, "Short Güiro"),
    (74, "Long Güiro"),
    (75, "Claves"),
    (76, "High Wood Block"),
    (77, "Low Wood Block"),
    (78, "Mute Cuíca"),
    (79, "Open Cuíca"),
    (80, "Mute Triangle"),
    (81, "Open Triangle")
    ]
-}
