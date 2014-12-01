
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


data InstrumentDef = InstrumentDef {
    _midiProgram          :: Int,
    _defaultMidiChannel   :: Int,
    _soundIf              :: StandardSoundId,
    _scoreOrder           :: Double,
    _defaultClef          :: Clef,
    _allowedClefs         :: [Clef],
    _topCategory          :: InstrumentTopCategory,
    _longName             :: String,
    _midiName             :: String,
    _shortName            :: String
    }

defs = [
    InstrumentDef {
      _midiProgram        = 0,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Piano"
    },
    InstrumentDef {
      _midiProgram        = 0,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Piano"
    },
    InstrumentDef {
      _midiProgram        = 1,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Bright Acoustic Piano"
    },
    InstrumentDef {
      _midiProgram        = 2,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Electric Grand Piano" 
    },
    InstrumentDef {
      _midiProgram        = 3,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Honky-tonk Piano"
    },
    InstrumentDef {
      _midiProgram        = 4,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Electric Piano 1"
    },
    InstrumentDef {
      _midiProgram        = 5,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Electric Piano 2"
    },
    InstrumentDef {
      _midiProgram        = 6,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Harpsichord"
    },
    InstrumentDef {
      _midiProgram        = 7,
      _defaultMidiChannel = 0,
      _scoreOrder         = 5.0,
      _defaultClef        = 0,
      _longName           = "Clavinet"
    },
    InstrumentDef {
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
      _midiProgram        = 11,
      _defaultMidiChannel = 0,
      _scoreOrder         = 4.0,
      _defaultClef        = 0,
      _longName           = "Vibraphone"
    },
    InstrumentDef {
      _midiProgram        = 12,
      _defaultMidiChannel = 0,
      _scoreOrder         = 4.0,
      _defaultClef        = 0,
      _longName           = "Marimba"
    },
    InstrumentDef {
      _midiProgram        = 13,
      _defaultMidiChannel = 0,
      _scoreOrder         = 4.0,
      _defaultClef        = 0,
      _longName           = "Xylophone"
    },
    InstrumentDef {
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
    -- (44, (0,  7.0, 0,  "Tremolo Strings")),
    -- (45, (0,  7.0, 0,  "Pizzicato Strings")),
    -- (46, (11,  5.9, 0,  "Orchestral Harp")),
    -- 
    -- (47, (8,  2.5, 2,  "Timpani")),
    -- 
    -- (48, (0,  7.0, 0,  "String Ensemble 1")),
    -- (49, (0,  7.0, 0,  "String Ensemble 2")),
    -- (50, (0,  7.0, 0,  "Synth Strings 1")),
    -- (51, (0,  7.0, 0,  "Synth Strings 2")),
    -- 
    -- (52, (0,  1.0, 0,  "Choir Aahs")),
    -- (53, (0,  1.0, 0,  "Voice Oohs")),
    -- (54, (0,  1.0, 0,  "Synth Choir")),
    -- (55, (0,  1.0, 0,  "Orchestra Hit")),
    -- 
    -- (56, (5,  2.2, 0,  "Trumpet in Bb")),
    -- (57, (6,  2.3, 2,  "Trombone")),
    -- (58, (7,  2.4, 2,  "Tuba")),
    -- (59, (0,  2.2, 0,  "Muted Trumpet")),
    -- -- (60, (4,  2.1, 0,  "French Horn")),
    -- (60, (4,  2.1, 0,  "Horn in F")),
    -- (61, (0,  2.0, 0,  "Brass Section")),
    -- (62, (0,  2.0, 0,  "Synth Brass 1")),
    -- (63, (0,  2.0, 0,  "Synth Brass 2")),
    -- 
    -- (64, (0,  1.51, 0,  "Soprano Sax")),
    -- (65, (0,  1.52, 0,  "Alto Sax")),
    -- (66, (0,  1.53, 0,  "Tenor Sax")),
    -- (67, (0,  1.54, 0,  "Baritone Sax")),
    -- (68, (1,  1.3, 0,  "Oboe")),
    -- (69, (1,  1.4, 0,  "English Horn")),
    -- (70, (3,  1.7, 2,  "Bassoon")),
    -- (71, (2,  1.6, 0,  "Clarinet in Bb")),
    -- 
    -- (72, (0,  1.1, 0,  "Piccolo")),
    -- (73, (0,  1.2, 0,  "Flute")),
    -- (74, (0,  1.0, 0,  "Recorder")),
    -- (75, (0,  1.0, 0,  "Pan Flute")),
    -- (76, (0,  1.0, 0,  "Blown bottle")),
    -- (77, (0,  1.0, 0,  "Shakuhachi")),
    -- (78, (0,  1.0, 0,  "Whistle")),
    -- (79, (0,  1.0, 0,  "Ocarina")),
    -- 
    -- (80, (0,  1.0, 0,  "Lead 1 (square)")),
    -- (81, (0,  1.0, 0,  "Lead 2 (sawtooth)")),
    -- (82, (0,  1.0, 0,  "Lead 3 (calliope)")),
    -- (83, (0,  1.0, 0,  "Lead 4 (chiff)")),
    -- (84, (0,  1.0, 0,  "Lead 5 (charang)")),
    -- (85, (0,  1.0, 0,  "Lead 6 (voice)")),
    -- (86, (0,  1.0, 0,  "Lead 7 (fifths)")),
    -- (87, (0,  1.0, 0,  "Lead 8 (bass + lead)")),
    -- 
    -- (88, (0,  1.0, 0,  "Pad 1 (new age)")),
    -- (89, (0,  1.0, 0,  "Pad 2 (warm)")),
    -- (90, (0,  1.0, 0,  "Pad 3 (polysynth)")),
    -- (91, (0,  1.0, 0,  "Pad 4 (choir)")),
    -- (92, (0,  1.0, 0,  "Pad 5 (bowed)")),
    -- (93, (0,  1.0, 0,  "Pad 6 (metallic)")),
    -- (94, (0,  1.0, 0,  "Pad 7 (halo)")),
    -- (95, (0,  1.0, 0,  "Pad 8 (sweep)")),
    -- 
    -- (96, (0,  1.0, 0,  "FX 1 (rain)")),
    -- (97, (0,  1.0, 0,  "FX 2 (soundtrack)")),
    -- (98, (0,  1.0, 0,  "FX 3 (crystal)")),
    -- (99, (0,  1.0, 0,  "FX 4 (atmosphere)")),
    -- (100, (0,  1.0, 0,  "FX 5 (brightness)")),
    -- (101, (0,  1.0, 0,  "FX 6 (goblins)")),
    -- (102, (0,  1.0, 0,  "FX 7 (echoes)")),
    -- (103, (0,  1.0, 0,  "FX 8 (sci-fi)")),
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
    -- 
    ]



{-
-- (midi program, (def midi ch, score order, def clef 0=g/1=c/2=f, name))

gmInstrs :: [(Int, (Int, Double, Int, String))]
gmInstrs = [
    -- (0, (0,  5.0, 0,  "Acoustic Grand Piano")),
    (0, (0,  5.0, 0,  "Piano")),
    (1, (0,  5.0, 0,  "Bright Acoustic Piano")),
    (2, (0,  5.0, 0,  "Electric Grand Piano")),
    (3, (0,  5.0, 0,  "Honky-tonk Piano")),
    (4, (0,  5.0, 0,  "Electric Piano 1")),
    (5, (0,  5.0, 0,  "Electric Piano 2")),
    (6, (0,  5.0, 0,  "Harpsichord")),
    (7, (0,  5.0, 0,  "Clavinet")),

    (8, (0,  5.0, 0,  "Celesta")),
    (9, (0,  5.0, 0,  "Glockenspiel")),
    (9, (0,  5.0, 0,  "Music Box")),
    (11, (0,  4.0, 0,  "Vibraphone")),
    (12, (0,  4.0, 0,  "Marimba")),
    (13, (0,  4.0, 0,  "Xylophone")),
    (14, (0,  4.0, 0,  "Tubular Bells")),
    (15, (0,  4.0, 0,  "Dulcimer")),

    (16, (0,  5.0, 0,  "Drawbar Organ")),
    (17, (0,  5.0, 0,  "Percussive Organ")),
    (18, (0,  5.0, 0,  "Rock Organ")),
    (19, (0,  5.0, 0,  "Church Organ")),
    (20, (0,  5.0, 0,  "Reed Organ")),
    (21, (0,  5.0, 0,  "Accordion")),
    (22, (0,  5.0, 0,  "Harmonica")),
    (23, (0,  5.0, 0,  "Tango Accordion")),

    (24, (0,  5.0, 0,  "Acoustic Guitar (nylon)")),
    (25, (0,  5.0, 0,  "Acoustic Guitar (steel)")),
    (26, (0,  5.0, 0,  "Electric Guitar (jazz)")),
    (27, (0,  5.0, 0,  "Electric Guitar (clean)")),
    (28, (0,  5.0, 0,  "Electric Guitar (muted)")),
    (29, (0,  5.0, 0,  "Overdriven Guitar")),
    (30, (0,  5.0, 0,  "Distortion Guitar")),
    (31, (0,  5.0, 0,  "Guitar Harmonics")),

    (32, (0,  8.0, 2,  "Acoustic Bass")),
    (33, (0,  8.0, 2,  "Electric Bass (finger)")),
    (34, (0,  8.0, 2,  "Electric Bass (pick)")),
    (35, (0,  8.0, 2,  "Fretless Bass")),
    (36, (0,  8.0, 2,  "Slap Bass 1")),
    (37, (0,  8.0, 2,  "Slap Bass 2")),
    (38, (0,  8.0, 2,  "Synth Bass 1")),
    (39, (0,  8.0, 2,  "Synth Bass 2")),

    (40, (12,  7.1, 0,  "Violin")),
    (41, (13,  7.2, 1,  "Viola")),
    -- (42, (0,  1.0, 0,  "Cello")),
    (42, (14,  7.3, 2,  "Violoncello")),
    (43, (15,  7.4, 2,  "Contrabass")),
    (44, (0,  7.0, 0,  "Tremolo Strings")),
    (45, (0,  7.0, 0,  "Pizzicato Strings")),
    (46, (11,  5.9, 0,  "Orchestral Harp")),

    (47, (8,  2.5, 2,  "Timpani")),

    (48, (0,  7.0, 0,  "String Ensemble 1")),
    (49, (0,  7.0, 0,  "String Ensemble 2")),
    (50, (0,  7.0, 0,  "Synth Strings 1")),
    (51, (0,  7.0, 0,  "Synth Strings 2")),

    (52, (0,  1.0, 0,  "Choir Aahs")),
    (53, (0,  1.0, 0,  "Voice Oohs")),
    (54, (0,  1.0, 0,  "Synth Choir")),
    (55, (0,  1.0, 0,  "Orchestra Hit")),

    (56, (5,  2.2, 0,  "Trumpet in Bb")),
    (57, (6,  2.3, 2,  "Trombone")),
    (58, (7,  2.4, 2,  "Tuba")),
    (59, (0,  2.2, 0,  "Muted Trumpet")),
    -- (60, (4,  2.1, 0,  "French Horn")),
    (60, (4,  2.1, 0,  "Horn in F")),
    (61, (0,  2.0, 0,  "Brass Section")),
    (62, (0,  2.0, 0,  "Synth Brass 1")),
    (63, (0,  2.0, 0,  "Synth Brass 2")),

    (64, (0,  1.51, 0,  "Soprano Sax")),
    (65, (0,  1.52, 0,  "Alto Sax")),
    (66, (0,  1.53, 0,  "Tenor Sax")),
    (67, (0,  1.54, 0,  "Baritone Sax")),
    (68, (1,  1.3, 0,  "Oboe")),
    (69, (1,  1.4, 0,  "English Horn")),
    (70, (3,  1.7, 2,  "Bassoon")),
    (71, (2,  1.6, 0,  "Clarinet in Bb")),

    (72, (0,  1.1, 0,  "Piccolo")),
    (73, (0,  1.2, 0,  "Flute")),
    (74, (0,  1.0, 0,  "Recorder")),
    (75, (0,  1.0, 0,  "Pan Flute")),
    (76, (0,  1.0, 0,  "Blown bottle")),
    (77, (0,  1.0, 0,  "Shakuhachi")),
    (78, (0,  1.0, 0,  "Whistle")),
    (79, (0,  1.0, 0,  "Ocarina")),

    (80, (0,  1.0, 0,  "Lead 1 (square)")),
    (81, (0,  1.0, 0,  "Lead 2 (sawtooth)")),
    (82, (0,  1.0, 0,  "Lead 3 (calliope)")),
    (83, (0,  1.0, 0,  "Lead 4 (chiff)")),
    (84, (0,  1.0, 0,  "Lead 5 (charang)")),
    (85, (0,  1.0, 0,  "Lead 6 (voice)")),
    (86, (0,  1.0, 0,  "Lead 7 (fifths)")),
    (87, (0,  1.0, 0,  "Lead 8 (bass + lead)")),

    (88, (0,  1.0, 0,  "Pad 1 (new age)")),
    (89, (0,  1.0, 0,  "Pad 2 (warm)")),
    (90, (0,  1.0, 0,  "Pad 3 (polysynth)")),
    (91, (0,  1.0, 0,  "Pad 4 (choir)")),
    (92, (0,  1.0, 0,  "Pad 5 (bowed)")),
    (93, (0,  1.0, 0,  "Pad 6 (metallic)")),
    (94, (0,  1.0, 0,  "Pad 7 (halo)")),
    (95, (0,  1.0, 0,  "Pad 8 (sweep)")),

    (96, (0,  1.0, 0,  "FX 1 (rain)")),
    (97, (0,  1.0, 0,  "FX 2 (soundtrack)")),
    (98, (0,  1.0, 0,  "FX 3 (crystal)")),
    (99, (0,  1.0, 0,  "FX 4 (atmosphere)")),
    (100, (0,  1.0, 0,  "FX 5 (brightness)")),
    (101, (0,  1.0, 0,  "FX 6 (goblins)")),
    (102, (0,  1.0, 0,  "FX 7 (echoes)")),
    (103, (0,  1.0, 0,  "FX 8 (sci-fi)")),

    (104, (0,  1.0, 0,  "Sitar")),
    (105, (0,  1.0, 0,  "Banjo")),
    (106, (0,  1.0, 0,  "Shamisen")),
    (107, (0,  1.0, 0,  "Koto")),
    (108, (0,  1.0, 0,  "Kalimba")),
    (109, (0,  1.0, 0,  "Bagpipe")),
    (110, (0,  1.0, 0,  "Fiddle")),
    (111, (0,  1.0, 0,  "Shanai")),

    (112, (0,  1.0, 0,  "Tinkle Bell")),
    (113, (0,  1.0, 0,  "Agogo")),
    (114, (0,  1.0, 0,  "Steel Drums")),
    (115, (0,  1.0, 0,  "Woodblock")),
    (116, (0,  1.0, 0,  "Taiko Drum")),
    (117, (0,  1.0, 0,  "Melodic Tom")),
    (118, (0,  1.0, 0,  "Synth Drum")),
    (119, (0,  1.0, 0,  "Reverse Cymbal")),

    (120, (0,  1.0, 0,  "Guitar Fret Noise")),
    (121, (0,  1.0, 0,  "Breath Noise")),
    (122, (0,  1.0, 0,  "Seashore")),
    (123, (0,  1.0, 0,  "Bird Tweet")),
    (124, (0,  1.0, 0,  "Telephone Ring")),
    (125, (0,  1.0, 0,  "Helicopter")),
    (126, (0,  1.0, 0,  "Applause")),
    (127, (0,  1.0, 0,  "Gunshot"))
    ]
-}

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
