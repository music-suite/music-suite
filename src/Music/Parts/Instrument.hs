
-- | Representation of musical instruments.
module Music.Parts.Instrument (
        Instrument(..),
        -- TODO hide impl
        allowedClefs,
        playableRange,
        comfortableRange,
        playableDynamics,

        -- * Name etc
        instrumentName,
        fullName,
        shortName,
        transposition,
        transpositionString,
        
        -- TODO
        gmInstrs,
        getInstrumentFilePath,
        getDataPath,
  ) where

import qualified Paths_music_parts
import qualified System.IO.Unsafe
import           Control.Applicative
import           Control.Lens                    (toListOf)
import           Data.Default
import           Data.Functor.Adjunction         (unzipR)
import qualified Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Option.Instances
import           Data.Set                        (Set)
import           Data.Traversable                (traverse)
import           Data.Typeable
import           Music.Dynamics                  (Dynamics)
import           Music.Pitch                     (Ambitus, Clef, trebleClef, bassClef)
import           Music.Pitch.Common              (Interval, Pitch)
import           Text.Numeral.Roman              (toRoman)

{-
Semantically, our instrument type is the MusicXML Standard Sounds 3.0
See http://www.musicxml.com/for-developers/standard-sounds/

We use the following map between GM2 and MusicXML Standard Sounds.
-}
getInstrumentFilePath = System.IO.Unsafe.unsafePerformIO $ getDataPath "data/instruments.csv"
getDataPath = Paths_music_parts.getDataFileName

-- | An 'Instrument' represents the set of all instruments of a given type.
data Instrument
    = StdInstrument Int
    | OtherInstrument String
    deriving (Eq)

instance Show Instrument where
    show (StdInstrument x) = fromMaybe "(unknown)" $ gmInstrName x
    show (OtherInstrument str) = go str
        where
            go "wind.flutes.flute.alto"   = "Alto Flute"
            go "wind.flutes.flute.bass"   = "Bass Flute"
            go "wind.reed.clarinet.eflat" = "Clarinet in Eb"
            go "wind.reed.clarinet.bass"  = "Bass Clarinet in Bb"
            go "wind.reed.contrabassoon"  = "Contrabassoon"
            go x = x
instance Enum Instrument where
    toEnum = StdInstrument
    fromEnum (StdInstrument x) = x
    fromEnum (OtherInstrument _) = error "Instrument.fromEnum used on unknown instrument"

instance Ord Instrument where
    StdInstrument x   `compare` StdInstrument   y = gmScoreOrder x `compare` gmScoreOrder y
    OtherInstrument x `compare` OtherInstrument y = x `compare` y
    StdInstrument x   `compare` OtherInstrument y = LT
    OtherInstrument x `compare` StdInstrument   y = GT

-- | This instance is quite arbitrary but very handy.
instance Default Instrument where
    def = StdInstrument 0


-- TODO consolidate

gmClef :: Int -> Int
gmMidiChannel :: Int -> Int
gmScoreOrder :: Int -> Double

gmMidiChannel = fromMaybe 0 . fmap get . (`lookup` gmInstrs)
    where get (x,_,_,_) = x

gmScoreOrder = fromMaybe 0 . fmap get . (`lookup` gmInstrs)
    where get (_,x,_,_) = x

gmClef = fromMaybe 0 . fmap get . (`lookup` gmInstrs)
    where get (_,_,x,_) = x


gmInstrName :: Int -> Maybe String
gmInstrName = fmap get . (`lookup` gmInstrs)
    where get (_,_,_,x) = x

-- (midi program, (def midi ch, score order, def clef 0=g/1=c/2=f, name))
{-
    Score order:
        Woodwinds:      1
        Brass:          2
        Timpani:        3
        Percussion:     4
        Keyboard/Harp   5
        Singers         6
        Strings:        7
        Bass:           8
-}
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



allowedClefs      :: Instrument -> Set Clef
allowedClefs = error "No allowedClefs"

data BracketType = Bracket | Brace | SubBracket
data StaffLayout = Staff Clef | Staves BracketType [StaffLayout]

pianoStaff :: StaffLayout
pianoStaff = Staves Brace [Staff trebleClef, Staff bassClef]


playableRange     :: Instrument -> Ambitus Pitch
playableRange = error "No playableRange"

comfortableRange  :: Instrument -> Ambitus Pitch
comfortableRange = error "No comfortableRange"

playableDynamics :: Instrument -> Pitch -> Dynamics
playableDynamics = error "No playableDynamics"

instrumentName              :: Instrument -> String
instrumentName = error "No name"

fullName          :: Instrument -> String
fullName = error "No fullName"

shortName         :: Instrument -> String
shortName = error "No shortName"

-- sounding .-. written, i.e. -P5 for clarinet
transposition     :: Instrument -> Interval
transposition = error "No transposition"

transpositionString :: Instrument -> String
transpositionString = error "No transpositionString"

