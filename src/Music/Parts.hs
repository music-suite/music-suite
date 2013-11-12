
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveDataTypeable,
    TypeFamilies #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Part representation.
--
-------------------------------------------------------------------------------------

module Music.Parts (
        -- * Terminology
        -- $terminology
        
        -- * Subparts
        Division,
        divisions,
        getDivision,
        Subpart,

        -- * Instruments
        Instrument,

        -- * Parts
        Solo(..),
        Part(..),
        divide,
        subPart,
        
        defaultClef,
        defaultMidiProgram,
        defaultMidiChannel,
        defaultMidiNote,

        -- * Basic
        BasicPart,

  ) where

import Data.Default
import Data.Typeable
import Data.Maybe

{- $terminology

Parts represent a subset of a group of performers. It is mainly used for instrumental and
vocal music, but some concetps may be useful in electronic music as well.

-   'Section' refers to a set of instrumentfamilies related by sound production method (i.e. woodwind).

-   'Family' refers to a set of instrument or voice types, which typically differ in size (i.e. saxophones).

-   'Instrument' refers to a set of instruments or voice types of a given type (i.e. soprano saxophones).
    Perhaps confusingly, this includes vocal types such as alto, tenor etc as well. However, there is
    no good general term that incorporate both /instrument/ and /voice type/.

-   A 'Part' is made up of an 'Instrument' and a 'Division' (i.e. Violin I). Solo parts are treated
    separately, so i.e. /Violin solo II/ (as in a double concerto) is distinct from /Violin II/.

-}
    


{-
    For each part we want to know:
        - Classification:
            - Type: (i.e. woodwind)
            - Family: (i.e. saxophone)
            - Range: (i.e. tenor)            
        - Range (i.e. [c_:e'])
        - Transposition:
            sounding = written .+^ transp 
        - Suggested clefs
-}

-- |
-- A division represents a subset of a finite group of performers.
--
-- For example a group may be divided into three equal divisions,
-- designated @(0, 3)@, @(1, 3)@ and @(2, 3)@ respectively.
--
newtype Division = Division { getDivision :: (Int, Int) }
    deriving (Eq, Ord, Show)
instance Default Division where
    def = Division (0,1)

-- | Get all possible divisions for a given divisor in ascending order.
divisions :: Int -> [Division]
divisions n = [Division (x,n) | x <- [0..n-1]]

-- | Divide a part into @n@ subparts.
divide :: Int -> Part -> [Part]
divide = undefined
-- divide n = [Division (x,n) | x <- [0..n]]

-- |
-- A subpart is a potentially infinite sequence of divisions, each typically
-- designated using a new index type, i.e. @I.1.2@.
--
-- The empty subpart (also known as 'def') represents all the players of the group,
-- or in the context of 'Part', all players of the given instrument.
-- 
type Subpart = [Division]


-- | An 'Instrument' represents the set of all instruments of a given type.
data Instrument 
    = StdInstrument Int
    | OtherInstrument String
    deriving (Eq, Ord, Show)
instance Enum Instrument where
    toEnum = StdInstrument
    fromEnum (StdInstrument x) = x
    fromEnum (OtherInstrument _) = error "Instrument.fromEnum used on unknown instrument"

-- | This instance is quite arbitrary but very handy.
instance Default Instrument where
    def = StdInstrument 0

data Solo
    = Solo
    | Tutti
    deriving (Eq, Ord, Show, Enum)

instance Default Solo where
    def = Tutti 


-- | A part is a subdivided group of instruments of a given type.
--
data Part = Part Solo Instrument Subpart
    deriving (Eq, Ord, Show)

-- FIXME bad instance (?)
instance Enum Part where
    toEnum x = Part Tutti (toEnum x) []
    fromEnum (Part solo instr subp) = fromEnum instr

instance Default Part where
    def = Part def def def

-- | 
-- @a \`subPart\` b@ holds if the set of players represented by a is an improper subset of the 
-- set of players represented by b.
subPart :: Part -> Part -> Bool
subPart = undefined

defaultClef :: Part -> Int
defaultMidiProgram :: Part -> Int
defaultMidiChannel :: Part -> Int
defaultMidiNote :: Part -> Int
defaultClef = undefined
defaultMidiProgram = undefined
defaultMidiChannel = undefined
defaultMidiNote = undefined

-- instance Show Instrument where

gmInstrs :: [(Int, String)]
gmInstrs = [
    (0, "Acoustic Grand Piano"),
    (1, "Bright Acoustic Piano"),
    (2, "Electric Grand Piano"),
    (3, "Honky-tonk Piano"),
    (4, "Electric Piano 1"),
    (5, "Electric Piano 2"),
    (6, "Harpsichord"),
    (7, "Clavinet"),

    (8, "Celesta"),
    (9, "Glockenspiel"),
    (10,  "Music Box"),
    (11, "Vibraphone"),
    (12, "Marimba"),
    (13, "Xylophone"),
    (14, "Tubular Bells"),
    (15, "Dulcimer"),

    (16, "Drawbar Organ"),
    (17, "Percussive Organ"),
    (18, "Rock Organ"),
    (19, "Church Organ"),
    (20, "Reed Organ"),
    (21, "Accordion"),
    (22, "Harmonica"),
    (23, "Tango Accordion"),

    (24, "Acoustic Guitar (nylon)"),
    (25, "Acoustic Guitar (steel)"),
    (26, "Electric Guitar (jazz)"),
    (27, "Electric Guitar (clean)"),
    (28, "Electric Guitar (muted)"),
    (29, "Overdriven Guitar"),
    (30, "Distortion Guitar"),
    (31, "Guitar Harmonics"),

    (32, "Acoustic Bass"),
    (33, "Electric Bass (finger)"),
    (34, "Electric Bass (pick)"),
    (35, "Fretless Bass"),
    (36, "Slap Bass 1"),
    (37, "Slap Bass 2"),
    (38, "Synth Bass 1"),
    (39, "Synth Bass 2"),

    (40, "Violin"),
    (41, "Viola"),
    (42, "Cello"),
    (43, "Contrabass"),
    (44, "Tremolo Strings"),
    (45, "Pizzicato Strings"),
    (46, "Orchestral Harp"),
    (47, "Timpani"),

    (48, "String Ensemble 1"),
    (49, "String Ensemble 2"),
    (50, "Synth Strings 1"),
    (51, "Synth Strings 2"),
    (52, "Choir Aahs"),
    (53, "Voice Oohs"),
    (54, "Synth Choir"),
    (55, "Orchestra Hit"),

    (56, "Trumpet"),
    (57, "Trombone"),
    (58, "Tuba"),
    (59, "Muted Trumpet"),
    (60, "French Horn"),
    (61, "Brass Section"),
    (62, "Synth Brass 1"),
    (63, "Synth Brass 2"),

    (64, "Soprano Sax"),
    (65, "Alto Sax"),
    (66, "Tenor Sax"),
    (67, "Baritone Sax"),
    (68, "Oboe"),
    (69, "English Horn"),
    (70, "Bassoon"),
    (71, "Clarinet"),

    (72, "Piccolo"),
    (73, "Flute"),
    (74, "Recorder"),
    (75, "Pan Flute"),
    (76, "Blown bottle"),
    (77, "Shakuhachi"),
    (78, "Whistle"),
    (79, "Ocarina"),

    (80, "Lead 1 (square)"),
    (81, "Lead 2 (sawtooth)"),
    (82, "Lead 3 (calliope)"),
    (83, "Lead 4 (chiff)"),
    (84, "Lead 5 (charang)"),
    (85, "Lead 6 (voice)"),
    (86, "Lead 7 (fifths)"),
    (87, "Lead 8 (bass + lead)"),

    (88, "Pad 1 (new age)"),
    (89, "Pad 2 (warm)"),
    (90, "Pad 3 (polysynth)"),
    (91, "Pad 4 (choir)"),
    (92, "Pad 5 (bowed)"),
    (93, "Pad 6 (metallic)"),
    (94, "Pad 7 (halo)"),
    (95, "Pad 8 (sweep)"),

    (96, "FX 1 (rain)"),
    (97, "FX 2 (soundtrack)"),
    (98, "FX 3 (crystal)"),
    (99, "FX 4 (atmosphere)"),
    (100, "FX 5 (brightness)"),
    (101, "FX 6 (goblins)"),
    (102, "FX 7 (echoes)"),
    (103, "FX 8 (sci-fi)"),

    (104, "Sitar"),
    (105, "Banjo"),
    (106, "Shamisen"),
    (107, "Koto"),
    (108, "Kalimba"),
    (109, "Bagpipe"),
    (110, "Fiddle"),
    (111, "Shanai"),

    (112, "Tinkle Bell"),
    (113, "Agogo"),
    (114, "Steel Drums"),
    (115, "Woodblock"),
    (116, "Taiko Drum"),
    (117, "Melodic Tom"),
    (118, "Synth Drum"),
    (119, "Reverse Cymbal"),

    (120, "Guitar Fret Noise"),
    (121, "Breath Noise"),
    (122, "Seashore"),
    (123, "Bird Tweet"),
    (124, "Telephone Ring"),
    (125, "Helicopter"),
    (126, "Applause"),
    (127, "Gunshot")
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





-- 
-- data Section
--     = Woodwind
--     | Brass
--     | Percussion
--     | Keyboard
--     | Voices
--     | Strings
-- 
-- data VoicePart
--     = Soprano
--     | MezzoSoprano
--     | Alto
--     | Tenor
--     | Baritone
--     | Bass
-- 
-- 
-- data GMInstrumentType
--     = GMPiano
--     | GMChromaticPercussion
--     | GMOrgan
--     | GMGuitar
--     | GMBass
--     | GMStrings
--     | GMEnsemble
--     | GMBrass
--     | GMReed
--     | GMPipe
--     | GMSynthLead
--     | GMSynthPad
--     | GMSynthEffects
--     | GMEthnic
--     | GMPercussive
--     | GMSoundEffects   


    
{-
    ## Terminology: Voice vs Part
    
    A voice is a container of notes (non-overlapping)
    
    A part is an identifier for a set of singers/musicians AND all the notes in a score
    designated for this set of performers. Part extraction has the type
    
        extractParts :: HasPart a => Score a -> [Score a]
    
    I.e. in a score for piano and ensemble, certain notes may be *in the piano part*, i.e.
    designated for the piano. Typically, a part is monophonic or polyphonic. A monophonic
    part is a voice, i.e.

        -- | Are there overlapping notes?
        isMonophonic :: Score a -> Bool

        -- | Fails for polyphonic scores.
        scoreToVoice :: Score a -> Voice (Maybe a)
    
    A polyphonic score contains several voices, i.e.

        scoreToVoices :: Score a -> [Voice (Maybe a)]  


    A part is any type a that satisfies (Ord a, Show a).
    Optionally, we may add a contraint (HasPartName a), i.e.

        class HasPartName a where
            partName :: a -> String
            partAbbr :: a -> String
    
    
    These contraints are used when printing scores (to get the order of the parts and their name).

        Vln1, Vln2 etc.

    Often we want to group parts, i.e.

        Chorus {
            Sop  { Sop1 Sop2 }
            Alto { Alto1 Alto2 }
            Ten  { Ten1 Ten 2 }
            Bass { Bass1 Bass2 }
        }
        Orchestra {
            Woodwinds { ... }
            Brass { ... }
            Perc { ... }
            Strings { ... }
        }


    isInGroup :: Group -> Part -> Bool
    partGroups :: Part -> [Group]


    partGroup :: (Part -> [Group] -> a) -> Group -> a
    tree :: (a -> [Tree a] -> b) -> Tree a -> b
    
    
    data MyPart
        = Fl
        | Sop
        | Vl1
        | Vl2
        | Pno

-}


newtype BasicPart = BasicPart { getBasicPart :: Integer }
    deriving (Eq, Ord, Num, Integral, Real, Enum, Typeable)

instance Default BasicPart where def = BasicPart 0

instance Show BasicPart where
    show _ = ""
