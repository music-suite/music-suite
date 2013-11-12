
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
        Instrument(..),
        -- TODO hide impl

        -- * Parts
        Solo(..),
        Part(..),
        divide,
        containsPart,
        containsSubpart,
        solo,
        tutti,
        
        -- ** Instruments etc
        violin,
        viola,
        cello,
        bass,
        tubularBells,

        -- ** Default values
        defaultClef,
        defaultMidiProgram,
        defaultMidiChannel,
        defaultMidiNote,


        -- * Basic
        -- TODO move
        BasicPart,

  ) where

import Data.Default
import Data.Semigroup
import Data.Typeable
import Data.Maybe
import Text.Numeral.Roman (toRoman)
import qualified Data.List

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

showDivisionR :: Division -> String
showDivisionR = toRoman . succ . fst . getDivision

showDivision :: Division -> String
showDivision  = show . succ . fst . getDivision

-- | Get all possible divisions for a given divisor in ascending order.
divisions :: Int -> [Division]
divisions n = [Division (x,n) | x <- [0..n-1]]

-- | Divide a part into @n@ subparts.
divide :: Int -> Part -> [Part]
divide n (Part solo instr subp) = fmap (\x -> Part solo instr (subp <> Subpart [x])) $ divisions n

-- |
-- A subpart is a potentially infinite sequence of divisions, each typically
-- designated using a new index type, i.e. @I.1.2@.
--
-- The empty subpart (also known as 'def') represents all the players of the group,
-- or in the context of 'Part', all players of the given instrument.
-- 
newtype Subpart = Subpart [Division]
    deriving (Eq, Ord, Default, Semigroup, Monoid)

instance Show Subpart where
    show (Subpart ps) = Data.List.intercalate "." $ mapFR showDivisionR showDivision $ ps
        where
            mapFR f g []     = []
            mapFR f g (x:xs) = f x : fmap g xs

-- | An 'Instrument' represents the set of all instruments of a given type.
data Instrument 
    = StdInstrument Int
    | OtherInstrument String
    deriving (Eq)

instance Show Instrument where      
    show (StdInstrument x) = fromMaybe "(unknown)" $ gmInstrName x
    show (OtherInstrument str) = str
instance Enum Instrument where
    toEnum = StdInstrument
    fromEnum (StdInstrument x) = x
    fromEnum (OtherInstrument _) = error "Instrument.fromEnum used on unknown instrument"

instance Ord Instrument where
    StdInstrument x `compare` StdInstrument y = gmScoreOrder x `compare` gmScoreOrder y

-- | This instance is quite arbitrary but very handy.
instance Default Instrument where
    def = StdInstrument 0

data Solo
    = Solo
    | Tutti
    deriving (Eq, Show, Ord, Enum)

instance Default Solo where
    def = Tutti 


-- | A part is a subdivided group of instruments of a given type.
--
data Part = Part Solo Instrument Subpart
    deriving (Eq, Ord)

instance Show Part where
    show (Part Solo instr subp) = "Solo " ++ show instr ++ addS (show subp)
        where
            addS "" = ""
            addS x = " " ++ x
    show (Part _ instr subp)    = show instr ++ addS (show subp)
        where
            addS "" = ""
            addS x = " " ++ x

-- FIXME bad instance (?)
instance Enum Part where
    toEnum x = Part Tutti (toEnum x) def
    fromEnum (Part solo instr subp) = fromEnum instr

instance Default Part where
    def = Part def def def

-- | 
-- @a \`containsPart\` b@ holds if the set of players represented by a is an improper subset of the 
-- set of players represented by b.
containsPart :: Part -> Part -> Bool
Part solo1 instr1 subp1 `containsPart` Part solo2 instr2 subp2 = 
        solo1 == solo2 
        && instr1 == instr2
        && subp1 `containsSubpart` subp2

containsSubpart :: Subpart -> Subpart -> Bool
Subpart x `containsSubpart` Subpart y = y `Data.List.isPrefixOf` x

solo instr = Part Solo instr def
tutti instr = Part Tutti instr def
violin = StdInstrument 40
viola = StdInstrument 41
cello = StdInstrument 42
bass = StdInstrument 43
tubularBells = StdInstrument 14

defaultClef :: Part -> Int
defaultMidiNote :: Part -> Int
defaultMidiProgram :: Part -> Int
defaultMidiChannel :: Part -> Int
defaultScoreOrder :: Part -> Double

defaultMidiNote _ = 0

defaultMidiProgram (Part _ (StdInstrument x) _) = x

defaultMidiChannel = fromMaybe 0 . fmap get . (`lookup` gmInstrs) . defaultMidiProgram
    where get (x,_,_,_) = x

defaultScoreOrder = fromMaybe 0 . fmap get . (`lookup` gmInstrs) . defaultMidiProgram
    where get (_,x,_,_) = x

defaultClef = fromMaybe 0 . fmap get . (`lookup` gmInstrs) . defaultMidiProgram
    where get (_,_,x,_) = x



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
        Woodwonds:      1
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
    (0, (0,  5.0, 0,  "Acoustic Grand Piano")),
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

    (32, (0,  8.0, 0,  "Acoustic Bass")),
    (33, (0,  8.0, 0,  "Electric Bass (finger)")),
    (34, (0,  8.0, 0,  "Electric Bass (pick)")),
    (35, (0,  8.0, 0,  "Fretless Bass")),
    (36, (0,  8.0, 0,  "Slap Bass 1")),
    (37, (0,  8.0, 0,  "Slap Bass 2")),
    (38, (0,  8.0, 0,  "Synth Bass 1")),
    (39, (0,  8.0, 0,  "Synth Bass 2")),

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

    (56, (5,  2.2, 0,  "Trumpet")),
    (57, (6,  2.3, 2,  "Trombone")),
    (58, (7,  2.4, 2,  "Tuba")),
    (59, (0,  2.2, 0,  "Muted Trumpet")),
    -- (60, (4,  2.1, 0,  "French Horn")),
    (60, (4,  2.1, 0,  "Horn")),
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
    (71, (2,  1.6, 0,  "Clarinet")),

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
