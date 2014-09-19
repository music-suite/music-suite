
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2015
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides various representaitons of musical instruments, subdivisions and parts.
--
-------------------------------------------------------------------------------------

module Music.Parts (
        -- * Terminology
        -- $terminology


        -- * Subparts
        module Music.Parts.Division,
        module Music.Parts.Subpart,

        -- * Solo vs. tutti
        module Music.Parts.Solo,
        
        -- * Instruments
        Instrument(..),
        -- TODO hide impl

        -- * Parts
        Part(..),
        divide,
        containsPart,
        solo,
        tutti,

        -- ** Instruments etc
        piccoloFlute,
        flute,
        altoFlute,
        bassFlute,

        oboe,
        corAnglais,
        heckelphone,

        ebClarinet,
        clarinet,
        aClarinet,
        bassClarinet,

        sopranoSax,
        altoSax,
        tenorSax,
        baritoneSax,

        bassoon,
        contraBassoon,

        horn,
        piccoloTrumpet,
        trumpet,
        bassTrumpet,
        altoTrombone,
        tenorTrombone,
        trombone,
        bassTrombone,
        tuba,

        timpani,
        piano,

        celesta,
        glockenspiel,
        vibraphone,
        marimba,
        xylophone,
        xylorimba,
        tubularBells,
        dulcimer,

        accordion,
        harmonica,

        violin,
        viola,
        cello,
        doubleBass,

        -- ** Ensembles
        piccoloFlutes,
        flutes,
        oboes,
        clarinets,
        bassoons,

        flutes1,
        flutes2,
        oboes1,
        oboes2,
        clarinets1,
        clarinets2,

        horns,
        highHorns,
        lowHorns,
        trumpets,
        trombones,
        trumpets1,
        trumpets2,
        trombones1,
        trombones2,
        tubas,

        violins,
        violins1,
        violins2,
        violas,
        cellos,
        doubleBasses,

        harp,

        -- ** Default values
        defaultClef,
        defaultMidiProgram,
        defaultMidiChannel,
        defaultMidiNote,


        -- * Basic
        module Music.Parts.Basic

  ) where

import           Control.Applicative
import           Control.Lens                    (toListOf)
import           Data.Default
import           Data.Functor.Adjunction         (unzipR)
import qualified Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Option.Instances
import           Data.Traversable                (traverse)
import           Data.Typeable
import           Text.Numeral.Roman              (toRoman)

import           Music.Parts.Basic
import           Music.Parts.Subpart
import           Music.Parts.Division
import           Music.Parts.Solo

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




-- | An 'Instrument' represents the set of all instruments of a given type.
data Instrument
    = StdInstrument Int
    | OtherInstrument String
    deriving (Eq)

instance Show Instrument where
    show (StdInstrument x) = fromMaybe "(unknown)" $ gmInstrName x
    show (OtherInstrument str) = str
instance Enum Instrument where
    toEnum = StdInstrument
    fromEnum (StdInstrument x) = x
    fromEnum (OtherInstrument _) = error "Instrument.fromEnum used on unknown instrument"

instance Ord Instrument where
    StdInstrument x `compare` StdInstrument y = gmScoreOrder x `compare` gmScoreOrder y

-- | This instance is quite arbitrary but very handy.
instance Default Instrument where
    def = StdInstrument 0


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

-- Semantics: Monoid (Option . First)
instance Monoid Part where
  mappend x _ = x
  mempty = def
instance Semigroup Part where
  (<>) = mappend
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

-- | Divide a part into @n@ subparts.
divide :: Int -> Part -> [Part]
divide n (Part solo instr subp) = fmap (\x -> Part solo instr (subp <> Subpart [x])) $ divisions n

solo instr      = Part Solo instr def
tutti instr     = Part Tutti instr def


piccoloFlute    = StdInstrument 72
flute           = StdInstrument 73
altoFlute       = OtherInstrument "Woodwind.Flute.Alto"
bassFlute       = OtherInstrument "Woodwind.Flute.Bass"

oboe            = StdInstrument 68
corAnglais      = StdInstrument 69
heckelphone     = OtherInstrument "Woodwind.DoubleReed.Heckelphone"

ebClarinet      = OtherInstrument "Woodwind.SingleReed.Clarinet.Eb"
clarinet        = StdInstrument 71
aClarinet       = OtherInstrument "Woodwind.SingleReed.Clarinet.A"
bassClarinet    = OtherInstrument "Woodwind.SingleReed.Clarinet.Bass"

sopranoSax      = StdInstrument 64
altoSax         = StdInstrument 65
tenorSax        = StdInstrument 66
baritoneSax     = StdInstrument 67

bassoon         = StdInstrument 70
contraBassoon   = OtherInstrument "Woodwind.DoubleReed.Bassoon.Contra"

horn            = StdInstrument 60
piccoloTrumpet  = OtherInstrument "Brass.Trumpet.Piccolo"
trumpet         = StdInstrument 56
bassTrumpet     = OtherInstrument "Brass.Trumpet.Bass"
altoTrombone    = OtherInstrument "Brass.Trombone.Alto"
tenorTrombone   = StdInstrument 57
trombone        = StdInstrument 57
bassTrombone    = OtherInstrument "Brass.Trombone.Bass"
tuba            = StdInstrument 58

timpani         = StdInstrument 47
piano           = StdInstrument 0

celesta         = StdInstrument 8
glockenspiel    = StdInstrument 9
vibraphone      = StdInstrument 11
marimba         = StdInstrument 12
xylophone       = StdInstrument 13
xylorimba       = OtherInstrument "Percussion.Pitched.Xylorimba"
tubularBells    = StdInstrument 14
dulcimer        = StdInstrument 15

accordion       = StdInstrument 21
harmonica       = StdInstrument 22

violin          = StdInstrument 40
viola           = StdInstrument 41
cello           = StdInstrument 42
doubleBass      = StdInstrument 43



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
--     | Alto
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
--     | GMEthnic
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
        | Sop
        | Vl1
        | Vl2
        | Pno

-}
-- TODO move
-- instance Num a => Num (Option a) where
--   (+)       = liftA2 (+)
--   (-)       = liftA2 (-)
--   (*)       = liftA2 (*)
--   abs       = fmap abs
--   signum    = fmap signum
--   fromInteger = pure . fromInteger
-- instance Integral a => Integral (Option a) where
--   quotRem x y = unzipR $ liftA2 quotRem x y
--   toInteger = toInteger . get where get = (head.toListOf traverse)
-- instance Real a => Real (Option a) where
--   toRational = toRational . get where get = (head.toListOf traverse)
-- instance Enum a => Enum (Option a) where
--   fromEnum = fromEnum . get where get = (head.toListOf traverse)
--   toEnum = pure . toEnum
--
-- instance Num a => Num (First a) where
--   (+)       = liftA2 (+)
--   (-)       = liftA2 (-)
--   (*)       = liftA2 (*)
--   abs       = fmap abs
--   signum    = fmap signum
--   fromInteger = pure . fromInteger
-- instance Integral a => Integral (First a) where
--   quotRem x y = unzipR $ liftA2 quotRem x y
--   toInteger = toInteger . get where get = (head.toListOf traverse)
-- instance Real a => Real (First a) where
--   toRational = toRational . get where get = (head.toListOf traverse)
-- -- instance Enum a => Enum (First a) where
--   -- toEnum = toEnum . get where get = (head.toListOf traverse)
--   -- fromEnum = pure . fromEnum


piccoloFlutes = tutti piccoloFlute
flutes = tutti flute
oboes = tutti oboe
clarinets = tutti clarinet
bassoons = tutti bassoon

[flutes1, flutes2] = divide 2 flutes
[oboes1, oboes2] = divide 2 oboes
[clarinets1, clarinets2] = divide 2 clarinets


horns = tutti horn
highHorns = zipWith (!!) (repeat $ divide 4 horns) [0,2]
lowHorns = zipWith (!!) (repeat $ divide 4 horns) [1,3]
trumpets = tutti trumpet
trombones = tutti trombone
[trumpets1, trumpets2] = divide 2 trumpets
[trombones1, trombones2] = divide 2 trombones
tubas = tutti tuba

violins = tutti violin
[violins1, violins2] = divide 2 violins
violas = tutti viola
cellos = tutti cello
doubleBasses = tutti doubleBass

harp' = StdInstrument 46
harp = tutti harp'


{-
  <sound id="brass.alphorn"/>
  <sound id="brass.alto-horn"/>
  <sound id="brass.baritone-horn"/>
  <sound id="brass.bugle"/>
  <sound id="brass.bugle.alto"/>
  <sound id="brass.bugle.baritone"/>
  <sound id="brass.bugle.contrabass"/>
  <sound id="brass.bugle.euphonium-bugle"/>
  <sound id="brass.bugle.mellophone-bugle"/>
  <sound id="brass.bugle.soprano"/>
  <sound id="brass.cimbasso"/>
  <sound id="brass.conch-shell"/>
  <sound id="brass.cornet"/>
  <sound id="brass.cornet.soprano"/>
  <sound id="brass.cornett"/>
  <sound id="brass.cornett.tenor"/>
  <sound id="brass.cornettino"/>
  <sound id="brass.didgeridoo"/>
  <sound id="brass.euphonium"/>
  <sound id="brass.fiscorn"/>
  <sound id="brass.flugelhorn"/>
  <sound id="brass.french-horn"/>
  <sound id="brass.group"/>
  <sound id="brass.group.synth"/>
  <sound id="brass.helicon"/>
  <sound id="brass.horagai"/>
  <sound id="brass.kuhlohorn"/>
  <sound id="brass.mellophone"/>
  <sound id="brass.natural-horn"/>
  <sound id="brass.ophicleide"/>
  <sound id="brass.posthorn"/>
  <sound id="brass.rag-dung"/>
  <sound id="brass.sackbutt"/>
  <sound id="brass.sackbutt.alto"/>
  <sound id="brass.sackbutt.bass"/>
  <sound id="brass.sackbutt.tenor"/>
  <sound id="brass.saxhorn"/>
  <sound id="brass.serpent"/>
  <sound id="brass.shofar"/>
  <sound id="brass.sousaphone"/>
  <sound id="brass.trombone"/>
  <sound id="brass.trombone.alto"/>
  <sound id="brass.trombone.bass"/>
  <sound id="brass.trombone.contrabass"/>
  <sound id="brass.trombone.tenor"/>
  <sound id="brass.trumpet"/>
  <sound id="brass.trumpet.baroque"/>
  <sound id="brass.trumpet.bass"/>
  <sound id="brass.trumpet.bflat"/>
  <sound id="brass.trumpet.c"/>
  <sound id="brass.trumpet.d"/>
  <sound id="brass.trumpet.piccolo"/>
  <sound id="brass.trumpet.pocket"/>
  <sound id="brass.trumpet.slide"/>
  <sound id="brass.trumpet.tenor"/>
  <sound id="brass.tuba"/>
  <sound id="brass.tuba.bass"/>
  <sound id="brass.tuba.subcontrabass"/>
  <sound id="brass.vienna-horn"/>
  <sound id="brass.vuvuzela"/>
  <sound id="brass.wagner-tuba"/>
  <sound id="drum.apentemma"/>
  <sound id="drum.ashiko"/>
  <sound id="drum.atabaque"/>
  <sound id="drum.atoke"/>
  <sound id="drum.atsimevu"/>
  <sound id="drum.axatse"/>
  <sound id="drum.bass-drum"/>
  <sound id="drum.bata"/>
  <sound id="drum.bata.itotele"/>
  <sound id="drum.bata.iya"/>
  <sound id="drum.bata.okonkolo"/>
  <sound id="drum.bendir"/>
  <sound id="drum.bodhran"/>
  <sound id="drum.bombo"/>
  <sound id="drum.bongo"/>
  <sound id="drum.bougarabou"/>
  <sound id="drum.buffalo-drum"/>
  <sound id="drum.cajon"/>
  <sound id="drum.chenda"/>
  <sound id="drum.chu-daiko"/>
  <sound id="drum.conga"/>
  <sound id="drum.cuica"/>
  <sound id="drum.dabakan"/>
  <sound id="drum.daff"/>
  <sound id="drum.dafli"/>
  <sound id="drum.daibyosi"/>
  <sound id="drum.damroo"/>
  <sound id="drum.darabuka"/>
  <sound id="drum.def"/>
  <sound id="drum.dhol"/>
  <sound id="drum.dholak"/>
  <sound id="drum.djembe"/>
  <sound id="drum.doira"/>
  <sound id="drum.dondo"/>
  <sound id="drum.doun-doun-ba"/>
  <sound id="drum.duff"/>
  <sound id="drum.dumbek"/>
  <sound id="drum.fontomfrom"/>
  <sound id="drum.frame-drum"/>
  <sound id="drum.frame-drum.arabian"/>
  <sound id="drum.geduk"/>
  <sound id="drum.ghatam"/>
  <sound id="drum.gome"/>
  <sound id="drum.group"/>
  <sound id="drum.group.chinese"/>
  <sound id="drum.group.ewe"/>
  <sound id="drum.group.indian"/>
  <sound id="drum.group.set"/>
  <sound id="drum.hand-drum"/>
  <sound id="drum.hira-daiko"/>
  <sound id="drum.ibo"/>
  <sound id="drum.igihumurizo"/>
  <sound id="drum.inyahura"/>
  <sound id="drum.ishakwe"/>
  <sound id="drum.jang-gu"/>
  <sound id="drum.kagan"/>
  <sound id="drum.kakko"/>
  <sound id="drum.kanjira"/>
  <sound id="drum.kendhang"/>
  <sound id="drum.kendhang.ageng"/>
  <sound id="drum.kendhang.ciblon"/>
  <sound id="drum.kenkeni"/>
  <sound id="drum.khol"/>
  <sound id="drum.kick-drum"/>
  <sound id="drum.kidi"/>
  <sound id="drum.ko-daiko"/>
  <sound id="drum.kpanlogo"/>
  <sound id="drum.kudum"/>
  <sound id="drum.lambeg"/>
  <sound id="drum.lion-drum"/>
  <sound id="drum.log-drum"/>
  <sound id="drum.log-drum.african"/>
  <sound id="drum.log-drum.native"/>
  <sound id="drum.log-drum.nigerian"/>
  <sound id="drum.madal"/>
  <sound id="drum.maddale"/>
  <sound id="drum.mridangam"/>
  <sound id="drum.naal"/>
  <sound id="drum.nagado-daiko"/>
  <sound id="drum.nagara"/>
  <sound id="drum.naqara"/>
  <sound id="drum.o-daiko"/>
  <sound id="drum.okawa"/>
  <sound id="drum.okedo-daiko"/>
  <sound id="drum.pahu-hula"/>
  <sound id="drum.pakhawaj"/>
  <sound id="drum.pandeiro"/>
  <sound id="drum.pandero"/>
  <sound id="drum.powwow"/>
  <sound id="drum.pueblo"/>
  <sound id="drum.repinique"/>
  <sound id="drum.riq"/>
  <sound id="drum.rototom"/>
  <sound id="drum.sabar"/>
  <sound id="drum.sakara"/>
  <sound id="drum.sampho"/>
  <sound id="drum.sangban"/>
  <sound id="drum.shime-daiko"/>
  <sound id="drum.slit-drum"/>
  <sound id="drum.slit-drum.krin"/>
  <sound id="drum.snare-drum"/>
  <sound id="drum.snare-drum.electric"/>
  <sound id="drum.sogo"/>
  <sound id="drum.surdo"/>
  <sound id="drum.tabla"/>
  <sound id="drum.tabla.bayan"/>
  <sound id="drum.tabla.dayan"/>
  <sound id="drum.taiko"/>
  <sound id="drum.talking"/>
  <sound id="drum.tama"/>
  <sound id="drum.tamborita"/>
  <sound id="drum.tambourine"/>
  <sound id="drum.tamte"/>
  <sound id="drum.tangku"/>
  <sound id="drum.tan-tan"/>
  <sound id="drum.taphon"/>
  <sound id="drum.tar"/>
  <sound id="drum.tasha"/>
  <sound id="drum.tenor-drum"/>
  <sound id="drum.teponaxtli"/>
  <sound id="drum.thavil"/>
  <sound id="drum.the-box"/>
  <sound id="drum.timbale"/>
  <sound id="drum.timpani"/>
  <sound id="drum.tinaja"/>
  <sound id="drum.toere"/>
  <sound id="drum.tombak"/>
  <sound id="drum.tom-tom"/>
  <sound id="drum.tom-tom.synth"/>
  <sound id="drum.tsuzumi"/>
  <sound id="drum.tumbak"/>
  <sound id="drum.uchiwa-daiko"/>
  <sound id="drum.udaku"/>
  <sound id="drum.udu"/>
  <sound id="drum.zarb"/>
  <sound id="effect.aeolian-harp"/>
  <sound id="effect.air-horn"/>
  <sound id="effect.applause"/>
  <sound id="effect.bass-string-slap"/>
  <sound id="effect.bird"/>
  <sound id="effect.bird.nightingale"/>
  <sound id="effect.bird.tweet"/>
  <sound id="effect.breath"/>
  <sound id="effect.bubble"/>
  <sound id="effect.bullroarer"/>
  <sound id="effect.burst"/>
  <sound id="effect.car"/>
  <sound id="effect.car.crash"/>
  <sound id="effect.car.engine"/>
  <sound id="effect.car.pass"/>
  <sound id="effect.car.stop"/>
  <sound id="effect.crickets"/>
  <sound id="effect.dog"/>
  <sound id="effect.door.creak"/>
  <sound id="effect.door.slam"/>
  <sound id="effect.explosion"/>
  <sound id="effect.flute-key-click"/>
  <sound id="effect.footsteps"/>
  <sound id="effect.frogs"/>
  <sound id="effect.guitar-cutting"/>
  <sound id="effect.guitar-fret"/>
  <sound id="effect.gunshot"/>
  <sound id="effect.hand-clap"/>
  <sound id="effect.heartbeat"/>
  <sound id="effect.helicopter"/>
  <sound id="effect.high-q"/>
  <sound id="effect.horse-gallop"/>
  <sound id="effect.jet-plane"/>
  <sound id="effect.laser-gun"/>
  <sound id="effect.laugh"/>
  <sound id="effect.lions-roar"/>
  <sound id="effect.machine-gun"/>
  <sound id="effect.marching-machine"/>
  <sound id="effect.metronome-bell"/>
  <sound id="effect.metronome-click"/>
  <sound id="effect.pat"/>
  <sound id="effect.punch"/>
  <sound id="effect.rain"/>
  <sound id="effect.scratch"/>
  <sound id="effect.scream"/>
  <sound id="effect.seashore"/>
  <sound id="effect.siren"/>
  <sound id="effect.slap"/>
  <sound id="effect.snap"/>
  <sound id="effect.stamp"/>
  <sound id="effect.starship"/>
  <sound id="effect.stream"/>
  <sound id="effect.telephone-ring"/>
  <sound id="effect.thunder"/>
  <sound id="effect.train"/>
  <sound id="effect.trash-can"/>
  <sound id="effect.whip"/>
  <sound id="effect.whistle"/>
  <sound id="effect.whistle.mouth-siren"/>
  <sound id="effect.whistle.police"/>
  <sound id="effect.whistle.slide"/>
  <sound id="effect.whistle.train"/>
  <sound id="effect.wind"/>
  <sound id="keyboard.accordion"/>
  <sound id="keyboard.bandoneon"/>
  <sound id="keyboard.celesta"/>
  <sound id="keyboard.clavichord"/>
  <sound id="keyboard.clavichord.synth"/>
  <sound id="keyboard.concertina"/>
  <sound id="keyboard.fortepiano"/>
  <sound id="keyboard.harmonium"/>
  <sound id="keyboard.harpsichord"/>
  <sound id="keyboard.ondes-martenot"/>
  <sound id="keyboard.organ"/>
  <sound id="keyboard.organ.drawbar"/>
  <sound id="keyboard.organ.percussive"/>
  <sound id="keyboard.organ.pipe"/>
  <sound id="keyboard.organ.reed"/>
  <sound id="keyboard.organ.rotary"/>
  <sound id="keyboard.piano"/>
  <sound id="keyboard.piano.electric"/>
  <sound id="keyboard.piano.grand"/>
  <sound id="keyboard.piano.honky-tonk"/>
  <sound id="keyboard.piano.prepared"/>
  <sound id="keyboard.piano.toy"/>
  <sound id="keyboard.piano.upright"/>
  <sound id="keyboard.virginal"/>
  <sound id="metal.adodo"/>
  <sound id="metal.anvil"/>
  <sound id="metal.babendil"/>
  <sound id="metal.bells.agogo"/>
  <sound id="metal.bells.almglocken"/>
  <sound id="metal.bells.bell-plate"/>
  <sound id="metal.bells.bell-tree"/>
  <sound id="metal.bells.carillon"/>
  <sound id="metal.bells.chimes"/>
  <sound id="metal.bells.chimta"/>
  <sound id="metal.bells.chippli"/>
  <sound id="metal.bells.church"/>
  <sound id="metal.bells.cowbell"/>
  <sound id="metal.bells.dawuro"/>
  <sound id="metal.bells.gankokwe"/>
  <sound id="metal.bells.ghungroo"/>
  <sound id="metal.bells.hatheli"/>
  <sound id="metal.bells.jingle-bell"/>
  <sound id="metal.bells.khartal"/>
  <sound id="metal.bells.mark-tree"/>
  <sound id="metal.bells.sistrum"/>
  <sound id="metal.bells.sleigh-bells"/>
  <sound id="metal.bells.temple"/>
  <sound id="metal.bells.tibetan"/>
  <sound id="metal.bells.tinklebell"/>
  <sound id="metal.bells.trychel"/>
  <sound id="metal.bells.wind-chimes"/>
  <sound id="metal.bells.zills"/>
  <sound id="metal.berimbau"/>
  <sound id="metal.brake-drums"/>
  <sound id="metal.crotales"/>
  <sound id="metal.cymbal.bo"/>
  <sound id="metal.cymbal.ceng-ceng"/>
  <sound id="metal.cymbal.chabara"/>
  <sound id="metal.cymbal.chinese"/>
  <sound id="metal.cymbal.ching"/>
  <sound id="metal.cymbal.clash"/>
  <sound id="metal.cymbal.crash"/>
  <sound id="metal.cymbal.finger"/>
  <sound id="metal.cymbal.hand"/>
  <sound id="metal.cymbal.kesi"/>
  <sound id="metal.cymbal.manjeera"/>
  <sound id="metal.cymbal.reverse"/>
  <sound id="metal.cymbal.ride"/>
  <sound id="metal.cymbal.sizzle"/>
  <sound id="metal.cymbal.splash"/>
  <sound id="metal.cymbal.suspended"/>
  <sound id="metal.cymbal.tebyoshi"/>
  <sound id="metal.cymbal.tibetan"/>
  <sound id="metal.cymbal.tingsha"/>
  <sound id="metal.flexatone"/>
  <sound id="metal.gong"/>
  <sound id="metal.gong.ageng"/>
  <sound id="metal.gong.agung"/>
  <sound id="metal.gong.chanchiki"/>
  <sound id="metal.gong.chinese"/>
  <sound id="metal.gong.gandingan"/>
  <sound id="metal.gong.kempul"/>
  <sound id="metal.gong.kempyang"/>
  <sound id="metal.gong.ketuk"/>
  <sound id="metal.gong.kkwenggwari"/>
  <sound id="metal.gong.luo"/>
  <sound id="metal.gong.singing"/>
  <sound id="metal.gong.thai"/>
  <sound id="metal.guira"/>
  <sound id="metal.hang"/>
  <sound id="metal.hi-hat"/>
  <sound id="metal.jaw-harp"/>
  <sound id="metal.kengong"/>
  <sound id="metal.murchang"/>
  <sound id="metal.musical-saw"/>
  <sound id="metal.singing-bowl"/>
  <sound id="metal.spoons"/>
  <sound id="metal.steel-drums"/>
  <sound id="metal.tamtam"/>
  <sound id="metal.thundersheet"/>
  <sound id="metal.triangle"/>
  <sound id="metal.washboard"/>
  <sound id="pitched-percussion.angklung"/>
  <sound id="pitched-percussion.balafon"/>
  <sound id="pitched-percussion.bell-lyre"/>
  <sound id="pitched-percussion.bells"/>
  <sound id="pitched-percussion.bianqing"/>
  <sound id="pitched-percussion.bianzhong"/>
  <sound id="pitched-percussion.bonang"/>
  <sound id="pitched-percussion.cimbalom"/>
  <sound id="pitched-percussion.crystal-glasses"/>
  <sound id="pitched-percussion.dan-tam-thap-luc"/>
  <sound id="pitched-percussion.fangxiang"/>
  <sound id="pitched-percussion.gandingan-a-kayo"/>
  <sound id="pitched-percussion.gangsa"/>
  <sound id="pitched-percussion.gender"/>
  <sound id="pitched-percussion.giying"/>
  <sound id="pitched-percussion.glass-harmonica"/>
  <sound id="pitched-percussion.glockenspiel"/>
  <sound id="pitched-percussion.glockenspiel.alto"/>
  <sound id="pitched-percussion.glockenspiel.soprano"/>
  <sound id="pitched-percussion.gyil"/>
  <sound id="pitched-percussion.hammer-dulcimer"/>
  <sound id="pitched-percussion.handbells"/>
  <sound id="pitched-percussion.kalimba"/>
  <sound id="pitched-percussion.kantil"/>
  <sound id="pitched-percussion.khim"/>
  <sound id="pitched-percussion.kulintang"/>
  <sound id="pitched-percussion.kulintang-a-kayo"/>
  <sound id="pitched-percussion.kulintang-a-tiniok"/>
  <sound id="pitched-percussion.likembe"/>
  <sound id="pitched-percussion.luntang"/>
  <sound id="pitched-percussion.marimba"/>
  <sound id="pitched-percussion.marimba.bass"/>
  <sound id="pitched-percussion.mbira"/>
  <sound id="pitched-percussion.mbira.array"/>
  <sound id="pitched-percussion.metallophone"/>
  <sound id="pitched-percussion.metallophone.alto"/>
  <sound id="pitched-percussion.metallophone.bass"/>
  <sound id="pitched-percussion.metallophone.soprano"/>
  <sound id="pitched-percussion.music-box"/>
  <sound id="pitched-percussion.pelog-panerus"/>
  <sound id="pitched-percussion.pemade"/>
  <sound id="pitched-percussion.penyacah"/>
  <sound id="pitched-percussion.ranat.ek"/>
  <sound id="pitched-percussion.ranat.ek-lek"/>
  <sound id="pitched-percussion.ranat.thum"/>
  <sound id="pitched-percussion.ranat.thum-lek"/>
  <sound id="pitched-percussion.reyong"/>
  <sound id="pitched-percussion.sanza"/>
  <sound id="pitched-percussion.saron-barung"/>
  <sound id="pitched-percussion.saron-demong"/>
  <sound id="pitched-percussion.saron-panerus"/>
  <sound id="pitched-percussion.slendro-panerus"/>
  <sound id="pitched-percussion.slentem"/>
  <sound id="pitched-percussion.tsymbaly"/>
  <sound id="pitched-percussion.tubes"/>
  <sound id="pitched-percussion.tubular-bells"/>
  <sound id="pitched-percussion.vibraphone"/>
  <sound id="pitched-percussion.xylophone"/>
  <sound id="pitched-percussion.xylophone.alto"/>
  <sound id="pitched-percussion.xylophone.bass"/>
  <sound id="pitched-percussion.xylophone.soprano"/>
  <sound id="pitched-percussion.xylorimba"/>
  <sound id="pitched-percussion.yangqin"/>
  <sound id="pluck.archlute"/>
  <sound id="pluck.autoharp"/>
  <sound id="pluck.baglama"/>
  <sound id="pluck.bajo"/>
  <sound id="pluck.balalaika"/>
  <sound id="pluck.balalaika.alto"/>
  <sound id="pluck.balalaika.bass"/>
  <sound id="pluck.balalaika.contrabass"/>
  <sound id="pluck.balalaika.piccolo"/>
  <sound id="pluck.balalaika.prima"/>
  <sound id="pluck.balalaika.secunda"/>
  <sound id="pluck.bandola"/>
  <sound id="pluck.bandura"/>
  <sound id="pluck.bandurria"/>
  <sound id="pluck.banjo"/>
  <sound id="pluck.banjo.tenor"/>
  <sound id="pluck.banjolele"/>
  <sound id="pluck.barbat"/>
  <sound id="pluck.bass"/>
  <sound id="pluck.bass.acoustic"/>
  <sound id="pluck.bass.bolon"/>
  <sound id="pluck.bass.electric"/>
  <sound id="pluck.bass.fretless"/>
  <sound id="pluck.bass.guitarron"/>
  <sound id="pluck.bass.synth"/>
  <sound id="pluck.bass.synth.lead"/>
  <sound id="pluck.bass.washtub"/>
  <sound id="pluck.bass.whamola"/>
  <sound id="pluck.begena"/>
  <sound id="pluck.biwa"/>
  <sound id="pluck.bordonua"/>
  <sound id="pluck.bouzouki"/>
  <sound id="pluck.bouzouki.irish"/>
  <sound id="pluck.celtic-harp"/>
  <sound id="pluck.charango"/>
  <sound id="pluck.chitarra-battente"/>
  <sound id="pluck.cithara"/>
  <sound id="pluck.cittern"/>
  <sound id="pluck.cuatro"/>
  <sound id="pluck.dan-bau"/>
  <sound id="pluck.dan-nguyet"/>
  <sound id="pluck.dan-tranh"/>
  <sound id="pluck.dan-ty-ba"/>
  <sound id="pluck.diddley-bow"/>
  <sound id="pluck.domra"/>
  <sound id="pluck.domu"/>
  <sound id="pluck.dulcimer"/>
  <sound id="pluck.dutar"/>
  <sound id="pluck.duxianqin"/>
  <sound id="pluck.ektara"/>
  <sound id="pluck.geomungo"/>
  <sound id="pluck.gottuvadhyam"/>
  <sound id="pluck.guitar"/>
  <sound id="pluck.guitar.acoustic"/>
  <sound id="pluck.guitar.electric"/>
  <sound id="pluck.guitar.nylon-string"/>
  <sound id="pluck.guitar.pedal-steel"/>
  <sound id="pluck.guitar.portuguese"/>
  <sound id="pluck.guitar.requinto"/>
  <sound id="pluck.guitar.resonator"/>
  <sound id="pluck.guitar.steel-string"/>
  <sound id="pluck.guitjo"/>
  <sound id="pluck.guitjo.double-neck"/>
  <sound id="pluck.guqin"/>
  <sound id="pluck.guzheng"/>
  <sound id="pluck.guzheng.choazhou"/>
  <sound id="pluck.harp"/>
  <sound id="pluck.harp-guitar"/>
  <sound id="pluck.huapanguera"/>
  <sound id="pluck.jarana-huasteca"/>
  <sound id="pluck.jarana-jarocha"/>
  <sound id="pluck.jarana-jarocha.mosquito"/>
  <sound id="pluck.jarana-jarocha.primera"/>
  <sound id="pluck.jarana-jarocha.segunda"/>
  <sound id="pluck.jarana-jarocha.tercera"/>
  <sound id="pluck.kabosy"/>
  <sound id="pluck.kantele"/>
  <sound id="pluck.kanun"/>
  <sound id="pluck.kayagum"/>
  <sound id="pluck.kobza"/>
  <sound id="pluck.komuz"/>
  <sound id="pluck.kora"/>
  <sound id="pluck.koto"/>
  <sound id="pluck.kutiyapi"/>
  <sound id="pluck.langeleik"/>
  <sound id="pluck.laud"/>
  <sound id="pluck.lute"/>
  <sound id="pluck.lyre"/>
  <sound id="pluck.mandobass"/>
  <sound id="pluck.mandocello"/>
  <sound id="pluck.mandola"/>
  <sound id="pluck.mandolin"/>
  <sound id="pluck.mandolin.octave"/>
  <sound id="pluck.mandora"/>
  <sound id="pluck.mandore"/>
  <sound id="pluck.marovany"/>
  <sound id="pluck.musical-bow"/>
  <sound id="pluck.ngoni"/>
  <sound id="pluck.oud"/>
  <sound id="pluck.pipa"/>
  <sound id="pluck.psaltery"/>
  <sound id="pluck.ruan"/>
  <sound id="pluck.sallaneh"/>
  <sound id="pluck.sanshin"/>
  <sound id="pluck.santoor"/>
  <sound id="pluck.sanxian"/>
  <sound id="pluck.sarod"/>
  <sound id="pluck.saung"/>
  <sound id="pluck.saz"/>
  <sound id="pluck.se"/>
  <sound id="pluck.setar"/>
  <sound id="pluck.shamisen"/>
  <sound id="pluck.sitar"/>
  <sound id="pluck.synth"/>
  <sound id="pluck.synth.charang"/>
  <sound id="pluck.synth.chiff"/>
  <sound id="pluck.synth.stick"/>
  <sound id="pluck.tambura"/>
  <sound id="pluck.tambura.bulgarian"/>
  <sound id="pluck.tambura.female"/>
  <sound id="pluck.tambura.male"/>
  <sound id="pluck.tar"/>
  <sound id="pluck.theorbo"/>
  <sound id="pluck.timple"/>
  <sound id="pluck.tiple"/>
  <sound id="pluck.tres"/>
  <sound id="pluck.ukulele"/>
  <sound id="pluck.ukulele.tenor"/>
  <sound id="pluck.valiha"/>
  <sound id="pluck.veena"/>
  <sound id="pluck.veena.mohan"/>
  <sound id="pluck.veena.rudra"/>
  <sound id="pluck.veena.vichitra"/>
  <sound id="pluck.vihuela"/>
  <sound id="pluck.vihuela.mexican"/>
  <sound id="pluck.xalam"/>
  <sound id="pluck.yueqin"/>
  <sound id="pluck.zither"/>
  <sound id="pluck.zither.overtone"/>
  <sound id="rattle.afoxe"/>
  <sound id="rattle.birds"/>
  <sound id="rattle.cabasa"/>
  <sound id="rattle.caxixi"/>
  <sound id="rattle.cog"/>
  <sound id="rattle.ganza"/>
  <sound id="rattle.hosho"/>
  <sound id="rattle.jawbone"/>
  <sound id="rattle.kayamba"/>
  <sound id="rattle.kpoko-kpoko"/>
  <sound id="rattle.lava-stones"/>
  <sound id="rattle.maraca"/>
  <sound id="rattle.rain-stick"/>
  <sound id="rattle.ratchet"/>
  <sound id="rattle.rattle"/>
  <sound id="rattle.shaker"/>
  <sound id="rattle.shaker.egg"/>
  <sound id="rattle.shekere"/>
  <sound id="rattle.sistre"/>
  <sound id="rattle.televi"/>
  <sound id="rattle.vibraslap"/>
  <sound id="rattle.wasembe"/>
  <sound id="strings.ajaeng"/>
  <sound id="strings.arpeggione"/>
  <sound id="strings.baryton"/>
  <sound id="strings.cello"/>
  <sound id="strings.cello.piccolo"/>
  <sound id="strings.contrabass"/>
  <sound id="strings.crwth"/>
  <sound id="strings.dan-gao"/>
  <sound id="strings.dihu"/>
  <sound id="strings.erhu"/>
  <sound id="strings.erxian"/>
  <sound id="strings.esraj"/>
  <sound id="strings.fiddle"/>
  <sound id="strings.fiddle.hardanger"/>
  <sound id="strings.gadulka"/>
  <sound id="strings.gaohu"/>
  <sound id="strings.gehu"/>
  <sound id="strings.group"/>
  <sound id="strings.group.synth"/>
  <sound id="strings.haegeum"/>
  <sound id="strings.hurdy-gurdy"/>
  <sound id="strings.igil"/>
  <sound id="strings.kamancha"/>
  <sound id="strings.kokyu"/>
  <sound id="strings.laruan"/>
  <sound id="strings.leiqin"/>
  <sound id="strings.lirone"/>
  <sound id="strings.lyra.byzantine"/>
  <sound id="strings.lyra.cretan"/>
  <sound id="strings.morin-khuur"/>
  <sound id="strings.nyckelharpa"/>
  <sound id="strings.octobass"/>
  <sound id="strings.rebab"/>
  <sound id="strings.rebec"/>
  <sound id="strings.sarangi"/>
  <sound id="strings.stroh-violin"/>
  <sound id="strings.tromba-marina"/>
  <sound id="strings.vielle"/>
  <sound id="strings.viol"/>
  <sound id="strings.viol.alto"/>
  <sound id="strings.viol.bass"/>
  <sound id="strings.viol.tenor"/>
  <sound id="strings.viol.treble"/>
  <sound id="strings.viol.violone"/>
  <sound id="strings.viola"/>
  <sound id="strings.viola-damore"/>
  <sound id="strings.violin"/>
  <sound id="strings.violono.piccolo"/>
  <sound id="strings.violotta"/>
  <sound id="strings.yayli-tanbur"/>
  <sound id="strings.yazheng"/>
  <sound id="strings.zhonghu"/>
  <sound id="synth.effects"/>
  <sound id="synth.effects.atmosphere"/>
  <sound id="synth.effects.brightness"/>
  <sound id="synth.effects.crystal"/>
  <sound id="synth.effects.echoes"/>
  <sound id="synth.effects.goblins"/>
  <sound id="synth.effects.rain"/>
  <sound id="synth.effects.sci-fi"/>
  <sound id="synth.effects.soundtrack"/>
  <sound id="synth.group"/>
  <sound id="synth.group.fifths"/>
  <sound id="synth.group.orchestra"/>
  <sound id="synth.pad"/>
  <sound id="synth.pad.bowed"/>
  <sound id="synth.pad.choir"/>
  <sound id="synth.pad.halo"/>
  <sound id="synth.pad.metallic"/>
  <sound id="synth.pad.polysynth"/>
  <sound id="synth.pad.sweep"/>
  <sound id="synth.pad.warm"/>
  <sound id="synth.theremin"/>
  <sound id="synth.tone.sawtooth"/>
  <sound id="synth.tone.sine"/>
  <sound id="synth.tone.square"/>
  <sound id="voice.aa"/>
  <sound id="voice.alto"/>
  <sound id="voice.aw"/>
  <sound id="voice.baritone"/>
  <sound id="voice.bass"/>
  <sound id="voice.child"/>
  <sound id="voice.countertenor"/>
  <sound id="voice.doo"/>
  <sound id="voice.ee"/>
  <sound id="voice.female"/>
  <sound id="voice.kazoo"/>
  <sound id="voice.male"/>
  <sound id="voice.mezzo-soprano"/>
  <sound id="voice.mm"/>
  <sound id="voice.oo"/>
  <sound id="voice.percussion"/>
  <sound id="voice.percussion.beatbox"/>
  <sound id="voice.soprano"/>
  <sound id="voice.synth"/>
  <sound id="voice.talk-box"/>
  <sound id="voice.tenor"/>
  <sound id="voice.vocals"/>
  <sound id="wind.flutes.bansuri"/>
  <sound id="wind.flutes.blown-bottle"/>
  <sound id="wind.flutes.calliope"/>
  <sound id="wind.flutes.danso"/>
  <sound id="wind.flutes.di-zi"/>
  <sound id="wind.flutes.dvojnice"/>
  <sound id="wind.flutes.fife"/>
  <sound id="wind.flutes.flageolet"/>
  <sound id="wind.flutes.flute"/>
  <sound id="wind.flutes.flute.alto"/>
  <sound id="wind.flutes.flute.bass"/>
  <sound id="wind.flutes.flute.contra-alto"/>
  <sound id="wind.flutes.flute.contrabass"/>
  <sound id="wind.flutes.flute.double-contrabass"/>
  <sound id="wind.flutes.flute.irish"/>
  <sound id="wind.flutes.flute.piccolo"/>
  <sound id="wind.flutes.flute.subcontrabass"/>
  <sound id="wind.flutes.fujara"/>
  <sound id="wind.flutes.gemshorn"/>
  <sound id="wind.flutes.hocchiku"/>
  <sound id="wind.flutes.hun"/>
  <sound id="wind.flutes.kaval"/>
  <sound id="wind.flutes.kawala"/>
  <sound id="wind.flutes.khlui"/>
  <sound id="wind.flutes.knotweed"/>
  <sound id="wind.flutes.koncovka.alto"/>
  <sound id="wind.flutes.koudi"/>
  <sound id="wind.flutes.ney"/>
  <sound id="wind.flutes.nohkan"/>
  <sound id="wind.flutes.nose"/>
  <sound id="wind.flutes.ocarina"/>
  <sound id="wind.flutes.overtone.tenor"/>
  <sound id="wind.flutes.palendag"/>
  <sound id="wind.flutes.panpipes"/>
  <sound id="wind.flutes.quena"/>
  <sound id="wind.flutes.recorder"/>
  <sound id="wind.flutes.recorder.alto"/>
  <sound id="wind.flutes.recorder.bass"/>
  <sound id="wind.flutes.recorder.contrabass"/>
  <sound id="wind.flutes.recorder.descant"/>
  <sound id="wind.flutes.recorder.garklein"/>
  <sound id="wind.flutes.recorder.great-bass"/>
  <sound id="wind.flutes.recorder.sopranino"/>
  <sound id="wind.flutes.recorder.soprano"/>
  <sound id="wind.flutes.recorder.tenor"/>
  <sound id="wind.flutes.ryuteki"/>
  <sound id="wind.flutes.shakuhachi"/>
  <sound id="wind.flutes.shepherds-pipe"/>
  <sound id="wind.flutes.shinobue"/>
  <sound id="wind.flutes.shvi"/>
  <sound id="wind.flutes.suling"/>
  <sound id="wind.flutes.tarka"/>
  <sound id="wind.flutes.tumpong"/>
  <sound id="wind.flutes.venu"/>
  <sound id="wind.flutes.whistle"/>
  <sound id="wind.flutes.whistle.alto"/>
  <sound id="wind.flutes.whistle.low-irish"/>
  <sound id="wind.flutes.whistle.shiva"/>
  <sound id="wind.flutes.whistle.slide"/>
  <sound id="wind.flutes.whistle.tin"/>
  <sound id="wind.flutes.whistle.tin.bflat"/>
  <sound id="wind.flutes.whistle.tin.d"/>
  <sound id="wind.flutes.xiao"/>
  <sound id="wind.flutes.xun"/>
  <sound id="wind.group"/>
  <sound id="wind.jug"/>
  <sound id="wind.pipes.bagpipes"/>
  <sound id="wind.pipes.gaida"/>
  <sound id="wind.pipes.highland"/>
  <sound id="wind.pipes.uilleann"/>
  <sound id="wind.pungi"/>
  <sound id="wind.reed.albogue"/>
  <sound id="wind.reed.alboka"/>
  <sound id="wind.reed.algaita"/>
  <sound id="wind.reed.arghul"/>
  <sound id="wind.reed.basset-horn"/>
  <sound id="wind.reed.bassoon"/>
  <sound id="wind.reed.bawu"/>
  <sound id="wind.reed.bifora"/>
  <sound id="wind.reed.bombarde"/>
  <sound id="wind.reed.chalumeau"/>
  <sound id="wind.reed.clarinet"/>
  <sound id="wind.reed.clarinet.a"/>
  <sound id="wind.reed.clarinet.alto"/>
  <sound id="wind.reed.clarinet.bass"/>
  <sound id="wind.reed.clarinet.basset"/>
  <sound id="wind.reed.clarinet.bflat"/>
  <sound id="wind.reed.clarinet.contra-alto"/>
  <sound id="wind.reed.clarinet.contrabass"/>
  <sound id="wind.reed.clarinet.eflat"/>
  <sound id="wind.reed.clarinet.piccolo.aflat"/>
  <sound id="wind.reed.clarinette-damour"/>
  <sound id="wind.reed.contrabass"/>
  <sound id="wind.reed.contrabassoon"/>
  <sound id="wind.reed.cornamuse"/>
  <sound id="wind.reed.cromorne"/>
  <sound id="wind.reed.crumhorn"/>
  <sound id="wind.reed.crumhorn.alto"/>
  <sound id="wind.reed.crumhorn.bass"/>
  <sound id="wind.reed.crumhorn.great-bass"/>
  <sound id="wind.reed.crumhorn.soprano"/>
  <sound id="wind.reed.crumhorn.tenor"/>
  <sound id="wind.reed.diple"/>
  <sound id="wind.reed.diplica"/>
  <sound id="wind.reed.duduk"/>
  <sound id="wind.reed.dulcian"/>
  <sound id="wind.reed.dulzaina"/>
  <sound id="wind.reed.english-horn"/>
  <sound id="wind.reed.guanzi"/>
  <sound id="wind.reed.harmonica"/>
  <sound id="wind.reed.harmonica.bass"/>
  <sound id="wind.reed.heckel-clarina"/>
  <sound id="wind.reed.heckelphone"/>
  <sound id="wind.reed.heckelphone.piccolo"/>
  <sound id="wind.reed.heckelphone-clarinet"/>
  <sound id="wind.reed.hichiriki"/>
  <sound id="wind.reed.hirtenschalmei"/>
  <sound id="wind.reed.hne"/>
  <sound id="wind.reed.hornpipe"/>
  <sound id="wind.reed.houguan"/>
  <sound id="wind.reed.hulusi"/>
  <sound id="wind.reed.jogi-baja"/>
  <sound id="wind.reed.ken-bau"/>
  <sound id="wind.reed.khaen-mouth-organ"/>
  <sound id="wind.reed.launeddas"/>
  <sound id="wind.reed.maqrunah"/>
  <sound id="wind.reed.melodica"/>
  <sound id="wind.reed.mijwiz"/>
  <sound id="wind.reed.mizmar"/>
  <sound id="wind.reed.nadaswaram"/>
  <sound id="wind.reed.oboe"/>
  <sound id="wind.reed.oboe.bass"/>
  <sound id="wind.reed.oboe.piccolo"/>
  <sound id="wind.reed.oboe-da-caccia"/>
  <sound id="wind.reed.oboe-damore"/>
  <sound id="wind.reed.octavin"/>
  <sound id="wind.reed.pi"/>
  <sound id="wind.reed.pibgorn"/>
  <sound id="wind.reed.piri"/>
  <sound id="wind.reed.rackett"/>
  <sound id="wind.reed.rauschpfeife"/>
  <sound id="wind.reed.rhaita"/>
  <sound id="wind.reed.rothphone"/>
  <sound id="wind.reed.sarrusaphone"/>
  <sound id="wind.reed.saxonette"/>
  <sound id="wind.reed.saxophone"/>
  <sound id="wind.reed.saxophone.alto"/>
  <sound id="wind.reed.saxophone.aulochrome"/>
  <sound id="wind.reed.saxophone.baritone"/>
  <sound id="wind.reed.saxophone.bass"/>
  <sound id="wind.reed.saxophone.contrabass"/>
  <sound id="wind.reed.saxophone.melody"/>
  <sound id="wind.reed.saxophone.mezzo-soprano"/>
  <sound id="wind.reed.saxophone.sopranino"/>
  <sound id="wind.reed.saxophone.sopranissimo"/>
  <sound id="wind.reed.saxophone.soprano"/>
  <sound id="wind.reed.saxophone.subcontrabass"/>
  <sound id="wind.reed.saxophone.tenor"/>
  <sound id="wind.reed.shawm"/>
  <sound id="wind.reed.shenai"/>
  <sound id="wind.reed.sheng"/>
  <sound id="wind.reed.sipsi"/>
  <sound id="wind.reed.sopila"/>
  <sound id="wind.reed.sorna"/>
  <sound id="wind.reed.sralai"/>
  <sound id="wind.reed.suona"/>
  <sound id="wind.reed.surnai"/>
  <sound id="wind.reed.taepyeongso"/>
  <sound id="wind.reed.tarogato"/>
  <sound id="wind.reed.tarogato.ancient"/>
  <sound id="wind.reed.trompeta-china"/>
  <sound id="wind.reed.tubax"/>
  <sound id="wind.reed.xaphoon"/>
  <sound id="wind.reed.zhaleika"/>
  <sound id="wind.reed.zurla"/>
  <sound id="wind.reed.zurna"/>
  <sound id="wood.agogo-block"/>
  <sound id="wood.agung-a-tamlang"/>
  <sound id="wood.ahoko"/>
  <sound id="wood.bones"/>
  <sound id="wood.castanets"/>
  <sound id="wood.claves"/>
  <sound id="wood.drum-sticks"/>
  <sound id="wood.gourd"/>
  <sound id="wood.granite-block"/>
  <sound id="wood.guban"/>
  <sound id="wood.guiro"/>
  <sound id="wood.hyoushigi"/>
  <sound id="wood.ipu"/>
  <sound id="wood.jam-block"/>
  <sound id="wood.kaekeeke"/>
  <sound id="wood.kagul"/>
  <sound id="wood.kalaau"/>
  <sound id="wood.kashiklar"/>
  <sound id="wood.kubing"/>
  <sound id="wood.pan-clappers"/>
  <sound id="wood.sand-block"/>
  <sound id="wood.slapstick"/>
  <sound id="wood.stir-drum"/>
  <sound id="wood.temple-block"/>
  <sound id="wood.tic-toc-block"/>
  <sound id="wood.tonetang"/>
  <sound id="wood.wood-block"/>
-}

