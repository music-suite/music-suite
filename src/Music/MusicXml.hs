
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    CPP
    #-}

-- |
-- A Haskell representation of MusicXML.
--
-- For an introduction, see <http://www.makemusic.com/musicxml/tutorial>.
module Music.MusicXml (
        -- * Score
        Score(..),
        ScoreAttrs(..),
        ScoreHeader(..),
        Defaults(..),
        MeasureAttrs(..),
        -- ** Part list
        PartList,
        PartListElem(..),


        -- * Music
        Music,
        MusicElem(..),

        -- ** Attributes
        Attrs(..),
        TimeSig(..),
        ClefSign(..),

        -- ** Notes
        Note(..),
        FullNote(..),
        NoteProps(..),
        Tie(..),
        TieNotation(..),

        -- ** Directions
        Direction(..),



        -- * Basic types
        -- ** Enum types
        NoteSize(..),
        Accidental(..),
        DynamicLevel(..),

        -- ** Numeric types
        Divisions(..),
        NoteValue(..),
        Octaves(..),
        Steps(..),
        Semitones(..),
        Line(..),
        Fifths(..),
        Beats(..),
        BeatTypes(..),

        -- ** Derived types
        -- *** Time
        Dur(..),
        NoteType(..),
        -- *** Pitch
        Pitch(..),
        DisplayPitch(..),

        -- * Import and export functions
        toXml,
        showXml
 ) where

import Data.Semigroup
import Data.Default
import Text.XML.Light hiding (Line)

-- --------------------------------------------------------------------------------
-- Score
-- --------------------------------------------------------------------------------

data Score
    = Partwise
        ScoreAttrs
        ScoreHeader
        [[(MeasureAttrs, Music)]]
    | Timewise
        ScoreAttrs
        ScoreHeader
        [(MeasureAttrs, [Music])]

#ifndef __HADDOCK__
instance Out Score where
    out (Partwise a h ms) = qnode "partwise-score" ()
    out (Timewise a h ms) = qnode "timewise-score" ()
#endif


data ScoreAttrs
    = ScoreAttrs
        [Int]               -- version

data ScoreHeader
    = ScoreHeader
                            -- titles?
                            -- identification?
                            -- defaults?
                            -- credit*
        PartList            -- partlist?

data Defaults
    = Defaults
                            -- page layout (marigins, distance etc)
                            -- system layout
                            -- staff layout
                            -- scaling
                            -- appearance (line width etc)

-- TODO fancy numbers
data MeasureAttrs
    = MeasureAttrs
        Int
                            -- number
                            -- implicit?
                            -- nonContr?
                            -- width?


-- --------------------------------------------------------------------------------
-- Part list
-- --------------------------------------------------------------------------------

type PartList = [PartListElem]

data PartListElem
    = Part
          String            -- id
          String            -- name
          (Maybe String)    -- abbrev
                            -- instr
                            -- midi device
                            -- midi instr
    | Group
          String            -- name
          (Maybe String)    -- abbrev
                            -- symbol
                            -- common barline
                            -- time (?)
                            -- type (?)
                            -- number (?)


-- --------------------------------------------------------------------------------
-- Music
-- --------------------------------------------------------------------------------

type Music = [MusicElem]

data MusicElem
    = MusicAttr Attrs
    --  | MusicBackup Backup
    --  | MusicForward Forward
    | MusicNote Note
    | MusicDirection Direction
    --  | Harmony Harmony
    --  | FiguredBass FiguredBass
    --  | Print Print
    --  | Sound Sound
    --  | Barline Barline
    --  | Grouping Grouping
    --  | Link Link
    --  | Bookmark Bookmark


-- --------------------------------------------------------------------------------
-- Attributes
-- --------------------------------------------------------------------------------

-- TODO multi-staff

data Attrs
    = Div  Divisions
    | Clef ClefSign Line
    | Key  Fifths
    | Time TimeSig

data TimeSig
    = CommonTime
    | CutTime
    | DivTime Beats BeatTypes

data ClefSign = GClef | CClef | FClef
    deriving (Eq, Ord, Enum, Bounded)

-- Staves
-- Transposition

-- --------------------------------------------------------------------------------
-- Notes
-- --------------------------------------------------------------------------------

data Note
    = Note
        FullNote
        Dur
        [Tie]
        -- NoteProps
    | CueNote
        FullNote
        Dur
        -- NoteProps
    | GraceNote
        FullNote
        [Tie]
        -- NoteProps

-- Note: Chords are indicated by setting isChord to True (and use same duration)
data FullNote
    = Pitched       -- isChord pitch
        Bool
        Pitch
    | Unpitched     -- isChord disp
        Bool
        DisplayPitch
    | Rest          -- isChord disp
        Bool
        DisplayPitch

-- TODO level
data Tie
    = TieStart
    | TieStop


-- TODO voice?
data NoteProps
    = NoteProps
                    -- instr
        NoteType    -- note type
        Int         -- dots
        Accidental  -- accidental
                    -- time modification
                    -- stem
                    -- note head
                    -- staff
                    -- beam
                    -- notations
                    -- lyrics


-- TODO
data Notation = Notation
    --  = NotationTied Tied
    --  | NotationSlur Slur
    --  | NotationTuplet Tuplet
    --  | NotationGlissando Glissando
    --  | NotationSlide Slide
    --  | NotationOrnaments Ornaments
    --  | NotationTechnical Technical
    --  | NotationArticulations Articulations
    --  | NotationDynamics Dynamics
    --  | NotationFermata Fermata
    --  | NotationArpeggiate Arpeggiate
    --  | NotationNonArpeggiate NonArpeggiate
    --  | NotationAccidentalMark AccidentalMark
    --  | NotationOther OtherNotation

data TieNotation
    = TieNotationStart
    | TieNotationStop

-- --------------------------------------------------------------------------------
-- Directions
-- --------------------------------------------------------------------------------

data Direction
    = Words String            -- TODO separate font style, placement etc
    | Dynamic DynamicLevel
    | Pedal Bool              -- start/stop (the latter usually generates "stop")
    | Crescendo Bool
    | Diminuendo Bool
    -- segno
    -- coda
    -- rehearsal
    -- pedals
    -- dashes, cesuras
    -- metronome
    -- 8va

-- --------------------------------------------------------------------------------
-- Notations
-- --------------------------------------------------------------------------------


-- --------------------------------------------------------------------------------
-- Basic types
-- --------------------------------------------------------------------------------

type Dur          = Divisions
type NoteType     = (NoteValue, NoteSize)
type Pitch        = (Steps, Maybe Semitones, Octaves)       -- semitones maybe redundant (use zero)?
type DisplayPitch = (Steps, Octaves)

newtype Divisions = Divisions { getDivisions :: Int }       -- absolute dur
    deriving (Eq, Ord, Num, Enum)
newtype NoteValue = NoteValue { getNoteValue :: Rational }  -- relative dur
    deriving (Eq, Ord, Num, Enum)


newtype Octaves   = Octaves { getOctaves :: Int }
    deriving (Eq, Ord, Num, Enum)
newtype Steps     = Steps { getSteps :: Int }
    deriving (Eq, Ord, Num, Enum)
newtype Semitones = Semitones { getSemitones :: Double }    -- microtones allowed here
    deriving (Eq, Ord, Num, Enum)
newtype Line      = Line { getLine :: Int }                 -- line number, from bottom
    deriving (Eq, Ord, Num, Enum)

newtype Fifths    = Fifths { getFifths :: Int }             -- number of upwards fifths, starting from C
    deriving (Eq, Ord, Num, Enum)
newtype Beats     = Beats { getBeats :: Int }               -- time nominator
    deriving (Eq, Ord, Num, Enum)
newtype BeatTypes = BeatTypes { getBeatTypes :: Int }       -- time denominator
    deriving (Eq, Ord, Num, Enum)


data NoteSize     = SizeFull | SizeCue | SizeLarge
    deriving (Eq, Ord, Enum, Bounded)
data Accidental   = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
    deriving (Eq, Ord, Enum, Bounded)
data DynamicLevel = PPP | PP | P | MP | MF | F | FF | FFF
    deriving (Eq, Ord, Enum, Bounded)


-- --------------------------------------------------------------------------------
-- Import and export functions
-- --------------------------------------------------------------------------------

-- |
-- Render a score as a MusicXML string.
showXml :: Score -> String
showXml = showElement . toXml

-- |
-- Render a score as MusicXML.
toXml :: Score -> Element
#ifndef __HADDOCK__
toXml = out
#else
toXml = undefined
#endif


-- --------------------------------------------------------------------------------

#ifndef __HADDOCK__
class Out a where
    out :: a -> Element
#endif

qnode n cs = node (QName n Nothing Nothing) cs

