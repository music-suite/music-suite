
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
        MusicData(..),

        -- * Notes
        Note(..),
        FullNote(..),
        NoteProps(..),
        Tie(..),
        TieNotation(..),

        -- * Directions
        Direction(..),

        -- * Attributes
        Attributes(..),



        -- * Basic types
        -- ** Enum types
        NoteSize(..),
        Accidental(..),

        -- ** Numeric types
        Divisions(..),
        NoteValue(..),
        Octaves(..),
        Steps(..),
        Semitones(..),

        -- ** Derived types
        -- *** Time
        Dur(..),
        NoteType(..),
        -- *** Pitch
        Pitch(..),
        DisplayPitch(..)
 ) where


-- --------------------------------------------------------------------------------
-- Score
-- --------------------------------------------------------------------------------

data Score
    = PartwiseScore
        ScoreAttrs
        ScoreHeader
        [[(MeasureAttrs, Music)]]
    | TimewiseScore
        ScoreAttrs
        ScoreHeader
        [(MeasureAttrs, [Music])]

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


data MeasureAttrs
    = MeasureAttrs
                            -- number
                            -- implicit?
                            -- nonContr?
                            -- width?


-- --------------------------------------------------------------------------------
-- Part list
-- --------------------------------------------------------------------------------

type PartList = [PartListElem]

data PartListElem
    = PartListPart
          String            -- id
          String            -- name
          (Maybe String)    -- abbrev
                            -- instr
                            -- midi device
                            -- midi instr
    | PartListGroup
          String            -- name
          (Maybe String)    -- abbrev
                            -- symbol
                            -- commonBarline
                            -- time (?)
                            -- type (?)
                            -- number (?)


-- --------------------------------------------------------------------------------
-- Music
-- --------------------------------------------------------------------------------

type Music = [MusicData]

data MusicData
    = MusicNote Note
    -- |   MusicBackup Backup
    -- |   MusicForward Forward
    | MusicDirection Direction
    | MusicAttributes Attributes
    --  | Harmony Harmony
    --  | FiguredBass FiguredBass
    --  | Print Print
    --  | Sound Sound
    --  | Barline Barline
    --  | Grouping Grouping
    --  | Link Link
    --  | Bookmark Bookmark


-- --------------------------------------------------------------------------------
-- Notes
-- --------------------------------------------------------------------------------

data Note
    = Note
        FullNote
        Dur
        [Tie]
        NoteProps
    | CueNote
        FullNote
        Dur
        NoteProps
    | GraceNote
        FullNote
        [Tie]
        NoteProps

data FullNote
    = Pitched       -- isChordNote pitch
        Bool
        Pitch
    | Unpitched     -- isChordNote disp
        Bool
        DisplayPitch
    | Rest          -- isChordNote disp
        Bool
        DisplayPitch


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



data Tie
    = TieStart Int -- level
    | TieStop  Int -- level

data TieNotation
    = TieNotationStart Int -- level
    | TieNotationStop  Int -- level
    -- TODO type, position, placement


-- --------------------------------------------------------------------------------
-- Directions
-- --------------------------------------------------------------------------------

data Direction = Direction














data Attributes = Attributes

-- --------------------------------------------------------------------------------
-- Basic types
-- --------------------------------------------------------------------------------








newtype Divisions = Divisions { getDivisions :: Int }      -- absolute dur
newtype NoteValue = NoteValue { getNoteValue :: Rational } -- relative dur
newtype Octaves   = Octaves { getOctaves :: Int }
newtype Steps     = Steps { getSteps :: Int }
newtype Semitones = Semitones { getSemitones :: Int }

data NoteSize     = SizeFull | SizeCue | SizeLarge
data Accidental   = DoubleFlat | Flat | Natural | Sharp | DoubleSharp

type Dur          = Divisions
type NoteType     = (NoteValue, NoteSize)
type Pitch        = (Steps, Maybe Semitones, Octaves)
type DisplayPitch = (Steps, Octaves)


