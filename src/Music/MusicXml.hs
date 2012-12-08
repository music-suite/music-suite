
module Music.MusicXml -- (
--  ) 
where

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
        -- id?

data ScoreHeader
    = ScoreHeader
        -- titles?
        -- identification?
        -- defaults?
        -- credit*
        -- partlist?

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
    = TieStart Int -- level
    | TieStop  Int -- level
    -- TODO type, position, placement


data Direction = Direction
data Attributes = Attributes








newtype Divisions = Divisions Int      -- absolute dur
newtype NoteValue = NoteValue Rational -- relative dur
newtype Octaves   = Octaves Int
newtype Steps     = Steps Int
newtype Semitones = Semitones Int

data NoteSize     = SizeFull | SizeCue | SizeLarge
data Accidental   = DoubleFlat | Flat | Natural | Sharp | DoubleSharp

type Dur          = Divisions
type NoteType     = (NoteValue, NoteSize)
type Pitch        = (Steps, Maybe Semitones, Octaves)
type DisplayPitch = (Steps, Octaves)


