
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
        -- i?mplicit
        -- nonContr?
        -- width?


type Music = [MusicData]

data MusicData 
    = MusicNote Note
    | MusicBackup Backup
    | MusicForward Forward
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
    | GraceNote
        FullNote
        [Tie]
        NoteProps

data FullNote
    = Pitched Pitch
    | Unpitched DispStepOct
    | Rest DispStepOct


data NoteProps 
    = NoteProps
        -- instr
        -- note type
        -- dots
        -- accidental
        -- time modification
        -- stem
        -- note head
        -- staff
        -- beam
        -- notations
        -- lyrics
    



data Tie = Tie



data Backup = Backup
data Forward = Forward
data Direction = Direction
data Attributes = Attributes








type Step        = Int -- TODO newtype?
type Oct         = Int -- TODO newtype?
type Divs        = Int -- TODO newtype?
type Dur         = Divs
type Pitch       = (Step, Oct)
type DispStepOct = (Step, Oct)

