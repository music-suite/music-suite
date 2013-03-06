
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------------------------------

module Music.MusicXml.Score (

        -----------------------------------------------------------------------------
        -- * Score
        -----------------------------------------------------------------------------

        Score(..),
        ScoreHeader(..),
        Identification(..),
        Creator(..),
        Defaults(..),
        ScoreAttrs(..),
        PartAttrs(..),
        MeasureAttrs(..),

        -- ** Part list
        PartList,
        PartListElem(..),
        GroupSymbol(..),
        GroupBarlines(..),

        -----------------------------------------------------------------------------
        -- * Music
        -----------------------------------------------------------------------------

        Music,
        MusicElem(..),

        -----------------------------------------------------------------------------

        -- ** Attributes
        Attributes(..),
        TimeSignature(..),
        ClefSign(..),

        -----------------------------------------------------------------------------
        -- ** Notes

        Note(..),
        FullNote(..),
        IsChord,
        noChord,
        noTies,
        Tie,
        NoteProps(..),
        mapNoteProps,
        mapNoteProps2,

        -----------------------------------------------------------------------------
        -- ** Notations

        Notation(..),
        FermataSign(..),
        Articulation(..),
        Ornament(..),
        Technical(..),

        -----------------------------------------------------------------------------
        -- ** Directions

        Direction(..),

        -----------------------------------------------------------------------------
        -- ** Lyrics

        Lyric(..),


        -----------------------------------------------------------------------------
        -- * Basic types
        -----------------------------------------------------------------------------

        -----------------------------------------------------------------------------
        -- ** Pitch

        Pitch(..),
        DisplayPitch(..),
        PitchClass,
        Semitones(..),
        noSemitones,

        Octaves(..),
        Fifths(..),
        Line(..),

        Mode(..),
        Accidental(..),

        -----------------------------------------------------------------------------
        -- ** Time

        Duration(..),
        NoteType(..),

        Divs(..),
        NoteVal(..),
        NoteSize(..),

        Beat(..),
        BeatType(..),

        -----------------------------------------------------------------------------
        -- ** Dynamics

        Dynamics(..),

        -----------------------------------------------------------------------------
        -- ** Misc

        StemDirection(..),
        NoteHead(..),
        LineType(..),

        Level(..),
        BeamType(..),
        StartStop(..),
        StartStopChange(..),
        StartStopContinue(..),
        StartStopContinueChange(..),

  ) where

import Prelude hiding (getLine)

import Numeric.Natural
import TypeUnary.Nat

import Music.MusicXml.Time
import Music.MusicXml.Pitch
import Music.MusicXml.Dynamics


-- ----------------------------------------------------------------------------------
-- Score
-- ----------------------------------------------------------------------------------

data Score
    = Partwise
        ScoreAttrs
        ScoreHeader
        [(PartAttrs,
            [(MeasureAttrs, Music)])]   -- music by part and time
    | Timewise
        ScoreAttrs
        ScoreHeader
        [(MeasureAttrs,
            [(PartAttrs, Music)])]      -- music by time and part

data ScoreHeader
    = ScoreHeader
        (Maybe String)                  --  title
        (Maybe String)                  --  movement title
        (Maybe Identification)          --  identification?
                                        --  defaults?
                                        --  credit*
        PartList                        --  partlist?


data Identification
    = Identification
        [Creator]                       --  creator

data Creator
    = Creator
        String                          --  type (composer, lyricist, arranger etc)
        String                          --  name

data Defaults
    = Defaults
                                        --  page layout (marigins, distance etc)
                                        --  system layout
                                        --  staff layout
                                        --  scaling
                                        --  appearance (line width etc)

data ScoreAttrs
    = ScoreAttrs
        [Int]                           --  score version

data PartAttrs
    = PartAttrs
        String                          --  part id

data MeasureAttrs
    = MeasureAttrs
        Int                             --  measure number


-- ----------------------------------------------------------------------------------
-- Part list
-- ----------------------------------------------------------------------------------

type PartList = [PartListElem]

data PartListElem
    = Part
        String
        String
        (Maybe String)                  -- id name abbrev?
    | Group                                           
        Level                                                
        StartStop
        String
        (Maybe String)
        (Maybe GroupSymbol)
        (Maybe GroupBarlines)
        Bool                            -- number start/stop name abbrev? symbol barline style

data GroupSymbol   = GroupBrace | GroupLine | GroupBracket | GroupSquare | NoGroupSymbol
data GroupBarlines = GroupBarLines | GroupNoBarLines | GroupMensurstrich

-- ----------------------------------------------------------------------------------
-- Music
-- ----------------------------------------------------------------------------------

type Music = [MusicElem]

data MusicElem
    = MusicAttributes
        Attributes
    | MusicBackup                       -- TODO
    | MusicForward                      -- TODO
    | MusicNote
        Note
    | MusicDirection
        Direction
    | MusicHarmony                      -- TODO
    | MusicFiguredBass                  -- TODO
    | MusicPrint                        -- TODO
    | MusicSound                        -- TODO
    | MusicBarline                      -- TODO
    | MusicGrouping                     -- TODO
    | MusicLink                         -- TODO
    | MusicBookmark                     -- TODO


-- ----------------------------------------------------------------------------------
-- Attributes
-- ----------------------------------------------------------------------------------

data Attributes
    = Divisions
        Divs
    | Key
        Fifths
        Mode
    | Time
        TimeSignature
    | Staves
        Natural
    | PartSymbol                        -- TODO
    | Instruments
        Natural
    | Clef
        ClefSign
        Line
    | StaffDetails                      -- TODO
    | Transpose                         -- TODO
    | Directive                         -- TODO
    | MeasureStyle                      -- TODO

data TimeSignature
    = CommonTime
    | CutTime
    | DivTime
        Beat
        BeatType

data ClefSign
    = GClef
    | CClef
    | FClef
    | PercClef
    | TabClef
    deriving (Eq, Ord, Enum, Bounded)


-- ----------------------------------------------------------------------------------
-- Notes
-- ----------------------------------------------------------------------------------

data Note
    = Note
        FullNote
        Duration
        [Tie]
        NoteProps
    | CueNote
        FullNote
        Duration
        NoteProps
    | GraceNote
        FullNote
        [Tie]
        NoteProps

data FullNote
    = Pitched
        IsChord
        Pitch
    | Unpitched
        IsChord
        (Maybe DisplayPitch)
    | Rest
        IsChord
        (Maybe DisplayPitch)

type IsChord = Bool
type Tie = StartStop

data NoteProps
    = NoteProps {
        noteInstrument   :: Maybe String,                       -- instrument
        noteVoice        :: Maybe Natural,                      -- voice
        noteType         :: Maybe NoteType,                     -- type
        noteDots         :: Natural,                            -- dots
        noteAccidental   :: Maybe (Accidental, Bool, Bool),     -- accidental, cautionary, editorial
        noteTimeMod      :: Maybe (Natural, Natural),           -- actual, normal
        noteStem         :: Maybe StemDirection,                -- stem
        noteNoteHead     :: Maybe (NoteHead, Bool, Bool),       -- notehead, filled, parentheses
        noteNoteHeadText :: Maybe String,                       -- notehead-text
        noteStaff        :: Maybe Natural,                      -- staff
        noteBeam         :: Maybe (Level, BeamType),            -- beam-level, beam-type
        noteNotations    :: [Notation],                         -- notation
        noteLyrics       :: [Lyric]                             -- lyric
    }

noChord :: IsChord
noChord = False

noTies :: [Tie]
noTies = []

mapNoteProps :: (NoteProps -> NoteProps) -> Note -> Note
mapNoteProps f (Note x d t p)     = Note x d t (f p)
mapNoteProps f (CueNote x d p)    = CueNote x d (f p)
mapNoteProps f (GraceNote x t p)  = GraceNote x t (f p)

mapNoteProps2 :: (NoteProps -> NoteProps) -> MusicElem -> MusicElem
mapNoteProps2 f (MusicNote n) = MusicNote (mapNoteProps f n)
mapNoteProps2 f x             = x



-- ----------------------------------------------------------------------------------
-- Notations
-- ----------------------------------------------------------------------------------

data Notation
     = Tied
        StartStopContinue               -- type
     | Slur
        Level
        StartStopContinue               -- level start/stop
     | Tuplet
        Level
        StartStopContinue               -- level start/stop
     | Glissando
        Level
        StartStopContinue
        LineType
        (Maybe String)                  -- level type start/stop text?
     | Slide
        Level
        StartStopContinue
        LineType
        (Maybe String)                  -- level type start/stop text?
     | Ornaments
        [(Ornament, [Accidental])]
     | Technical
        [Technical]
     | Articulations
        [Articulation]
     | DynamicNotation
        Dynamics
     | Fermata FermataSign
     | Arpeggiate
     | NonArpeggiate
     | AccidentalMark
        Accidental
     | OtherNotation
        String

data FermataSign = NormalFermata | AngledFermata | SquaredFermata

data Articulation
    = Accent 
    | StrongAccent 
    | Staccato 
    | Tenuto
    | DetachedLegato 
    | Staccatissimo 
    | Spiccato
    | Scoop 
    | Plop 
    | Doit 
    | Falloff 
    | BreathMark
    | Caesura 
    | Stress 
    | Unstress 
    | OtherArticulation

data Ornament
    = TrillMark 
    | Turn 
    | DelayedTurn 
    | InvertedTurn 
    | DelayedInvertedTurn 
    | VerticalTurn 
    | Shake 
    | WavyLine 
    | Mordent 
    | InvertedMordent 
    | Schleifer 
    | Tremolo 
        Natural                         -- TODO restrict to (1..8) range
    | OtherOrnament
        String
        
data Technical
    = UpBow 
    | DownBow 
    | Harmonic 
    | OpenString 
    | ThumbPosition 
    | Fingering 
    | Pluck 
    | DoubleTongue 
    | TripleTongue 
    | Stopped 
    | SnapPizzicato 
    | Fret 
    | String 
    | HammerOn 
    | PullOff 
    | Bend 
    | Tap 
    | Heel 
    | Toe 
    | Fingernails 
    | Hole 
    | Arrow 
    | Handbell 
    | OtherTechnical
        String

-- ----------------------------------------------------------------------------------
-- Directions
-- ----------------------------------------------------------------------------------

data Direction
    = Rehearsal
        String
    | Segno
    | Words
        String
    | Coda
    | Crescendo
        StartStop                       -- start/stop
    | Diminuendo
        StartStop                       -- start/stop
    | Dynamics
        Dynamics
    | Dashes
        Level
        StartStop                       -- level start/stop
    | Bracket                           -- TODO
    | Pedal
        StartStopChange
    | Metronome
        NoteVal
        Bool
        Tempo                           -- noteVal isDotted bpm 
    | OctaveShift                       -- TODO
    | HarpPedals                        -- TODO
    | Damp                              -- TODO
    | DampAll                           -- TODO
    | EyeGlasses                        -- TODO
    | StringMute                        -- TODO
    | Scordatura                        -- TODO
    | Image                             -- TODO
    | PrincipalVoice                    -- TODO
    | AccordionRegistration             -- TODO
    | Percussion                        -- TODO
    | OtherDirection
        String


-- ----------------------------------------------------------------------------------
-- Lyrics
-- ----------------------------------------------------------------------------------

data Lyric = Lyric -- TODO


-- ----------------------------------------------------------------------------------
-- Basic types
-- ----------------------------------------------------------------------------------

newtype Level   = Level { getLevel :: Max8 }

data BeamType
    = BeginBeam
    | ContinueBeam
    | EndBeam
    | ForwardHook
    | BackwardHook

type StartStop         = StartStopContinueChange
type StartStopChange   = StartStopContinueChange
type StartStopContinue = StartStopContinueChange

data StartStopContinueChange
    = Start
    | Stop
    | Continue              
    | Change

data StemDirection
    = StemDown
    | StemUp
    | StemNone
    | StemDouble

data LineType
    = Solid
    | Dashed
    | Dotted
    | Wavy

data NoteHead
    = SlashNoteHead
    | TriangleNoteHead
    | DiamondNoteHead
    | SquareNoteHead
    | CrossNoteHead
    | XNoteHead
    | CircleXNoteHead
    | InvertedTriangleNoteHead
    | ArrowDownNoteHead
    | ArrowUpNoteHead
    | SlashedNoteHead
    | BackSlashedNoteHead
    | NormalNoteHead
    | ClusterNoteHead
    | CircleDotNoteHead
    | LeftTriangleNoteHead
    | RectangleNoteHead
    | NoNoteHead                        -- "none"

deriving instance Eq            Level
deriving instance Show          Level
deriving instance Num           Level

-- ----------------------------------------------------------------------------------

-- Bounded ints
type Max8 = Index N8

notImplemented x = error $ "Not implemented: " ++ x


