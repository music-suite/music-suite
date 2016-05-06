
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

module Data.Music.MusicXml.Score (

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
        PartList(..),
        PartListElem(..),
        GroupSymbol(..),
        GroupBarlines(..),

        -----------------------------------------------------------------------------
        -- * Music
        -----------------------------------------------------------------------------

        Music(..),
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
        HasNoteProps(..),

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
        -- ** Barlines
        Barline(..),
        BarStyle(..),
        BarlineLocation(..),
        Repeat(..),
        RepeatDirection(..),

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

import Data.Default
import Data.Semigroup
import Data.Foldable
import Numeric.Natural
import TypeUnary.Nat

import Data.Music.MusicXml.Time
import Data.Music.MusicXml.Pitch
import Data.Music.MusicXml.Dynamics

import qualified Data.List as List

-- ----------------------------------------------------------------------------------
-- Score
-- ----------------------------------------------------------------------------------

data Score
    = Partwise
        ScoreAttrs
        ScoreHeader
        [(PartAttrs,
            [(MeasureAttrs, Music)])]   -- ^ music by part and time
    | Timewise
        ScoreAttrs
        ScoreHeader
        [(MeasureAttrs,
            [(PartAttrs, Music)])]      -- ^ music by time and part

data ScoreHeader
    = ScoreHeader
        (Maybe String)                  --  ^ title
        (Maybe String)                  --  ^ movement title
        (Maybe Identification)          --  ^ identification?
                                        --  ^ defaults?
                                        --  ^ credit*
        PartList                        --  ^ partlist?


data Identification
    = Identification
        [Creator]                       --  ^ creator

data Creator
    = Creator
        String                          --  ^ type (composer, lyricist, arranger etc)
        String                          --  ^ name

data Defaults
    = Defaults
                                        --  ^ page layout (marigins, distance etc)
                                        --  ^ system layout
                                        --  ^ staff layout
                                        --  ^ scaling
                                        --  ^ appearance (line width etc)

data ScoreAttrs
    = ScoreAttrs
        [Int]                           --  ^ score version

data PartAttrs
    = PartAttrs
        String                          --  ^ part id

data MeasureAttrs
    = MeasureAttrs
        Int                             --  ^ measure number


-- ----------------------------------------------------------------------------------
-- Part list
-- ----------------------------------------------------------------------------------

newtype PartList = PartList { getPartList :: [PartListElem] }

instance Default PartList where
    def = PartList []

instance Semigroup PartList where
    PartList xs <> PartList ys = PartList (setIds $ xs <> ys)
        where
            setIds                                    = snd . List.mapAccumL setId partIds
            setId id (Part _ name abbr dname dabbrev) = (tail id, Part (head id) name abbr dname dabbrev)
            setId id x                                = (id, x)

            partIds = [ "P" ++ show n | n <- [1..] ]

instance Monoid PartList where
    mempty  = def
    mappend = (<>)

data PartListElem
    = Part
        String
        String
        (Maybe String)
        (Maybe String)
        (Maybe String)                  -- ^ id name abbrev? name-display? abbrev-display?
    | Group
        Level
        StartStop
        String
        (Maybe String)
        (Maybe GroupSymbol)
        (Maybe GroupBarlines)
        Bool                            -- ^ number start/stop name abbrev? symbol barline style

data GroupSymbol   = GroupBrace | GroupLine | GroupBracket | GroupSquare | NoGroupSymbol
data GroupBarlines = GroupBarLines | GroupNoBarLines | GroupMensurstrich

-- ----------------------------------------------------------------------------------
-- Music
-- ----------------------------------------------------------------------------------

newtype Music = Music { getMusic :: [MusicElem] }
    deriving (Semigroup, Monoid)

data MusicElem
    = MusicAttributes
        Attributes
    | MusicBackup
        Duration
    | MusicForward
        Duration
    | MusicNote
        Note
    | MusicDirection
        Direction
    | MusicBarline
        Barline
    | MusicHarmony                      -- TODO
    | MusicFiguredBass                  -- TODO
    | MusicPrint                        -- TODO
    | MusicSound                        -- TODO
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
    | Transpose
        Int -- diatonic
        Int -- chromatic
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
        noteInstrument   :: Maybe String,                       -- ^ instrument
        noteVoice        :: Maybe Natural,                      -- ^ voice
        noteType         :: Maybe NoteType,                     -- ^ type
        noteDots         :: Natural,                            -- ^ dots
        noteAccidental   :: Maybe (Accidental, Bool, Bool),     -- ^ accidental, cautionary, editorial
        noteTimeMod      :: Maybe (Natural, Natural),           -- ^ actual, normal
        noteStem         :: Maybe StemDirection,                -- ^ stem
        noteNoteHead     :: Maybe (NoteHead, Bool, Bool),       -- ^ notehead, filled, parentheses
        noteNoteHeadText :: Maybe String,                       -- ^ notehead-text
        noteStaff        :: Maybe Natural,                      -- ^ staff
        noteBeam         :: Maybe (Level, BeamType),            -- ^ beam-level, beam-type
        noteNotations    :: [Notation],                         -- ^ notation
        noteLyrics       :: [Lyric]                             -- ^ lyric
    }

noChord :: IsChord
noChord = False

noTies :: [Tie]
noTies = []


class HasNoteProps a where
    modifyNoteProps :: (NoteProps -> NoteProps) -> a -> a


instance HasNoteProps Note where
    modifyNoteProps f (Note x d t p)     = Note x d t (f p)
    modifyNoteProps f (CueNote x d p)    = CueNote x d (f p)
    modifyNoteProps f (GraceNote x t p)  = GraceNote x t (f p)

instance HasNoteProps MusicElem where
    modifyNoteProps f (MusicNote n) = MusicNote (modifyNoteProps f n)
    modifyNoteProps f x             = x



-- ----------------------------------------------------------------------------------
-- Notations
-- ----------------------------------------------------------------------------------

data Notation
     = Tied
        StartStopContinue               -- ^ type
     | Slur
        Level
        StartStopContinue               -- ^ level start/stop
     | Tuplet
        Level
        StartStopContinue               -- ^ level start/stop
     | Glissando
        Level
        StartStopContinue
        LineType
        (Maybe String)                  -- ^ level type start/stop text?
     | Slide
        Level
        StartStopContinue
        LineType
        (Maybe String)                  -- ^ level type start/stop text?
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
        StartStop                       -- ^ start/stop
    | Diminuendo
        StartStop                       -- ^ start/stop
    | Dynamics
        Dynamics
    | Dashes
        Level
        StartStop                       -- ^ level start/stop
    | Bracket                           -- TODO
    | Pedal
        StartStopChange
    | Metronome
        NoteVal
        Bool
        Tempo                           -- ^ noteVal isDotted bpm
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
-- Barlines/Repeats
-- ----------------------------------------------------------------------------------

-- TODO: footnote, level, wavyLine, segno, coda, fermata, ending, divisions
data Barline = Barline
             BarlineLocation
             BarStyle
             (Maybe Repeat)
 deriving (Show,Eq)

-- TODO: color
data BarStyle = BSRegular|BSDotted|BSDashed|BSHeavy|BSLightLight|
                BSLightHeavy|BSHeavyLight|BSHeavyHeavy|BSTick|
                BSShort|BSNone deriving (Eq,Enum)

instance Show BarStyle where
    show BSRegular = "regular"
    show BSDotted = "dotted"
    show BSDashed = "dashed"
    show BSHeavy = "heavy"
    show BSLightLight = "light-light"
    show BSLightHeavy = "light-heavy"
    show BSHeavyLight = "heavy-light"
    show BSHeavyHeavy = "heavy-heavy"
    show BSTick = "tick"
    show BSShort = "short"
    show BSNone = "none"

-- TODO: times
data Repeat = Repeat RepeatDirection deriving (Eq,Show)

data RepeatDirection = RepeatBackward|RepeatForward deriving (Eq)

instance Show RepeatDirection where
    show RepeatForward = "forward"
    show RepeatBackward = "backward"

data BarlineLocation = BLRight|BLLeft|BLMiddle deriving (Eq)

instance Show BarlineLocation where
    show BLRight = "right"
    show BLLeft = "left"
    show BLMiddle = "middle"



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
    | NoNoteHead                        -- ^ @"none"@

deriving instance Eq            Level
deriving instance Show          Level
deriving instance Num           Level

-- ----------------------------------------------------------------------------------

-- Bounded ints
type Max8 = Index N8

notImplemented x = error $ "Not implemented: " ++ x
