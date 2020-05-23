{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
module Data.Music.MusicXml.Score
  ( -----------------------------------------------------------------------------

    -- * Score
    -----------------------------------------------------------------------------
    Score (..),
    ScoreHeader (..),
    Identification (..),
    Creator (..),
    Defaults (..),
    ScoreAttrs (..),
    PartAttrs (..),
    MeasureAttrs (..),

    -- ** Part list
    PartList (..),
    PartListElem (..),
    GroupSymbol (..),
    GroupBarlines (..),
    -----------------------------------------------------------------------------

    -- * Music
    -----------------------------------------------------------------------------
    Music (..),
    MusicElem (..),
    -----------------------------------------------------------------------------

    -- ** Attributes
    Attributes (..),
    TimeSignature (..),
    ClefSign (..),
    OctaveChange (..),
    -----------------------------------------------------------------------------

    -- ** Notes
    Note (..),
    FullNote (..),
    IsChord,
    noChord,
    noTies,
    Tie,
    NoteProps (..),
    HasNoteProps (..),
    -----------------------------------------------------------------------------

    -- ** Notations
    Notation (..),
    FermataSign (..),
    Articulation (..),
    Ornament (..),
    Technical (..),
    -----------------------------------------------------------------------------

    -- ** Directions
    Direction (..),
    -----------------------------------------------------------------------------

    -- ** Barlines
    Barline (..),
    BarStyle (..),
    BarlineLocation (..),
    Repeat (..),
    RepeatDirection (..),
    -----------------------------------------------------------------------------

    -- ** Lyrics
    Lyric (..),
    -----------------------------------------------------------------------------

    -- * Basic types
    -----------------------------------------------------------------------------

    -----------------------------------------------------------------------------

    -- ** Pitch
    Pitch,
    DisplayPitch,
    PitchClass,
    Semitones (..),
    noSemitones,
    Octaves (..),
    Fifths (..),
    Line (..),
    Mode (..),
    Accidental (..),
    -----------------------------------------------------------------------------

    -- ** Time
    Duration,
    NoteType,
    Divs (..),
    NoteVal (..),
    NoteSize (..),
    Beat (..),
    BeatType (..),
    -----------------------------------------------------------------------------

    -- ** Dynamics
    Dynamics (..),
    -----------------------------------------------------------------------------

    -- ** Misc
    StemDirection (..),
    NoteHead (..),
    LineType (..),
    Level (..),
    BeamType (..),
    StartStop,
    StartStopChange,
    StartStopContinue,
    StartStopContinueChange (..),
  )
where

import Data.Default
import qualified Data.List as List
import Data.Music.MusicXml.Dynamics
import Data.Music.MusicXml.Pitch
import Data.Music.MusicXml.Time
import Data.Semigroup
import Numeric.Natural
import TypeUnary.Nat
import Prelude hiding (getLine)

-- ----------------------------------------------------------------------------------
-- Score
-- ----------------------------------------------------------------------------------

data Score
  = -- | music by part and time
    Partwise
      ScoreAttrs
      ScoreHeader
      [ ( PartAttrs,
          [(MeasureAttrs, Music)]
        )
      ]
  | -- | music by time and part
    Timewise
      ScoreAttrs
      ScoreHeader
      [ ( MeasureAttrs,
          [(PartAttrs, Music)]
        )
      ]
  deriving Show

data ScoreHeader
  = ScoreHeader
      { scoreTitle :: Maybe String,
        mvmNumber :: Maybe Int,
        mvmTitle :: Maybe String,
        scoreIdentification :: Maybe Identification,
            --  ^ defaults?
            --  ^ credit*
        scorePartList :: PartList
      }
  deriving Show

data Identification
  = Identification
      [Creator] --  ^ creator
  deriving Show

data Creator
  = Creator
      { creatorType :: String, -- (composer, lyricist, arranger etc)
        creatorName :: String
      }
  deriving Show

data Defaults
  = Defaults

--  ^ page layout (marigins, distance etc)
--  ^ system layout
--  ^ staff layout
--  ^ scaling
--  ^ appearance (line width etc)

data ScoreAttrs
  = ScoreAttrs
      [Int] --  ^ score version
  deriving Show

data PartAttrs
  = PartAttrs
      String --  ^ part id
  deriving Show

data MeasureAttrs
  = MeasureAttrs
      Int --  ^ measure number
  deriving Show

-- ----------------------------------------------------------------------------------
-- Part list
-- ----------------------------------------------------------------------------------

newtype PartList = PartList {getPartList :: [PartListElem]}
  deriving Show

instance Default PartList where
  def = PartList []

instance Semigroup PartList where
  PartList xs <> PartList ys = PartList (setIds $ xs <> ys)
    where
      setIds = snd . List.mapAccumL setId partIds
      setId id (Part _ name abbr dname dabbrev) = (tail id, Part (head id) name abbr dname dabbrev)
      setId id x = (id, x)
      partIds = ["P" ++ show n | n <- [1 ..]]

instance Monoid PartList where

  mempty = def

  mappend = (<>)

data PartListElem
  = -- | id name abbrev? name-display? abbrev-display?
    Part
      String
      String
      (Maybe String)
      (Maybe String)
      (Maybe String)
      -- score-instrument?
      -- midi-device?
      -- midi-instrument?
  | -- | number start/stop name? abbrev? symbol barline style
    Group
      Level
      StartStop
      (Maybe String)
      (Maybe String)
      (Maybe GroupSymbol)
      (Maybe GroupBarlines)
      Bool
  deriving Show

data GroupSymbol = GroupBrace | GroupLine | GroupBracket | GroupSquare | NoGroupSymbol
  deriving Show

data GroupBarlines = GroupBarLines | GroupNoBarLines | GroupMensurstrich
  deriving Show

-- ----------------------------------------------------------------------------------
-- Music
-- ----------------------------------------------------------------------------------

newtype Music = Music {getMusic :: [MusicElem]}
  deriving (Semigroup, Monoid, Show)

data MusicElem
  = MusicAttributes
      [Attributes]
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
  | MusicHarmony -- TODO
  | MusicFiguredBass -- TODO
  | MusicPrint -- TODO
  | MusicSound -- TODO
  | MusicGrouping -- TODO
  | MusicLink -- TODO
  | MusicBookmark -- TODO
  deriving Show

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
  | PartSymbol -- TODO
  | Instruments
      Natural
  | Clef
      ClefSign
      Line
      (Maybe OctaveChange)
  | StaffDetails -- TODO
  | Transpose
      Int -- diatonic
      Int -- chromatic
      (Maybe OctaveChange)
  | Directive -- TODO
  | MeasureStyle -- TODO
  deriving Show

data TimeSignature
  = CommonTime
  | CutTime
  | DivTime
      Beat
      BeatType
  deriving Show

data ClefSign
  = GClef
  | CClef
  | FClef
  | PercClef
  | TabClef
  deriving (Eq, Ord, Enum, Bounded, Show)

newtype OctaveChange = OctaveChange {getOctaveChage :: Int}
  deriving Show

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
  deriving Show

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
  deriving Show

type IsChord = Bool

type Tie = StartStop

data NoteProps
  = NoteProps
      { -- | instrument
        noteInstrument :: Maybe String,
        -- | voice
        noteVoice :: Maybe Natural,
        -- | type
        noteType :: Maybe NoteType,
        -- | dots
        noteDots :: Natural,
        -- | accidental, cautionary, editorial
        noteAccidental :: Maybe (Accidental, Bool, Bool),
        -- | actual, normal
        noteTimeMod :: Maybe (Natural, Natural),
        -- | stem
        noteStem :: Maybe StemDirection,
        -- | notehead, filled, parentheses
        noteNoteHead :: Maybe (NoteHead, Bool, Bool),
        -- | display-text, accidental-text
        noteNoteHeadText :: Maybe (String, Accidental),
        -- | staff
        noteStaff :: Maybe Natural,
        -- | beam-level, beam-type
        noteBeam :: Maybe (Level, BeamType),
        -- | notation
        noteNotations :: [Notation],
        -- | lyric
        noteLyrics :: [Lyric]
      }
  deriving Show

noChord :: IsChord
noChord = False

noTies :: [Tie]
noTies = []

class HasNoteProps a where
  modifyNoteProps :: (NoteProps -> NoteProps) -> a -> a

instance HasNoteProps Note where
  modifyNoteProps f (Note x d t p) = Note x d t (f p)
  modifyNoteProps f (CueNote x d p) = CueNote x d (f p)
  modifyNoteProps f (GraceNote x t p) = GraceNote x t (f p)

instance HasNoteProps MusicElem where
  modifyNoteProps f (MusicNote n) = MusicNote (modifyNoteProps f n)
  modifyNoteProps _ x = x

-- ----------------------------------------------------------------------------------
-- Notations
-- ----------------------------------------------------------------------------------

data Notation
  = -- | type
    Tied
      StartStopContinue
  | -- | level start/stop
    Slur
      Level
      StartStopContinue
  | -- | level start/stop
    Tuplet
      Level
      StartStopContinue
  | -- | level type start/stop text?
    Glissando
      Level
      StartStopContinue
      LineType
      (Maybe String)
  | -- | level type start/stop text?
    Slide
      Level
      StartStopContinue
      LineType
      (Maybe String)
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
  deriving Show

data FermataSign = NormalFermata | AngledFermata | SquaredFermata
  deriving Show

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
  deriving Show

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
      Natural -- TODO restrict to (1..8) range
  | OtherOrnament
      String
  deriving Show

data Technical
  = UpBow
  | DownBow
  | Harmonic
  | OpenString
  | ThumbPosition
  | Fingering
      Natural -- TODO restrict to (0..5)?
  | Pluck
  | DoubleTongue
  | TripleTongue
  | Stopped
  | SnapPizzicato
  | Fret
  | String
      Natural
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
  deriving Show

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
  | -- | start/stop
    Crescendo
      StartStop
  | -- | start/stop
    Diminuendo
      StartStop
  | Dynamics
      Dynamics
  | -- | level start/stop
    Dashes
      Level
      StartStop
  | Bracket -- TODO
  | Pedal
      StartStopContinueChange
  | -- | noteVal isDotted bpm
    Metronome
      NoteVal
      Bool
      Tempo
  | OctaveShift -- TODO
  | HarpPedals -- TODO
  | Damp -- TODO
  | DampAll -- TODO
  | EyeGlasses -- TODO
  | StringMute -- TODO
  | Scordatura -- TODO
  | Image -- TODO
  | PrincipalVoice -- TODO
  | AccordionRegistration -- TODO
  | Percussion -- TODO
  | OtherDirection
      String
  deriving Show

-- ----------------------------------------------------------------------------------
-- Barlines/Repeats
-- ----------------------------------------------------------------------------------

-- TODO: footnote, level, wavyLine, segno, coda, fermata, ending, divisions
data Barline
  = Barline
      BarlineLocation
      BarStyle
      (Maybe Repeat)
  deriving (Show, Eq)

-- TODO: color
data BarStyle
  = BSRegular
  | BSDotted
  | BSDashed
  | BSHeavy
  | BSLightLight
  | BSLightHeavy
  | BSHeavyLight
  | BSHeavyHeavy
  | BSTick
  | BSShort
  | BSNone
  deriving (Eq, Enum)

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
data Repeat = Repeat RepeatDirection deriving (Eq, Show)

data RepeatDirection = RepeatBackward | RepeatForward deriving (Eq)

instance Show RepeatDirection where
  show RepeatForward = "forward"
  show RepeatBackward = "backward"

data BarlineLocation = BLRight | BLLeft | BLMiddle deriving (Eq)

instance Show BarlineLocation where
  show BLRight = "right"
  show BLLeft = "left"
  show BLMiddle = "middle"

-- ----------------------------------------------------------------------------------
-- Lyrics
-- ----------------------------------------------------------------------------------

data Lyric = Lyric -- TODO
  deriving Show

-- ----------------------------------------------------------------------------------
-- Basic types
-- ----------------------------------------------------------------------------------

newtype Level = Level {getLevel :: Max8}

data BeamType
  = BeginBeam
  | ContinueBeam
  | EndBeam
  | ForwardHook
  | BackwardHook
  deriving Show

type StartStop = StartStopContinueChange

type StartStopChange = StartStopContinueChange

type StartStopContinue = StartStopContinueChange

data StartStopContinueChange
  = Start
  | Stop
  | Continue
  | Change
  deriving Show

data StemDirection
  = StemDown
  | StemUp
  | StemNone
  | StemDouble
  deriving Show

data LineType
  = Solid
  | Dashed
  | Dotted
  | Wavy
  deriving Show

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
  | -- | @"none"@
    NoNoteHead
  deriving Show

deriving instance Eq Level

deriving instance Show Level

deriving instance Num Level

-- ----------------------------------------------------------------------------------

-- Bounded ints
type Max8 = Index N8

notImplemented x = error $ "Not implemented: " ++ x

-- ----------------------------------------------------------------------------------
-- Default instances
-- ----------------------------------------------------------------------------------

instance Default ScoreAttrs where
  def = ScoreAttrs []

instance Default ScoreHeader where
  def = ScoreHeader Nothing Nothing Nothing Nothing mempty

instance Default Note where
  def = Note def def [] def

instance Default FullNote where
  def = Rest noChord Nothing

instance Default NoteProps where
  def = NoteProps Nothing Nothing (Just (1 / 4, Nothing)) 0 Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] []
