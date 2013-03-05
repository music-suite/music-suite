
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
        Tie(..),
        noTies,
        NoteProps(..),
        mapNoteProps,
        mapNoteProps2,

        -----------------------------------------------------------------------------
        -- ** Notations
        
        Notation(..),

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
        
        Dynamics,

        -----------------------------------------------------------------------------
        -- ** Misc
        
        StemDirection(..),
        NoteHead(..),

        DashLevel(..),
        BeamLevel(..),
        SlurLevel(..),
        TupletLevel(..),

        BeamType(..),
        StartStopContinue(..),

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
        String
        (Maybe String)                  -- name abbrev


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
    | Staves                            -- TODO
    | PartSymbol                        -- TODO
    | Instruments                       -- TODO
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

data Tie
    = TieStart
    | TieStop

data NoteProps
    = NoteProps {
        noteInstrument   :: Maybe String,                       -- instrument
        noteVoice        :: Maybe Natural,                      -- voice
        noteType         :: Maybe NoteType,                     -- type
        noteDots         :: Natural,                            -- dots
        noteAccidental   :: Maybe (Accidental, Bool, Bool),     -- accidental, cautionary, editorial
        noteTimeMod      :: Maybe (Natural, Natural),           -- actual, normal
        noteStem         :: Maybe StemDirection,                         -- stem
        noteNoteHead     :: Maybe (NoteHead, Bool, Bool),       -- notehead, filled, parentheses
        noteNoteHeadText :: Maybe String,                       -- notehead-text
        noteStaff        :: Maybe Natural,                      -- staff
        noteBeam         :: Maybe (BeamLevel, BeamType),        -- beam-level, beam-type
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

-- TODO
data Notation
     = Tied                         
        StartStopContinue               -- type
     | Slur                         
        SlurLevel 
        StartStopContinue               -- level type
     | Tuplet                       
        TupletLevel 
        StartStopContinue 
        Bool                            -- level type bracket
     | Glissando                        -- TODO line type: solid/dotted/dashed, number, start/stop, text?
     | Slide                            -- TODO line type: solid/dotted/dashed, number, start/stop, text?
     | Ornaments                        -- TODO TODO
     | Technical                        -- TODO TODO
     | Articulations                    -- TODO TODO
     | DynamicNotation                  
        Dynamics                        
     | Fermata                          -- TODO ferm-type sign
     | Arpeggiate                       -- TODO bottom/top?
     | NonArpeggiate                    -- TODO bottom/top?
     | AccidentalMark                   
        Accidental
     | OtherNotation                
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
        Bool -- start/stop
    | Diminuendo                    
        Bool -- start/stop
    | Dynamics                      
        Dynamics
    | Dashes                        
        DashLevel 
        Bool -- level start/stop
    | Bracket                           -- TODO TODO
    | Pedal                         
        Bool -- start/change/stop
    | Metronome                         -- TODO unit bpm
    | OctaveShift                       -- TODO size: 8/15, up/down/stop
    | HarpPedals                        -- TODO TODO
    | Damp                              -- TODO TODO
    | DampAll                           -- TODO TODO
    | EyeGlasses                        -- TODO TODO
    | StringMute                        -- TODO TODO
    | Scordatura                        -- TODO TODO
    | Image                             -- TODO TODO
    | PrincipalVoice                    -- TODO TODO
    | AccordionRegistration             -- TODO TODO
    | Percussion                        -- TODO TODO
    | OtherDirection                    
        String


-- ----------------------------------------------------------------------------------
-- Lyrics
-- ----------------------------------------------------------------------------------

data Lyric = Lyric -- TODO


-- ----------------------------------------------------------------------------------
-- Basic types
-- ----------------------------------------------------------------------------------

newtype DashLevel   = DashLevel { getDashLevel :: Max8 }
newtype BeamLevel   = BeamLevel { getBeamLevel :: Max8 }
newtype SlurLevel   = SlurLevel { getSlurLevel :: Max8 }
newtype TupletLevel = TupletLevel { getTupletLevel :: Max8 }

data BeamType
    = BeginBeam
    | ContinueBeam
    | EndBeam
    | ForwardHook
    | BackwardHook

data StartStopContinue
    = Start
    | Stop
    | Continue

data StemDirection
    = StemDown 
    | StemUp 
    | StemNone 
    | StemDouble

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

deriving instance Eq            BeamLevel
deriving instance Show          BeamLevel
deriving instance Num           BeamLevel

deriving instance Eq            TupletLevel
deriving instance Show          TupletLevel
deriving instance Num           TupletLevel

deriving instance Eq            SlurLevel
deriving instance Show          SlurLevel
deriving instance Num           SlurLevel

-- ----------------------------------------------------------------------------------

-- Bounded ints
type Max8 = Index N8

notImplemented x = error $ "Not implemented: " ++ x


