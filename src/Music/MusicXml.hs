
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
-- A Haskell representation of MusicXML.
--
-- For an introduction, see <http://www.makemusic.com/musicxml/tutorial>.
--
-------------------------------------------------------------------------------------

module Music.MusicXml (
        -- * Score
        Score(..),
        ScoreAttrs(..),
        ScoreHeader(..),
        Identification(..),
        Creator(..),
        Defaults(..),
        MeasureAttrs(..),
        -- ** Part list
        PartList,
        PartListElem(..),

        -- * Music
        Music,
        MusicElem(..),

        -- ** Attributes
        Attributes(..),
        TimeSignature(..),
        Mode(..),
        ClefSign(..),

        -- ** Notes
        Note(..),      
        noTies,
        FullNote(..),
        chord, noChord,
        NoteProps(..),
        Tie(..),
        TieNotation(..),

        -- ** Directions
        Direction(..),



        -- * Basic types
        -- ** Enum types
        NoteSize(..),
        Accidental(..),
        Level(..),

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
        Duration(..),
        NoteType(..),
        -- *** Pitch
        Pitch(..),
        noSemitones,
        DisplayPitch(..),

        -- * Import and export functions
        toXml,
        showXml
 ) where

import Data.Maybe (maybeToList)
import Data.Semigroup
import Data.Default
import Text.XML.Light hiding (Line)

import qualified Data.List as List

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

-- This instance is used by toXml and must return a single list
instance Out Score where

    out (Partwise attr hdr parts) 
        = single $ attrs attr $ unode "score-partwise" (out hdr ++ [{-parts-}])
        where
            attrs a = id -- TODO

    out (Timewise attr hdr measures) 
        = single $ attrs attr $ unode "timewise-score" (out hdr ++ [{-parts-}])
        where
            attrs a = id -- TODO

            -- attrs (ScoreAttrs as) 
            --     = addAttr $ Attr (unqual "version") (concatSep "." $ map show as)


-- instance Out

data ScoreAttrs
    = ScoreAttrs
        [Int]                       -- version

data ScoreHeader
    = ScoreHeader    
        (Maybe String)              -- title
        (Maybe String)              -- movement title
        (Maybe Identification)
                                    -- identification?
                                    -- defaults?
                                    -- credit*
        PartList                    -- partlist?

instance Out ScoreHeader where
    out (ScoreHeader title mvm ident partList) 
        = titleN ++ mvmN ++ identN ++ concatMap partListN partList
        where
            titleN      = single $ unode "title" ()
            mvmN        = single $ unode "movement-title" ()
            identN      = single $ unode "identification" (concatMap out (maybeToList ident))
            partListN p = single $ unode "part-list" ()

data Identification 
    = Identification
        [Creator]                   -- Creator
                                    -- TODO

instance Out Identification where
    out (Identification creators) 
        = map creatorN creators
        where
            creatorN (Creator t n) = unode "creator" (Attr (unqual "type") t, n)

data Creator
    = Creator 
        String                      -- Type (composer, lyricist, arranger etc)
        String                      -- Name
        
data Defaults
    = Defaults
                                    --   page layout (marigins, distance etc)
                                    --   system layout
                                    --   staff layout
                                    --   scaling
                                    --   appearance (line width etc)

-- TODO fancy numbers
data MeasureAttrs
    = MeasureAttrs
        Int                         -- TODO only support simple number for now


-- --------------------------------------------------------------------------------
-- Part list
-- --------------------------------------------------------------------------------

type PartList = [PartListElem]

data PartListElem
    = Part String String (Maybe String) -- id name abbrev?
    | Group String (Maybe String)       -- name abbrev

--   TODO instr midi-device midi-instr
--   TODO symbol barline time?


-- --------------------------------------------------------------------------------
-- Music
-- --------------------------------------------------------------------------------

type Music = [MusicElem]

data MusicElem
    = MusicAttributes Attributes
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

data Attributes
    = Div  Divisions
    | Clef ClefSign Line
    | Key  Fifths Mode
    | Time TimeSignature

data TimeSignature
    = CommonTime
    | CutTime
    | DivTime Beats BeatTypes

data Mode 
    = Major 
    | Minor 
    | Dorian 
    | Phrygian 
    | Lydian 
    | Mixolydian 
    | Aeolian 
    | Ionian 
    | Locrian 
    | NoMode

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
        Duration
        [Tie]
        -- NoteProps
    | CueNote
        FullNote
        Duration
        -- NoteProps
    | GraceNote
        FullNote
        [Tie]
        -- NoteProps

noTies = []


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

chord   = True
noChord = False

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
    | Dynamic Level
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

type Duration     = Divisions
type NoteType     = (NoteValue, NoteSize)
type Pitch        = (Steps, Maybe Semitones, Octaves)
type DisplayPitch = (Steps, Octaves)

noSemitones = Nothing

newtype Divisions = Divisions { getDivisions :: Int }       -- absolute dur in ticks
    deriving (Eq, Ord, Num, Enum)
newtype NoteValue = NoteValue { getNoteValue :: Rational }  -- relative dur in notated time
    deriving (Eq, Ord, Num, Enum)


newtype Octaves   = Octaves { getOctaves :: Int }
    deriving (Eq, Ord, Num, Enum)
newtype Steps     = Steps { getSteps :: Int }
    deriving (Eq, Ord, Num, Enum)
newtype Semitones = Semitones { getSemitones :: Double }    -- microtones allowed
    deriving (Eq, Ord, Num, Enum)
newtype Line      = Line { getLine :: Int }                 -- line number, from bottom
    deriving (Eq, Ord, Num, Enum)

newtype Fifths    = Fifths { getFifths :: Int }             -- number of fifths, upwards, starting from C
    deriving (Eq, Ord, Num, Enum)
newtype Beats     = Beats { getBeats :: Int }               -- time nominator
    deriving (Eq, Ord, Num, Enum)
newtype BeatTypes = BeatTypes { getBeatTypes :: Int }       -- time denominator
    deriving (Eq, Ord, Num, Enum)


data NoteSize     = SizeFull | SizeCue | SizeLarge
    deriving (Eq, Ord, Enum, Bounded)
data Accidental   = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
    deriving (Eq, Ord, Enum, Bounded)
data Level = PPP | PP | P | MP | MF | F | FF | FFF
    deriving (Eq, Ord, Enum, Bounded)


-- --------------------------------------------------------------------------------
-- Import and export functions
-- --------------------------------------------------------------------------------

-- |
-- Render a score as a MusicXML string.
showXml :: Score -> String
showXml = ppTopElement . toXml

-- |
-- Render a score as MusicXML.
toXml :: Score -> Element
toXml = fromSingle . out


-- --------------------------------------------------------------------------------

-- TODO We would like to hide this from the docs

class Out a where
    out :: a -> [Element]

addAttr  = add_attr
addAttrs = add_attrs
-- qnode n cs = node (QName n Nothing Nothing) cs

sep :: a -> [a] -> [a]
sep = List.intersperse

concatSep :: [a] -> [[a]] -> [a]
concatSep x = concat . sep x

single :: a -> [a]
single = return

fromSingle :: [a] -> a
fromSingle [x] = x
fromSingle _   = error "fromSingle: non-single list"

