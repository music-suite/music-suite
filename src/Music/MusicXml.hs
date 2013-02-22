
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
        Divs(..),
        NoteVal(..),
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

-- import Control.Arrow
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
        [Int]                           --  version

data MeasureAttrs
    = MeasureAttrs
        Int                             --   number

data PartAttrs
    = PartAttrs
        String                          --   id


-- This instance is used by toXml and should return a single list
instance Out Score where
    out (Partwise attr header parts)
        = single . unode "score-partwise" $ out header <> [{-parts-}]
    out (Timewise attr header measures)
        = single . unode "timewise-score" $ out header <> [{-parts-}]

instance Out ScoreHeader where
    out (ScoreHeader title mvm ident partList)
        = mempty <> outTitle title <> outMvm mvm
                 <> outIdent ident <> outPartList partList
        where
            outTitle, outMvm :: Maybe String -> [Element]
            outIdent :: Maybe Identification -> [Element]
            outPartList :: [PartListElem] -> [Element]

            outTitle    = fmap (unode "title") . maybeToList
            outMvm      = fmap (unode "movement-title") . maybeToList
            outIdent    = single . unode "identification" . (out =<<) . maybeToList
            outPartList = single . unode "part-list" . (out =<<)

instance Out Identification where
    out (Identification creators) = map outCreator creators
        where
            outCreator (Creator t n) = unode "creator" (uattr "type" t, n)


-- --------------------------------------------------------------------------------
-- Part list
-- --------------------------------------------------------------------------------

type PartList = [PartListElem]

data PartListElem
    = Part String String (Maybe String) -- id name abbrev?
    | Group String (Maybe String)       -- name abbrev

instance Out PartListElem where
    out (Part id name abbrev)   =
            single $ unode "score-part"
                        ([Attr (unqual "id") id], outName name <> outAbbrev abbrev)
        where
            outName   = single . unode "part-name"
            outAbbrev = maybeToList . fmap (unode "part-abbreviation")

    out (Group name abbrev) = notImplemented "Out instance for PartListElem.Group"

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
    = Divisions Divs
    | Clef      ClefSign Line
    | Key       Fifths Mode
    | Time      TimeSignature

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

type Duration     = Divs
type NoteType     = (NoteVal, NoteSize)
type Pitch        = (Steps, Maybe Semitones, Octaves)
type DisplayPitch = (Steps, Octaves)

noSemitones = Nothing

newtype Divs = Divs { getDivs :: Int }                      -- absolute dur in ticks
    deriving (Eq, Ord, Num, Enum)
newtype NoteVal = NoteVal { getNoteVal :: Rational }        -- relative dur in notated time
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

-- unode :: Node t => String -> t -> Element

uattr :: String -> String -> Attr
uattr n = Attr (unqual n)

sep :: a -> [a] -> [a]
sep = List.intersperse

concatSep :: [a] -> [[a]] -> [a]
concatSep x = concat . sep x

single :: a -> [a]
single = return

fromSingle :: [a] -> a
fromSingle [x] = x
fromSingle _   = error "fromSingle: non-single list"

notImplemented x = error $ "Not implemented: " ++ x
