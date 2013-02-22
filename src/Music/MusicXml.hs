
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
        Beat(..),
        BeatType(..),

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

import Prelude hiding (getLine)

import Data.Maybe (maybeToList)
import Data.Semigroup
import Data.Default
import Text.XML.Light hiding (Line)

import qualified Data.List as List
import qualified Data.Char as Char

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
        [Int]                           --  score version

data PartAttrs
    = PartAttrs
        String                          --  part id

data MeasureAttrs
    = MeasureAttrs
        Int                             --  measure number



addScoreAttrs :: ScoreAttrs -> Element -> Element
addScoreAttrs (ScoreAttrs []) = id
addScoreAttrs (ScoreAttrs xs) = addAttr (uattr "version" $ concatSep "." $ map show xs)

addPartAttrs :: PartAttrs -> Element -> Element
addPartAttrs (PartAttrs id) = addAttr (uattr "id" id)

addMeasureAttrs :: MeasureAttrs -> Element -> Element
addMeasureAttrs (MeasureAttrs n) = addAttr (uattr "number" $ show n)


-- This instance is used by toXml and should return a single list
instance Out Score where
    out (Partwise attr header parts)
        = single . unode "score-partwise" $ out header <> outPartwise parts
    out (Timewise attr header measures)
        = single . unode "timewise-score" $ out header <> outTimewise measures

outPartwise :: [(PartAttrs, [(MeasureAttrs, Music)])] -> [Element]
outPartwise = fmap (\(partAttrs, measures) -> outPar partAttrs 
            $ fmap (\(measureAttrs, music) -> outMes measureAttrs $ outMus music) measures)

outTimewise :: [(MeasureAttrs, [(PartAttrs, Music)])] -> [Element]
outTimewise = fmap (\(measureAttrs, parts) -> outMes measureAttrs 
            $ fmap (\(partAttrs, music) -> outPar partAttrs $ outMus music) parts)

outPar a xs = addPartAttrs a    $ unode "part"    xs
outMes a xs = addMeasureAttrs a $ unode "measure" xs
outMus = concatMap out


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

instance Out MusicElem where
    out (MusicAttributes x) = single $ unode "attributes" $ out x
    out (MusicNote x)       = single $ unode "note"       $ () --out x
    out (MusicDirection x)  = single $ unode "direction"  $ () --out x

-- --------------------------------------------------------------------------------
-- Attributes
-- --------------------------------------------------------------------------------

-- TODO multi-staff

data Attributes
    = Divisions Divs
    | Clef      ClefSign Line   -- sign line-from-bottom
    | Key       Fifths Mode
    | Time      TimeSignature

data TimeSignature
    = CommonTime
    | CutTime
    | DivTime Beat BeatType

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
    deriving (Eq, Ord, Show)  
modeName :: Mode -> String
modeName = toLowerString . show

data ClefSign = GClef | CClef | FClef | PercClef | TabClef
    deriving (Eq, Ord, Enum, Bounded)

clefName GClef    = "G"
clefName CClef    = "C"
clefName FClef    = "F"
clefName PercClef = "percussion"
clefName TabClef  = "tab"

-- Staves
-- Transposition

instance Out Attributes where
    out (Divisions divs)                  = single $ unode "divisions" 
                                                   $ show $ getDivs divs

    out (Clef sign line)                  = single $ unode "clef" 
                                                        [ unode "sign" (clefName sign), 
                                                          unode "line" (show $ getLine line)]

    out (Key fifths mode)                 = single $ unode "key" 
                                                        [ unode "fifths" (show $ getFifths fifths), 
                                                          unode "mode" (modeName mode)]

    out (Time (CommonTime))               = single $ addAttr (uattr "symbol" "common") 
                                                   $ unode "time" 
                                                        [ unode "beat" (show 4), 
                                                          unode "beat-type" (show 4)]

    out (Time (CutTime))                  = single $ addAttr (uattr "symbol" "cut") 
                                                   $ unode "time" 
                                                        [ unode "beat" (show 4), 
                                                          unode "beat-type" (show 4) ]

    out (Time (DivTime beats beatType))   = single $ unode "time" 
                                                        [ unode "beat" (show $ getBeat beats), 
                                                          unode "beat-type" (show $ getBeatType beatType)]

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

instance Out Note where
    out = notImplemented "Out instance"

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

instance Out Direction where
    out = notImplemented "Out instance"

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
newtype Beat     = Beat { getBeat :: Int }                  -- time nominator
    deriving (Eq, Ord, Num, Enum)
newtype BeatType = BeatType { getBeatType :: Int }          -- time denominator
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

-- |
-- Synonym for 'Char.toUpper'
toUpperChar :: Char -> Char
toUpperChar = Char.toUpper
 
-- |
-- Synonym for 'Char.toLower'
toLowerChar :: Char -> Char
toLowerChar = Char.toLower
 
-- |
-- Synonym for 'fmap Char.toUpper'
toUpperString :: String -> String
toUpperString = fmap Char.toUpper
 
-- |
-- Synonym for 'fmap Char.toLower'
toLowerString :: String -> String
toLowerString = fmap Char.toLower
 
-- |
-- Convert a string to use upper case for the leading letter and lower case for
-- remaining letters.
toCapitalString :: String -> String
toCapitalString [] = []
toCapitalString (x:xs) = toUpperChar x : toLowerString xs

single :: a -> [a]
single = return

fromSingle :: [a] -> a
fromSingle [x] = x
fromSingle _   = error "fromSingle: non-single list"

notImplemented x = error $ "Not implemented: " ++ x
