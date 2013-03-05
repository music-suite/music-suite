
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
        mapNoteProps,
        mapNoteProps2,
        Tie(..),
        TieNotation(..),

        -- ** Directions
        Direction(..),



        -- * Basic types
        -- ** Pitch
        Pitch(..),
        DisplayPitch(..),
        PitchClass,
        Semitones(..),
        noSemitones,

        Octaves(..),
        Fifths(..),
        Line(..),

        Accidental(..),

        -- ** Time
        Duration(..),
        NoteType(..),

        Divs(..),
        NoteVal(..),
        NoteSize(..),

        Beat(..),
        BeatType(..),

        -- ** Dynamics
        Level,       
        
        -- ** Misc
        BeamLevel,
        BeamType(..),

        -- * Import and export functions
        toXml,
        showXml
 ) where

-- import Control.Arrow

import Prelude hiding (getLine)

import Data.Maybe (maybeToList)
import Data.Semigroup
import Data.Default
import Numeric.Natural

-- Probably always use this, factor out of this module anyway?
import Text.XML.Light hiding (Line)

-- We will try to use bounded integers for beam levels etc
import TypeUnary.Nat

import Music.MusicXml.Time
import Music.MusicXml.Pitch
import Music.MusicXml.Dynamics
import Music.MusicXml.Read
import Music.MusicXml.Write

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


-- This instance is used by toXml and should return a single list
instance WriteMusicXml Score where
    write (Partwise attr
                    header
                    parts) = single
                           $ unode "score-partwise"
                           -- TODO attrs
                           $ write header <> writePartwise parts

    write (Timewise attr
                    header
                    measures) = single
                              $ unode "timewise-score"
                              $ write header <> writeTimewise measures

writePartwise :: [(PartAttrs, [(MeasureAttrs, Music)])] -> [Element]
writeTimewise :: [(MeasureAttrs, [(PartAttrs, Music)])] -> [Element]

writePartwise = fmap (\(attrs, measures) -> writePartElem attrs $
                    fmap (\(attrs, music) -> writeMeasureElem attrs $
                        writeMusic music) measures)

writeTimewise = fmap (\(attrs, parts) -> writeMeasureElem attrs $
                    fmap (\(attrs, music) -> writePartElem attrs $
                        writeMusic music) parts)

writePartElem attrs     = addPartAttrs attrs    . unode "part"
writeMeasureElem attrs  = addMeasureAttrs attrs . unode "measure"
writeMusic              = concatMap write

addScoreAttrs   :: ScoreAttrs   -> Element -> Element
addPartAttrs    :: PartAttrs    -> Element -> Element
addMeasureAttrs :: MeasureAttrs -> Element -> Element

addScoreAttrs   (ScoreAttrs [])  = id
addScoreAttrs   (ScoreAttrs xs)  = addAttr (uattr "version" $ concatSep "." $ map show xs)

addPartAttrs    (PartAttrs x)    = addAttr (uattr "id" x)
addMeasureAttrs (MeasureAttrs n) = addAttr (uattr "number" $ show n)


instance WriteMusicXml ScoreHeader where
    write (ScoreHeader title
                       mvm
                       ident
                       partList) = mempty <> writeTitle title
                                          <> writeMvm mvm
                                          <> writeIdent ident
                                          <> writePartList partList
        where {
            writeTitle, writeMvm :: Maybe String -> [Element]                           ;
            writeIdent           :: Maybe Identification -> [Element]                   ;
            writePartList        :: [PartListElem] -> [Element]                         ;

            writeTitle    = fmap (unode "title") . maybeToList                          ;
            writeMvm      = fmap (unode "movement-title") . maybeToList                 ;
            writeIdent    = single . unode "identification" . (write =<<) . maybeToList ;
            writePartList = single . unode "part-list" . (write =<<)                    ;
        }

instance WriteMusicXml Identification where
    write (Identification creators) = map writeCreator creators
        where
            writeCreator (Creator t n) = unode "creator" (uattr "type" t, n)


-- --------------------------------------------------------------------------------
-- Part list
-- --------------------------------------------------------------------------------

type PartList = [PartListElem]

data PartListElem
    = Part String String (Maybe String) -- id name abbrev?
    | Group String (Maybe String)       -- name abbrev

instance WriteMusicXml PartListElem where
    write (Part id
                name
                abbrev) = single
                        $ unode "score-part"
                        $ (single $ uattr "id" id,
                           writeName name <> writeAbbrev abbrev)
        where
            writeName   = single . unode "part-name"
            writeAbbrev = maybeToList . fmap (unode "part-abbreviation")

    write (Group name abbrev) = notImplemented "WriteMusicXml instance for PartListElem.Group"

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

instance WriteMusicXml MusicElem where
    write (MusicAttributes x) = single $ unode "attributes" $ write x
    write (MusicNote x)       = single $ unode "note"       $ write x
    write (MusicDirection x)  = single $ unode "direction"  $ () --write x

mapNoteProps2 :: (NoteProps -> NoteProps) -> MusicElem -> MusicElem
mapNoteProps2 f (MusicNote n) = MusicNote (mapNoteProps f n)
mapNoteProps2 f x             = x

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

clefName :: ClefSign -> String
clefName GClef    = "G"
clefName CClef    = "C"
clefName FClef    = "F"
clefName PercClef = "percussion"
clefName TabClef  = "tab"

-- Staves
-- Transposition

instance WriteMusicXml Attributes where
    write (Divisions divs)                  = single $ unode "divisions"
                                                   $ show $ getDivs divs

    write (Clef sign line)                  = single $ unode "clef"
                                                        [ unode "sign" (clefName sign),
                                                          unode "line" (show $ getLine line)]

    write (Key fifths mode)                 = single $ unode "key"
                                                        [ unode "fifths" (show $ getFifths fifths),
                                                          unode "mode" (modeName mode)]

    write (Time (CommonTime))               = single $ addAttr (uattr "symbol" "common")
                                                   $ unode "time"
                                                        [ unode "beats" (show 4),
                                                          unode "beat-type" (show 4)]

    write (Time (CutTime))                  = single $ addAttr (uattr "symbol" "cut")
                                                   $ unode "time"
                                                        [ unode "beats" (show 2),
                                                          unode "beat-type" (show 2) ]

    write (Time (DivTime beats beatType))   = single $ unode "time"
                                                        [ unode "beats" (show $ getBeat beats),
                                                          unode "beat-type" (show $ getBeatType beatType)]

-- --------------------------------------------------------------------------------
-- Notes
-- --------------------------------------------------------------------------------

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

noTies :: [Tie]
noTies = []

mapNoteProps :: (NoteProps -> NoteProps) -> Note -> Note
mapNoteProps f (Note x d t p)     = Note x d t (f p)
mapNoteProps f (CueNote x d p)    = CueNote x d (f p)
mapNoteProps f (GraceNote x t p)  = GraceNote x t (f p)


data FullNote
    = Pitched       -- isChord pitch
        Bool
        Pitch
    | Unpitched     -- isChord disp
        Bool
        (Maybe DisplayPitch)
    | Rest          -- isChord disp
        Bool
        (Maybe DisplayPitch)

chord   = True
noChord = False

-- TODO level
data Tie
    = TieStart
    | TieStop

data NoteProps
    = NoteProps {
        -- instrument
        noteVoice       :: Maybe Int,                       -- TODO bounds?
        noteType        :: Maybe NoteType,
        noteDots        :: Natural,                         -- TODO bounds?
        -- accidental                                   -- TODO for unusual spelling
        noteTimeMod     :: Maybe (Int, Int),                -- actual, normal 
        -- stem
        -- notehead
        -- notehead-text
        -- staff
        noteBeam        :: Maybe (BeamLevel, BeamType),
        noteNotations   :: [Notation]
        -- lyrics
        -- play
    }


instance WriteMusicXml NoteProps where
    write (NoteProps
            voice
            typ
            dots
            timeMod -- FIXME
            beam
            notations) 
                = mempty <> maybeOne (\(noteVal, noteSize) -> unode "type" (noteValName noteVal)) typ
                         <> replicate (fromIntegral dots) (unode "dot" ())
                         <> maybeOne (\n -> unode "voice" $ show n) voice
                         <> maybeOne (\(n, typ) -> addAttr (uattr "number" $ show $ getBeamLevel n) 
                                            $ unode "beam" $ show typ) beam

-- TODO voice, beam


noteValName :: NoteVal -> String
noteValName (NoteVal x)
    | x == (1/1024) = "1024th"
    | x == (1/512)  = "512th"
    | x == (1/256)  = "256th"
    | x == (1/128)  = "128th"
    | x == (1/64)   = "64th"
    | x == (1/32)   = "32nd"
    | x == (1/16)   = "16th"
    | x == (1/8)    = "eighth"
    | x == (1/4)    = "quarter"
    | x == (1/2)    = "half"
    | x == (1/1)    = "whole"
    | x == (2/1)    = "breve"
    | x == (4/1)    = "long"
    | x == (8/1)    = "maxima"
    | otherwise     = error $ "Invalid note value:" ++ show x



instance WriteMusicXml FullNote where
    write (Pitched isChord
        (steps, alter, octaves))      = mempty
                                        <> singleIf isChord (unode "chord" ())
                                        <> single (unode "pitch" (mempty
                                            <> single   ((unode "step" . show) steps)
                                            <> maybeOne (unode "alter" . show . getSemitones) alter
                                            <> single   ((unode "octave" . show . getOctaves) octaves)))
    write (Unpitched isChord
        Nothing)                      = mempty
                                        <> singleIf isChord (unode "chord" ())
                                        <> single (unode "unpitched" ())
    write (Unpitched isChord
        (Just (steps, octaves)))      = mempty
                                        <> singleIf isChord (unode "chord" ())
                                        <> single (unode "unpitched" (mempty
                                            <> single ((unode "display-step" . show) steps)
                                            <> single ((unode "display-octave" . show . getOctaves) octaves)))
    write (Rest isChord
        Nothing)                      = mempty
                                        <> singleIf isChord (unode "chord" ())
                                        <> single (unode "rest" ())
    write (Rest isChord
        (Just (steps, octaves)))      = mempty
                                        <> singleIf isChord (unode "chord" ())
                                        <> single (unode "rest" (mempty
                                            <> single ((unode "display-step" . show) steps)
                                            <> single ((unode "display-octave" . show . getOctaves) octaves)))

writeDuration :: Duration -> [Element]
writeDuration = single . unode "duration" . show . getDivs

-- TODO
writeTie :: Tie -> [Element]
writeTie t = []

instance WriteMusicXml Note where
    write (Note full
                dur
                ties
                props) = write full <> writeDuration dur
                                    <> concatMap writeTie ties
                                    <> write props

    write (CueNote full
                   dur
                   props) = [unode "cue" ()] <> write full
                                             <> writeDuration dur
                                             <> write props

    write (GraceNote full
                     ties
                     props) = [unode "grace" ()] <> write full
                                                 <> concatMap writeTie ties
                                                 <> write props

-- --------------------------------------------------------------------------------
-- Notations
-- --------------------------------------------------------------------------------

-- TODO
data Notation
     = NotationTied             -- Tied
     | NotationSlur             -- Slur
     | NotationTuplet           -- Tuplet
     | NotationGlissando        -- Glissando
     | NotationSlide            -- Slide
     | NotationOrnaments        -- Ornaments
     | NotationTechnical        -- Technical
     | NotationArticulations    -- Articulations
     | NotationDynamics         -- Dynamics
     | NotationFermata          -- Fermata
     | NotationArpeggiate       -- Arpeggiate
     | NotationNonArpeggiate    -- NonArpeggiate
     | NotationAccidentalMark   -- AccidentalMark
     | NotationOther            -- OtherNotation

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
    | Rehearsal String
    -- pedals
    -- dashes, cesuras
    -- metronome
    -- 8va

instance WriteMusicXml Direction where
    write = notImplemented "WriteMusicXml instance"

-- --------------------------------------------------------------------------------
-- Basic types
-- --------------------------------------------------------------------------------

-- TODO move to separate module?
data BeamType = Begin | Continue | End | ForwardHook | BackwardHook

instance Show BeamType where
    show Begin          = "begin"
    show Continue       = "continue"
    show End            = "end"
    show ForwardHook    = "forward-hook"
    show BackwardHook   = "backward-hook"

newtype BeamLevel = BeamLevel { getBeamLevel :: Max8 }

deriving instance Eq            BeamLevel
deriving instance Show          BeamLevel
deriving instance Num           BeamLevel



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
toXml = fromSingle . write


-- --------------------------------------------------------------------------------


-- XML.Light aliases
addAttr  :: Attr -> Element -> Element
addAttrs :: [Attr] -> Element -> Element
addAttr  = add_attr
addAttrs = add_attrs

uattr :: String -> String -> Attr
uattr n = Attr (unqual n)



-- Some bounded int synonyms

type Max8 = Index N8



-- Misc

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

one :: (a -> b) -> a -> [b]
one f = single . f

maybeOne :: (a -> b) -> Maybe a -> [b]
maybeOne f = maybeToList . fmap f

single :: a -> [a]
single = return

fromSingle :: [a] -> a
fromSingle [x] = x
fromSingle _   = error "fromSingle: non-single list"

singleIf :: Bool -> a -> [a]
singleIf p x | not p     = []
             | otherwise = [x]


notImplemented x = error $ "Not implemented: " ++ x

