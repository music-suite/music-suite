
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
        ClefSign(..),

        -- ** Notes
        Note(..),
        FullNote(..),
        IsChord,
        noChord,
        Tie(..),
        noTies,
        NoteProps(..),

        -- TODO rewrite these
        mapNoteProps,
        mapNoteProps2,

        -- ** Notations
        Notation(..),

        -- ** Directions
        Direction(..),

        -- ** Lyrics
        Lyric(..),




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

        Mode(..),
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
        Dynamics,


        -- ** Misc
        Stem(..),
        NoteHead(..),

        DashLevel,
        BeamLevel,
        SlurLevel(..),
        TupletLevel(..),

        BeamType(..),
        StartStopContinue(..),


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


-- --------------------------------------------------------------------------------
-- Music
-- --------------------------------------------------------------------------------

type Music = [MusicElem]

data MusicElem
    = MusicAttributes   Attributes
    | MusicBackup       -- TODO
    | MusicForward      -- TODO
    | MusicNote         Note
    | MusicDirection    Direction
    | MusicHarmony      -- TODO
    | MusicFiguredBass  -- TODO
    | MusicPrint        -- TODO
    | MusicSound        -- TODO
    | MusicBarline      -- TODO
    | MusicGrouping     -- TODO
    | MusicLink         -- TODO
    | MusicBookmark     -- TODO


instance WriteMusicXml MusicElem where
    write (MusicAttributes x) = single $ unode "attributes" $ write x
    write (MusicNote x)       = single $ unode "note"       $ write x
    write (MusicDirection x)  = single $ unode "direction"  $ () --write x


-- --------------------------------------------------------------------------------
-- Attributes
-- --------------------------------------------------------------------------------

data Attributes
    = Divisions         Divs
    | Key               Fifths Mode
    | Time              TimeSignature
    | Staves            -- TODO
    | PartSymbol        -- TODO
    | Instruments       -- TODO
    | Clef              ClefSign Line
    | StaffDetails      -- TODO
    | Transpose         -- TODO
    | Directive         -- TODO
    | MeasureStyle      -- TODO

data TimeSignature
    = CommonTime
    | CutTime
    | DivTime
        Beat
        BeatType

data ClefSign = GClef | CClef | FClef | PercClef | TabClef
    deriving (Eq, Ord, Enum, Bounded)



instance WriteMusicXml Attributes where
    write (Divisions divs)                  = single $ unode "divisions"
                                                   $ show $ getDivs divs

    write (Clef sign line)                  = single $ unode "clef"
                                                        [ unode "sign" (writeClef sign),
                                                          unode "line" (show $ getLine line)]

    write (Key fifths mode)                 = single $ unode "key"
                                                        [ unode "fifths" (show $ getFifths fifths),
                                                          unode "mode" (writeMode mode)]

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

writeClef :: ClefSign -> String
writeClef GClef    = "G"
writeClef CClef    = "C"
writeClef FClef    = "F"
writeClef PercClef = "percussion"
writeClef TabClef  = "tab"

writeMode :: Mode -> String
writeMode = toLowerString . show


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

noChord :: IsChord
noChord = False

data Tie
    = TieStart
    | TieStop

noTies :: [Tie]
noTies = []

data NoteProps
    = NoteProps {
        noteInstrument   :: Maybe String,                       -- instrument
        noteVoice        :: Maybe Natural,                      -- voice
        noteType         :: Maybe NoteType,                     -- type
        noteDots         :: Natural,                            -- dots
        noteAccidental   :: Maybe (Accidental, Bool, Bool),     -- accidental, cautionary, editorial
        noteTimeMod      :: Maybe (Natural, Natural),           -- actual, normal
        noteStem         :: Maybe Stem,                         -- stem
        noteNoteHead     :: Maybe (NoteHead, Bool, Bool),       -- notehead, filled, parentheses
        noteNoteHeadText :: Maybe String,                       -- notehead-text
        noteStaff        :: Maybe Natural,                      -- staff
        noteBeam         :: Maybe (BeamLevel, BeamType),        -- beam-level, beam-type
        noteNotations    :: [Notation],                         -- notation
        noteLyrics       :: [Lyric]                             -- lyric
    }

mapNoteProps :: (NoteProps -> NoteProps) -> Note -> Note
mapNoteProps f (Note x d t p)     = Note x d t (f p)
mapNoteProps f (CueNote x d p)    = CueNote x d (f p)
mapNoteProps f (GraceNote x t p)  = GraceNote x t (f p)

mapNoteProps2 :: (NoteProps -> NoteProps) -> MusicElem -> MusicElem
mapNoteProps2 f (MusicNote n) = MusicNote (mapNoteProps f n)
mapNoteProps2 f x             = x







instance WriteMusicXml NoteProps where
    write (NoteProps
            instrument      -- TODO
            voice
            typ
            dots
            accidetnal      -- TODO
            timeMod         -- TODO
            stem            -- TODO
            noteHead        -- TODO
            noteHeadText    -- TODO
            staff           -- TODO
            beam
            notations
            lyrics)         -- TODO
                = mempty <> maybeOne (\(noteVal, noteSize) -> unode "type" (writeNoteVal noteVal)) typ
                         <> replicate (fromIntegral dots) (unode "dot" ())
                         <> maybeOne (\n -> unode "voice" $ show n) voice
                         <> maybeOne (\(n, typ) -> addAttr (uattr "number" $ show $ getBeamLevel n)
                                            $ unode "beam" $ show typ) beam

writeNoteVal :: NoteVal -> String
writeNoteVal (NoteVal x)
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
writeDuration :: Duration -> [Element]
writeDuration = single . unode "duration" . show . getDivs

-- TODO
writeTie :: Tie -> [Element]
writeTie t = []


-- --------------------------------------------------------------------------------
-- Notations
-- --------------------------------------------------------------------------------

-- TODO
data Notation
     = Tied                             StartStopContinue                   -- type
     | Slur                             SlurLevel StartStopContinue         -- level type
     | Tuplet                           TupletLevel StartStopContinue Bool  -- level type bracket
     | Glissando                        -- TODO line type: solid/dotted/dashed, number, start/stop, text?
     | Slide                            -- TODO line type: solid/dotted/dashed, number, start/stop, text?
     | Ornaments                        -- TODO TODO
     | Technical                        -- TODO TODO
     | Articulations                    -- TODO TODO
     | DynamicsN                        Dynamics
     | Fermata                          -- TODO ferm-type sign
     | Arpeggiate                       -- TODO bottom/top?
     | NonArpeggiate                    -- TODO bottom/top?
     | AccidentalMark                   Accidental
     | OtherNotation                    String



-- --------------------------------------------------------------------------------
-- Directions
-- --------------------------------------------------------------------------------

data Direction
    = Rehearsal                         String
    | Segno
    | Words                             String
    | Coda
    | Crescendo                         Bool -- start/stop
    | Diminuendo                        Bool -- start/stop
    | Dynamics                          Dynamics
    | Dashes                            DashLevel Bool -- level start/stop
    | Bracket                           -- TODO TODO
    | Pedal                             Bool -- start/change/stop
    | Metronome                         -- TODO unit bpm
    | OctaveShift                       -- TODO size: 8/15, up/down/stop
    | HarpPedals                        -- TODO TODO
    | Damp                              -- TODO TODO
    | DampAll                           -- TODO TODO
    | EyeGlasses                        -- TODO TODO
    | StringMute                        -- TODO TODO
    | Scordatura                        -- TODO TODO
    | Image                             -- TODO TODO
    | PrincipalVoice                    -- TODO TODO
    | AccordionRegistration             -- TODO TODO
    | Percussion                        -- TODO TODO
    | OtherDirection                    String

instance WriteMusicXml Direction where
    write = notImplemented "WriteMusicXml instance"


-- --------------------------------------------------------------------------------
-- Lyrics
-- --------------------------------------------------------------------------------

data Lyric = Lyric -- TODO

-- --------------------------------------------------------------------------------
-- Basic types
-- --------------------------------------------------------------------------------

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

newtype DashLevel   = DashLevel { getDashLevel :: Max8 }
newtype BeamLevel   = BeamLevel { getBeamLevel :: Max8 }
newtype SlurLevel   = SlurLevel { getSlurLevel :: Max8 }
newtype TupletLevel = TupletLevel { getTupletLevel :: Max8 }

data BeamType
    = BeginBeam
    | ContinueBeam
    | EndBeam
    | ForwardHookBeam
    | BackwardHookBeam

data StartStopContinue
    = Start
    | Stop
    | Continue

data Stem
    = StemDown | StemUp | StemNone | StemDouble

data NoteHead
    = NoteHeadSlash | NoteHeadTriangle | NoteHeadDiamond | NoteHeadSquare | NoteHeadCross | NoteHeadX
    | NoteHeadCircleX | NoteHeadInvertedTriangle | NoteHeadArrowDown | NoteHeadArrowUp | NoteHeadSlashed
    | NoteHeadBackSlashed | NoteHeadNormal | NoteHeadCluster | NoteHeadCircleDot | NoteHeadLeftTriangle
    | NoteHeadRectangle | NoteHeadNone


instance Show BeamType where
    show BeginBeam          = "begin"
    show ContinueBeam       = "continue"
    show EndBeam            = "end"
    show ForwardHookBeam    = "forward-hook"
    show BackwardHookBeam   = "backward-hook"

instance Show StartStopContinue where
    show Start                  = "start"
    show Stop                   = "stop"
    show Continue               = "continue"

deriving instance Eq            BeamLevel
deriving instance Show          BeamLevel
deriving instance Num           BeamLevel

deriving instance Eq            TupletLevel
deriving instance Show          TupletLevel
deriving instance Num           TupletLevel

deriving instance Eq            SlurLevel
deriving instance Show          SlurLevel
deriving instance Num           SlurLevel


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

toUpperChar :: Char -> Char
toUpperChar = Char.toUpper

toLowerChar :: Char -> Char
toLowerChar = Char.toLower

toUpperString :: String -> String
toUpperString = fmap Char.toUpper

toLowerString :: String -> String
toLowerString = fmap Char.toLower

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

