
module Music.MusicXml.Write.Score

where

import Prelude hiding (getLine)

import Data.Maybe (maybeToList)
import Data.Semigroup
import Data.Default
import Numeric.Natural

import Text.XML.Light hiding (Line)

import Music.MusicXml.Score
import Music.MusicXml.Time
import Music.MusicXml.Pitch
import Music.MusicXml.Dynamics
import Music.MusicXml.Read
import Music.MusicXml.Write

import qualified Data.List as List
import qualified Data.Char as Char


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

instance WriteMusicXml MusicElem where
    write (MusicAttributes x) = single $ unode "attributes" $ write x
    write (MusicNote x)       = single $ unode "note"       $ write x
    write (MusicDirection x)  = single $ unode "direction"  $ () --write x

-- --------------------------------------------------------------------------------
-- Attributes
-- --------------------------------------------------------------------------------


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


-- --------------------------------------------------------------------------------
-- Directions
-- --------------------------------------------------------------------------------

instance WriteMusicXml Direction where
    write = notImplemented "WriteMusicXml instance"


-- --------------------------------------------------------------------------------
-- Lyrics
-- --------------------------------------------------------------------------------






-- --------------------------------------------------------------------------------


-- XML aliases

addAttr  :: Attr -> Element -> Element
addAttrs :: [Attr] -> Element -> Element
addAttr  = add_attr
addAttrs = add_attrs

uattr :: String -> String -> Attr
uattr n = Attr (unqual n)


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
