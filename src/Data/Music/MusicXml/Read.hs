{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright   : (c) Joseph Morag 2020
--
-- License     : BSD-style
--
-- Maintainer  : jm@josephmorag.com
-- Stability   : experimental
-- Portability : portable
module Data.Music.MusicXml.Read
  ( parseScore,
  )
where

import Control.Applicative
import Control.Monad
import Data.Char (toUpper)
import Data.Default
import Data.List.Split (wordsBy)
import Data.Maybe
import Data.Music.MusicXml.Score
import Text.Read
import Text.XML.Light
import TypeUnary.Nat

parseScore :: Element -> Maybe Score
parseScore top = case getName top of
  "score-partwise" -> do
    let scoreAttrs =
          fromMaybe [] $
            attr "version" top >>= traverse readMaybe . wordsBy (== '.')
    scoreHeader <- parseHeader top
    Partwise (ScoreAttrs scoreAttrs) scoreHeader
      <$> traverse parsePart (children ["part"] top)
  "score-timewise" -> error "timewise not supported"
  _ -> empty

parseHeader :: Element -> Maybe ScoreHeader
parseHeader top =
  pure $
    ScoreHeader
      title
      movementNumber
      movementTitle
      identification
      (fromMaybe (PartList []) partList)
  where
    title = child "work" top >>= child "work-title" >>= getText
    movementNumber = child "movement-number" top >>= readText
    movementTitle = child "movement-title" top >>= getText
    identification = do
      clist <- child "identification" top
      Identification <$> traverse parseCreator (children ["creator"] clist)
    parseCreator c = Creator <$> attr "type" c <*> getText c
    partList = do
      plist <- child "part-list" top
      PartList
        <$> traverse
          (\p -> parsePartGroup p <|> parseScorePart p)
          (children ["part-group", "score-part"] plist)

parseScorePart :: Element -> Maybe PartListElem
parseScorePart scorePart = do
  id <- attr "id" scorePart
  name <- child "part-name" scorePart >>= getText
  let abbrev = child "part-abbreviation" scorePart >>= getText
  let nameDisplay = child "part-name-display" scorePart >>= getText
  let abbrevDisplay = child "part-abbreviation-display" scorePart >>= getText
  pure $ Part id name abbrev nameDisplay abbrevDisplay

parsePartGroup :: Element -> Maybe PartListElem
parsePartGroup partGroup = do
  let level = parseLevel partGroup
  startStop <- parseStartStop partGroup
  let name = child "group-name" partGroup >>= getText
      abbrev = child "group-abbreviation" partGroup >>= getText
      symbol = child "group-symbol" partGroup >>= getText >>= \case
        "brace" -> pure GroupBrace
        "line" -> pure GroupLine
        "bracket" -> pure GroupBracket
        "square" -> pure GroupSquare
        "none" -> pure NoGroupSymbol
        _ -> empty
      barline = child "group-barline" partGroup >>= getText >>= \case
        "yes" -> pure GroupBarLines
        "no" -> pure GroupNoBarLines
        "Mensurstrich" -> pure GroupMensurstrich
        _ -> empty
  pure $ Group level startStop name abbrev symbol barline True

parsePart :: Element -> Maybe (PartAttrs, [(MeasureAttrs, Music)])
parsePart part =
  (,)
    <$> (PartAttrs <$> attr "id" part)
    <*> traverse parseMeasure (children ["measure"] part)

parseMeasure :: Element -> Maybe (MeasureAttrs, Music)
parseMeasure measure =
  (,)
    <$> (MeasureAttrs <$> (attr "number" measure >>= readMaybe))
    <*> (Music <$> traverse parseMusicElem (children supportedElems measure))

supportedElems :: [String]
supportedElems =
  [ "attributes",
    "backup",
    "forward",
    "note",
    "direction",
    "barline"
  ]

elemParsers :: [Element -> Maybe MusicElem]
elemParsers =
  [ fmap MusicAttributes . parseAttributes,
    fmap MusicBackup . parseDuration,
    fmap MusicForward . parseDuration,
    fmap MusicNote . parseNote,
    fmap MusicDirection . parseDirection,
    fmap MusicBarline . parseBarline
  ]

parseAttributes :: Element -> Maybe [Attributes]
parseAttributes =
  traverse parseAttr
    . children
      ["divisions", "key", "time", "instruments", "clef", "transpose"]
  where
    parseAttr =
      tryAll
        [ parseDivisions,
          parseKey,
          parseTime,
          child "instruments" >=> readText >=> fmap Instruments,
          parseClef,
          parseTranspose
        ]
      where
        parseDivisions = getText >=> readMaybe >=> fmap (Divisions . Divs)
        parseKey key =
          let fifths = child "fifths" key >>= readText
              mode = case child "mode" key of
                Nothing -> Just NoMode
                Just m -> getText m >>= \case
                  "nomode" -> Just NoMode
                  (c : cs) -> readMaybe (toUpper c : cs)
                  _ -> Nothing
           in Key <$> (Fifths <$> fifths) <*> mode
        parseTime time = do
          let symbol = attr "symbol" time
          numerator <- child "beats" time >>= readText
          denomenator <- child "beat-type" time >>= readText
          pure $ case (symbol, numerator, denomenator) of
            (Just "common", 4, 4) -> Time CommonTime
            (Just "cut", 2, 2) -> Time CutTime
            _ -> Time $ DivTime (Beat numerator) (BeatType denomenator)
        parseClef clef = do
          sign <- child "sign" clef >>= getText >>= \case
            "G" -> pure GClef
            "C" -> pure CClef
            "F" -> pure FClef
            "percussion" -> pure PercClef
            "TAB" -> pure TabClef
            _ -> empty
          line <- case child "line" clef of
            -- Percussion clefs can have no line element in which case they appear
            -- in the middle of the staff
            Nothing -> pure 3
            Just l -> getText l >>= readMaybe
          let octaveChange = child "clef-octave-change" clef >>= readText
          if line <= 5 && line >= 1
            then pure $ Clef sign (Line line) (OctaveChange <$> octaveChange)
            else Nothing
        parseTranspose t =
          Transpose
            <$> (child "diatonic" t >>= readText)
            <*> (child "chromatic" t >>= readText)
            <*> (pure $ child "octave-change" t >>= readText >>= fmap OctaveChange)

parseDuration :: Element -> Maybe Duration
parseDuration = child "duration" >=> readText >=> fmap Divs

parseNote :: Element -> Maybe Note
parseNote note = do
  fullNote <- parseFullNote note
  let ties = fromMaybe [] $ traverse parseStartStop (children ["tie"] note)
  noteProps <- parseNoteProps note
  ((GraceNote fullNote ties noteProps) <$ child "grace" note)
    <|> ( do
            duration <- parseDuration note
            ((CueNote fullNote duration noteProps) <$ child "cue" note)
              <|> (pure (Note fullNote duration ties noteProps))
        )

parseFullNote :: Element -> Maybe FullNote
parseFullNote note =
  Pitched (isChord note) <$> parsePitch note
    <|> Rest (isChord note) <$> (child "rest" note >>= parseDisplayPitch)
    <|> Unpitched (isChord note) <$> (child "unpitched" note >>= parseDisplayPitch)
  where
    parseDisplayPitch restOrUnpitched =
      pure $
        (,) <$> (child "display-step" restOrUnpitched >>= readText)
          <*> (child "display-octave" restOrUnpitched >>= readText >>= fmap Octaves)

parsePitch :: Element -> Maybe Pitch
parsePitch note = do
  pitch <- child "pitch" note
  (,,) <$> (child "step" pitch >>= readText)
    <*> (pure $ child "alter" pitch >>= readText >>= fmap Semitones)
    <*> (child "octave" pitch >>= readText >>= fmap Octaves)

isChord :: Element -> IsChord
isChord note = isJust $ child "chord" note

parseNoteProps :: Element -> Maybe NoteProps
parseNoteProps note = do
  let noteInstrument = child "instrument" note >>= attr "id"
      noteVoice = child "voice" note >>= readText
      noteType = child "type" note >>= parseNoteType
      noteDots = fromIntegral $ length $ children ["dot"] note
      noteAccidental = do
        accidentalElem <- child "accidental" note
        a <- parseAccidental accidentalElem
        let cautionary = attr "cautionary" accidentalElem == Just "yes"
            editorial = attr "editorial" accidentalElem == Just "yes"
        pure (a, cautionary, editorial)
      noteTimeMod = child "time-modification" note >>= \t ->
        (,) <$> (child "actual-notes" t >>= readText)
          <*> (child "normal-notes" note >>= readText)
      noteStem = child "stem" note >>= getText >>= \case
        "up" -> pure StemUp
        "down" -> pure StemDown
        "none" -> pure StemNone
        "double" -> pure StemDouble
        _ -> empty
      noteNoteHead = do
        noteheadElem <- child "notehead" note
        n <- case getText noteheadElem of
          Just "slash" -> pure SlashNoteHead
          Just "triangle" -> pure TriangleNoteHead
          Just "diamond" -> pure DiamondNoteHead
          Just "square" -> pure SquareNoteHead
          Just "cross" -> pure CrossNoteHead
          Just "x" -> pure XNoteHead
          Just "circle-x" -> pure CircleXNoteHead
          Just "inverted triangle" -> pure InvertedTriangleNoteHead
          Just "arrow down" -> pure ArrowDownNoteHead
          Just "arrow up" -> pure ArrowUpNoteHead
          Just "slashed" -> pure SlashedNoteHead
          Just "back slashed" -> pure BackSlashedNoteHead
          Just "normal" -> pure NormalNoteHead
          Just "cluster" -> pure ClusterNoteHead
          Just "circle dot" -> pure CircleDotNoteHead
          Just "left triangle" -> pure LeftTriangleNoteHead
          Just "rectangle" -> pure RectangleNoteHead
          Just "none" -> pure NoNoteHead
          _ -> empty
        let filled = attr "filled" noteheadElem == Just "yes"
            parentheses = attr "parentheses" noteheadElem == Just "yes"
        pure (n, filled, parentheses)
      noteNoteHeadText = do
        n <- child "notehead-text" note
        displayText <- child "display-text" n >>= getText
        accidentalText <- child "accidental-text" n >>= parseAccidental
        pure (displayText, accidentalText)
      noteStaff = child "staff" note >>= readText
      noteBeam = do
        b <- child "beam" note
        let level = parseLevel b
        beamType <- case getText b of
          Just "begin" -> pure BeginBeam
          Just "continue" -> pure ContinueBeam
          Just "end" -> pure EndBeam
          Just "forward hook" -> pure ForwardHook
          Just "backward hook" -> pure BackwardHook
          _ -> empty
        pure (level, beamType)
      noteNotations = []
      noteLyrics = []
  pure $ NoteProps {..}

parseNoteType :: Element -> Maybe NoteType
parseNoteType typ = do
  noteval <- case getText typ of
    Just "1024th" -> pure (1 / 1024)
    Just "512th" -> pure (1 / 512)
    Just "256th" -> pure (1 / 256)
    Just "128th" -> pure (1 / 128)
    Just "64th" -> pure (1 / 64)
    Just "32nd" -> pure (1 / 32)
    Just "16th" -> pure (1 / 16)
    Just "eighth" -> pure (1 / 8)
    Just "quarter" -> pure (1 / 4)
    Just "half" -> pure (1 / 2)
    Just "whole" -> pure (1 / 1)
    Just "breve" -> pure (2 / 1)
    Just "long" -> pure (4 / 1)
    Just "maxima" -> pure (8 / 1)
    _ -> empty
  let notesize = case attr "size" typ of
        Just "full" -> pure SizeFull
        Just "cue" -> pure SizeCue
        Just "large" -> pure SizeLarge
        _ -> empty
  pure (noteval, notesize)

parseAccidental :: Element -> Maybe Accidental
parseAccidental = getText >=> \case
  "flat-flat" -> pure DoubleFlat
  "flat" -> pure Flat
  "natural" -> pure Natural
  "sharp" -> pure Sharp
  "double-sharp" -> pure Sharp
  -- TODO quarter tone accidentals
  _ -> empty

-- | Total, will default level to 1
-- Subtracts 1 from "number" attribute in order to make musicxml level
-- fully representable by Max8 type. See
-- http://usermanuals.musicxml.com/MusicXML/MusicXML.htm#ST-MusicXML-beam-level.htm
parseLevel :: Element -> Level
parseLevel e = Level . coerceToIndex @Int . pred $ case attr "number" e >>= readMaybe of
  Nothing -> 0
  Just n | n < 8 && n >= 0 -> n
  _ -> 0

parseDirection :: Element -> Maybe Direction
parseDirection =
  child "direction-type"
    >=> tryAll
      [ fmap Rehearsal . (child "rehearsal" >=> getText),
        fmap (const Segno) . (child "segno"),
        fmap Words . (child "words" >=> getText),
        fmap (const Coda) . (child "coda"),
        -- TODO crescendo/diminuendo
        fmap Dynamics . (child "dynamics" >=> (readMaybe . map toUpper . getName)),
        child "dashes" >=> \dashes ->
          Dashes (parseLevel dashes) <$> (parseStartStop dashes),
        fmap Pedal
          . ( child "pedal" >=> attr "type" >=> \case
                "start" -> pure Start
                "stop" -> pure Stop
                "change" -> pure Change
                "continue" -> pure Continue
                _ -> empty
            ),
        -- TODO metronome
        fmap OtherDirection . (child "other-direction" >=> getText)
      ]

parseBarline :: Element -> Maybe Barline
parseBarline barline = do
  loc <- case attr "location" barline of
    Nothing -> pure BLRight
    Just "right" -> pure BLRight
    Just "left" -> pure BLLeft
    Just "middle" -> pure BLMiddle
    _ -> empty
  style <- case child "bar-style" barline of
    Nothing -> pure BSRegular
    Just bstyle -> case getText bstyle of
      Just "regular" -> pure BSRegular
      Just "dotted" -> pure BSDotted
      Just "dashed" -> pure BSDashed
      Just "heavy" -> pure BSHeavy
      Just "light-light" -> pure BSLightLight
      Just "light-heavy" -> pure BSLightHeavy
      Just "heavy-light" -> pure BSHeavyLight
      Just "heavy-heavy" -> pure BSHeavyHeavy
      Just "tick" -> pure BSTick
      Just "short" -> pure BSShort
      Just "none" -> pure BSNone
      _ -> empty
  let repeat = child "repeat" barline >>= attr "direction" >>= \case
        "backward" -> pure (Repeat RepeatBackward)
        "forward" -> pure (Repeat RepeatForward)
        _ -> empty
  pure $ Barline loc style repeat

parseMusicElem :: Element -> Maybe MusicElem
parseMusicElem = tryAll elemParsers

parseStartStop :: Element -> Maybe StartStop
parseStartStop = attr "type" >=> \case
  "start" -> pure Start
  "stop" -> pure Stop
  _ -> empty

-- Run all parsers on a single element and return the successful parse
tryAll :: (Alternative f, Foldable t) => t (a -> f b) -> a -> f b
tryAll fs x = foldr (\fun acc -> acc <|> fun x) empty fs

-- ----------------------------------------------------------------------------------
-- Xml utils
-- ----------------------------------------------------------------------------------

readText :: Read a => Element -> Maybe a
readText = getText >=> readMaybe

getText :: Element -> Maybe String
getText element = case elContent element of
  [Text cdata] -> pure $ cdData cdata
  _ -> Nothing

getName :: Element -> String
getName = qName . elName

-- MusicXml names are always unqualified, as far as I can tell
child :: String -> Element -> Maybe Element
child str = filterChildName (\q -> qName q == str)

children :: [String] -> Element -> [Element]
children strs = filterChildrenName (\q -> qName q `elem` strs)

attr :: String -> Element -> Maybe String
attr str = findAttrBy (\q -> qName q == str)
