{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

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
import Control.Category ((>>>))
import Control.Monad
import Data.Char (toUpper)
import Data.Default
import Data.List.Split (wordsBy)
import Data.Maybe
import Data.Music.MusicXml.Score
import Text.Read
import Text.XML.Light

parseScore :: Element -> Maybe Score
parseScore top = case getName top of
  "score-partwise" -> do
    let scoreAttrs =
          fromMaybe [] $
            attr "version" top >>= traverse readMaybe . wordsBy (== '.')
    scoreHeader <- parseHeader top
    Partwise (ScoreAttrs scoreAttrs) scoreHeader
      <$> traverse parsePart (children "part" top)
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
      Identification <$> traverse parseCreator (children "creator" clist)
    parseCreator c = attr "type" c >>= \case
      "composer" -> Composer <$> getText c
      "lyricist" -> Lyricist <$> getText c
      "arranger" -> Arranger <$> getText c
      other -> OtherCreator other <$> getText c
    partList = do
      plist <- child "part-list" top
      PartList
        <$> dispatch [("part-group", parsePartGroup), ("score-part", parseScorePart)] plist

parseScorePart :: Element -> Maybe PartListElem
parseScorePart scorePart = do
  partId <- attr "id" scorePart
  name <- child "part-name" scorePart >>= getText
  let abbrev = child "part-abbreviation" scorePart >>= getText
  let nameDisplay = child "part-name-display" scorePart >>= getText
  let abbrevDisplay = child "part-abbreviation-display" scorePart >>= getText
  pure $ Part partId name abbrev nameDisplay abbrevDisplay

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
    <*> traverse parseMeasure (children "measure" part)

parseMeasure :: Element -> Maybe (MeasureAttrs, Music)
parseMeasure measure =
  (,)
    <$> (MeasureAttrs <$> (attr "number" measure >>= readMaybe))
    <*> (Music <$> dispatch supportedElems measure)

supportedElems :: [(String, Element -> Maybe MusicElem)]
supportedElems =
  [ ("attributes", fmap MusicAttributes . dispatch supportedAttributes),
    ("backup", fmap MusicBackup . parseDuration),
    ("forward", fmap MusicForward . parseDuration),
    ("note", fmap MusicNote . parseNote),
    ("direction", fmap MusicDirection . parseDirection),
    ("barline", fmap MusicBarline . parseBarline)
  ]

supportedAttributes :: [(String, Element -> Maybe Attributes)]
supportedAttributes =
  [ ("divisions", parseDivisions),
    ("key", parseKey),
    ("time", parseTime),
    ("instruments", readText >=> pure . Instruments),
    ("clef", parseClef),
    ("transpose", parseTranspose)
  ]

parseDivisions :: Element -> Maybe Attributes
parseDivisions = getText >=> readMaybe >=> pure . (Divisions . Divs)

parseKey :: Element -> Maybe Attributes
parseKey key =
  let fifths = child "fifths" key >>= readText
      mode = case child "mode" key of
        Nothing -> Just NoMode
        Just m -> getText m >>= \case
          "nomode" -> Just NoMode
          (c : cs) -> readMaybe (toUpper c : cs)
          _ -> Nothing
   in Key <$> (Fifths <$> fifths) <*> mode

parseTime :: Element -> Maybe Attributes
parseTime time = do
  let symbol = attr "symbol" time
  numerator <- child "beats" time >>= readText
  denomenator <- child "beat-type" time >>= readText
  pure $ case (symbol, numerator, denomenator) of
    (Just "common", 4, 4) -> Time CommonTime
    (Just "cut", 2, 2) -> Time CutTime
    _ -> Time $ DivTime (Beat numerator) (BeatType denomenator)

parseClef :: Element -> Maybe Attributes
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

parseTranspose :: Element -> Maybe Attributes
parseTranspose t =
  Transpose
    <$> (child "diatonic" t >>= readText)
    <*> (child "chromatic" t >>= readText)
    <*> (pure $ child "octave-change" t >>= readText >>= pure . OctaveChange)

parseDuration :: Element -> Maybe Duration
parseDuration = child "duration" >=> readText >=> pure . Divs

parseNote :: Element -> Maybe Note
parseNote note = do
  fullNote <- parseFullNote note
  let ties = fromMaybe [] $ traverse parseStartStop (children "tie" note)
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
          <*> (child "display-octave" restOrUnpitched >>= readText >>= pure . Octaves)

parsePitch :: Element -> Maybe Pitch
parsePitch note = do
  pitch <- child "pitch" note
  (,,) <$> (child "step" pitch >>= readText)
    <*> (pure . fmap Semitones $ child "alter" pitch >>= readText)
    <*> (fmap Octaves $ child "octave" pitch >>= readText)

isChord :: Element -> IsChord
isChord note = isJust $ child "chord" note

parseNoteProps :: Element -> Maybe NoteProps
parseNoteProps note = do
  let noteInstrument = child "instrument" note >>= attr "id"
      noteVoice = child "voice" note >>= readText
      noteType = child "type" note >>= parseNoteType
      noteDots = fromIntegral $ length $ children "dot" note
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
      noteNotations =
        fromMaybe [] $
          child "notations" note >>= dispatch supportedNotations
      noteLyrics = []
  pure $ NoteProps {..}

supportedNotations :: [(String, Element -> Maybe Notation)]
supportedNotations =
  [ ("tied", fmap Tied . parseStartStopContinue),
    ("slur", \slur -> Slur (parseLevel slur) <$> parseStartStopContinue slur),
    ("tuplet", \tuplet -> Tuplet (parseLevel tuplet) <$> parseStartStop tuplet),
    ("glissando", shift Glissando),
    ("slide", shift Slide),
    -- Ornaments TODO
    ("ornaments", const (pure $ Ornaments [])),
    ("technical", fmap Technical . dispatch supportedTechniques),
    ("articulations", fmap Articulations . dispatch supportedArticulations),
    ("dynamics", fmap DynamicNotation . parseDynamics),
    ( "fermata",
      fmap Fermata
        . ( getText >>> \case
              Just "normal" -> pure NormalFermata
              Just "angled" -> pure AngledFermata
              Just "square" -> pure SquaredFermata
              Just _ -> empty
              Nothing -> pure NormalFermata
          )
    ),
    ("non-arpeggiate", const (pure NonArpeggiate)),
    ("arpeggiate", const (pure Arpeggiate)),
    ("accidental-mark", fmap AccidentalMark . parseAccidental),
    ("other-notation", fmap OtherNotation . getText)
  ]
  where
    shift constructor slideType =
      constructor (parseLevel slideType)
        <$> parseStartStopContinue slideType
        <*> parseLineType slideType
        <*> pure (getText slideType)
    parseLineType = child "line-type" >=> getText >=> \case
      "solid" -> pure Solid
      "dashed" -> pure Dashed
      "dotted" -> pure Dotted
      "wavy" -> pure Wavy
      _ -> empty

parseDynamics :: Element -> Maybe Dynamics
parseDynamics = elChildren >>> headMay >=> getName >>> map toUpper >>> readMaybe

supportedTechniques :: [(String, Element -> Maybe Technical)]
supportedTechniques =
  [ ("up-bow", bare UpBow),
    ("down-bow", bare DownBow),
    ("harmonic", bare Harmonic),
    ("open-string", bare OpenString),
    ("thumb-position", bare ThumbPosition),
    ("fingering", fmap Fingering . readText),
    ("pluck", bare Pluck),
    ("double-tongue", bare DoubleTongue),
    ("triple-tongue", bare TripleTongue),
    ("stopped", bare Stopped),
    ("snap-pizzicato", bare SnapPizzicato),
    ("fret", fmap Fret . readText),
    ("string", fmap Data.Music.MusicXml.Score.String . readText),
    ("hammer-on", bare HammerOn),
    ("pull-off", bare PullOff),
    ("bend", bare Bend),
    ("tap", bare Tap),
    ("heel", bare Heel),
    ("toe", bare Toe),
    ("fingernails", bare Fingernails),
    ("hole", bare Hole),
    ("arrow", bare Arrow),
    ("handbell", bare Handbell),
    ("other-technical", fmap OtherTechnical . getText)
  ]
  where bare = const . pure

supportedArticulations :: [(String, Element -> Maybe Articulation)]
supportedArticulations = map (\a -> (show a, const (pure a)))
  [Accent .. OtherArticulation]


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

parseLevel :: Element -> Level
parseLevel e = maybe def mkLevel $ attr "number" e >>= readMaybe

supportedDirections :: [(String, Element -> Maybe Direction)]
supportedDirections =
  [ ("rehearsal", fmap Rehearsal . getText),
    ("segno", const (pure Segno)),
    ("words", fmap Words . getText),
    ("coda", const (pure Coda)),
    ("dynamics", fmap Dynamics . parseDynamics),
    ("dashes", \dashes -> Dashes (parseLevel dashes) <$> (parseStartStop dashes)),
    ( "pedal",
      fmap Pedal
        . ( attr "type" >=> \case
              "start" -> pure Start
              "stop" -> pure Stop
              "change" -> pure Change
              "continue" -> pure Continue
              _ -> empty
          )
    ),
    ("other-direction", fmap OtherDirection . getText)
    -- TODO metronome, crescendo, diminuendo
  ]

parseDirection :: Element -> Maybe Direction
parseDirection = child "direction-type" >=> dispatch supportedDirections >=> headMay

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
  let barRepeat = child "repeat" barline >>= attr "direction" >>= \case
        "backward" -> pure (Repeat RepeatBackward)
        "forward" -> pure (Repeat RepeatForward)
        _ -> empty
  pure $ Barline loc style barRepeat

parseStartStop :: Element -> Maybe StartStop
parseStartStop = attr "type" >=> \case
  "start" -> pure Start
  "stop" -> pure Stop
  _ -> empty

parseStartStopContinue :: Element -> Maybe StartStopContinue
parseStartStopContinue = attr "type" >=> \case
  "start" -> pure Start
  "stop" -> pure Stop
  "continue" -> pure Continue
  _ -> empty

-- For elements with many children corresponding to a sum type. This parser
-- is much more permissive than the other parsers in that it will simply
-- ignore invalid elements instead of failing the whole thing
dispatch :: [(String, Element -> Maybe a)] -> Element -> Maybe [a]
dispatch parsers parent =
  pure $
    mapMaybe
      (\e -> fromMaybe (const Nothing) (lookup (getName e) parsers) $ e)
      (elChildren parent)

-- ----------------------------------------------------------------------------------
-- Xml utils
-- ----------------------------------------------------------------------------------
headMay :: [a] -> Maybe a
headMay = \case
  [] -> Nothing
  x : _ -> Just x

readText :: Read a => Element -> Maybe a
readText = getText >=> readMaybe

getText :: Element -> Maybe String
getText element = case elContent element of
  [Text cdata] -> pure $ cdData cdata
  _ -> Nothing

getName :: Element -> String
getName = qName . elName

child :: String -> Element -> Maybe Element
child str = filterChildName (\q -> qName q == str)

children :: String -> Element -> [Element]
children str = filterChildrenName (\q -> qName q == str)

attr :: String -> Element -> Maybe String
attr str = findAttrBy (\q -> qName q == str)
