{-# LANGUAGE LambdaCase #-}

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
  abbrev <- pure $ child "part-abbreviation" scorePart >>= getText
  nameDisplay <- pure $ child "part-name-display" scorePart >>= getText
  abbrevDisplay <- pure $ child "part-abbreviation-display" scorePart >>= getText
  pure $ Part id name abbrev nameDisplay abbrevDisplay

parsePartGroup :: Element -> Maybe PartListElem
parsePartGroup partGroup = do
  number <- attr "number" partGroup >>= readMaybe
  startStop <- parseStartStop partGroup
  name <- pure $ child "group-name" partGroup >>= getText
  abbrev <- pure $ child "group-abbreviation" partGroup >>= getText
  symbol <- pure $
    child "group-symbol" partGroup >>= getText >>= \case
      "brace" -> pure GroupBrace
      "line" -> pure GroupLine
      "bracket" -> pure GroupBracket
      "square" -> pure GroupSquare
      "none" -> pure NoGroupSymbol
      _ -> empty
  barline <- pure $
    child "group-barline" partGroup >>= getText >>= \case
      "yes" -> pure GroupBarLines
      "no" -> pure GroupNoBarLines
      "Mensurstrich" -> pure GroupMensurstrich
      _ -> empty
  pure $ Group (Level (coerceToIndex number)) startStop name abbrev symbol barline True

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
          symbol <- pure $ attr "symbol" time
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
          octaveChange <- pure $ child "clef-octave-change" clef >>= readText
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
  ((GraceNote fullNote ties def) <$ child "grace" note)
    <|> ( do
            duration <- parseDuration note
            ((CueNote fullNote duration def) <$ child "cue" note)
              <|> (pure (Note fullNote duration ties def))
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
        child "dashes" >=> \dashes -> do
          level <- pure $ attr "number" dashes >>= readMaybe
          startStop <- parseStartStop dashes
          pure $ Dashes (Level (coerceToIndex (fromMaybe 1 level))) startStop,
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
  repeat <- pure $
    child "repeat" barline >>= attr "direction" >>= \case
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
