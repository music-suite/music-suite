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
  -- (
  -- )
where

import Text.Read
import Text.XML.Light
import Data.Char (isSpace)
import Data.Maybe
import Data.Music.MusicXml.Score
-- import Data.Music.MusicXml.Lens
-- import Control.Monad
-- import Data.List (zipWith5)

-- | Clean blank text elements from the output of @parseXML
cleanXML :: [Content] -> [Content]
cleanXML = mapMaybe $ \case
    t@(Text (CData CDataText s _)) -> if all isSpace s then Nothing else Just t
    (Elem e) -> Just $ Elem $ e {elContent = cleanXML (elContent e)}
    c -> Just c

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

getText :: Element -> Maybe String
getText element = case elContent element of
  [Text cdata] -> pure $ cdData cdata
  _ -> Nothing

getName :: Element -> String
getName = qName . elName

-- MusicXml names are always unqualified, as far as I can tell
child :: String -> Element -> Maybe Element
child str = filterChildName (\q -> qName q == str)

children :: String -> Element -> [Element]
children str = filterChildrenName (\q -> qName q == str)

attr :: String -> Element -> Maybe String
attr str = findAttrBy (\q -> qName q == str)

-- >>> xml <- parseXMLDoc <$>  readFile "/home/joseph/Documents/MuseScore3/Scores/Sibelius_violin_concerto_excerpt.musicxml"
-- >>> xml >>= parseHeader
-- Just (ScoreHeader {scoreTitle = Just "Excerpt from Violin Concerto", mvmNumber = Nothing, mvmTitle = Nothing, scoreIdentification = Just (Identification [Creator {creatorType = "composer", creatorName = "Jean Sibelius"}]), scorePartList = PartList {getPartList = [Part "P1" "Violin" (Just "Vln.") Nothing Nothing]}})
