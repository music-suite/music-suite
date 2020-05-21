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
    scoreAttrs <- ScoreAttrs <$> mapM (readMaybe . attrVal) (elAttribs top)
    scoreHeader <- parseHeader top
    pure $ Partwise scoreAttrs scoreHeader []

parseHeader :: Element -> Maybe ScoreHeader
parseHeader top =
  pure $ ScoreHeader title mvmNumber mvmTitle identification (fromMaybe (PartList []) partList)
  where
    title = child "work" top >>= child "work-title" >>= getText
    mvmNumber = child "movement-number" top >>= getText >>= readMaybe
    mvmTitle = child "movement-title" top >>= getText
    identification = case child "identification" top of
      Nothing -> Nothing
      Just i -> do
        let creators = children "creator" i
        cTypes <- traverse (attr "type") creators
        cNames <- traverse getText creators
        pure $ Identification (zipWith Creator cTypes cNames)
    partList = case child "part-list" top of
      Nothing -> Nothing
      Just parts -> PartList <$> traverse parsePart (children "score-part" parts)
    parsePart scorePart = do
      id <- attr "id" scorePart
      name <- child "part-name" scorePart >>= getText
      abbrev <- pure $ child "part-abbreviation" scorePart >>= getText
      nameDisplay <- pure $ child "part-name-display" scorePart >>= getText
      abbrevDisplay <- pure $ child "part-abbreviation-display" scorePart >>= getText
      pure $ Part id name abbrev nameDisplay abbrevDisplay

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
