
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Main where

import Data.Aeson
import Control.Applicative
import Data.Aeson.Types(Parser)
import Music.Prelude.Basic

import Data.ByteString(ByteString)
import qualified Data.ByteString as B

fromJson :: ByteString -> Score Note
fromJson = const c

setTitle :: String -> Score a -> Score a
setTitle = setMeta "title"
setComposer :: String -> Score a -> Score a
setComposer = setMeta "composer"
setInformation :: String -> Score a -> Score a
setInformation = setMeta "information"
setMeta :: String -> String -> Score a -> Score a
setMeta _ _ = id


-- The whole score
{-
newtype WholeScore = JSCore (Score Note)
    deriving (Eq, Ord, Show)
newtype Staff      = Staff  (Score Note)
    deriving (Eq, Ord, Show)
newtype Bar        = Bar    (Score Note)
    deriving (Eq, Ord, Show)
newtype BarObject  = BarObject (Score Note)
    deriving (Eq, Ord, Show)
instance FromJSON BarObject where
    parseJSON = undefined
-}


data JSStaff = JSStaff {
            bars                :: [JSBar],
            name                :: String,
            shortName           :: String
    }
    deriving (Eq, Ord, Show)

data JSBar = JSBar {
            elements            :: [JSElement]
    }
    deriving (Eq, Ord, Show)

data JSElement 
    = JSElementText JSText
    | JSElementClef JSClef
    | JSElementSlur JSSlur
    | JSElementCrescendoLine JSCrescendoLine
    | JSElementDiminuendoLine JSDiminuendoLine
    | JSElementTimeSignature JSTimeSignature
    | JSElementKeySignature JSKeySignature
    | JSElementTuplet JSTuplet
    | JSElementChord JSChord
    deriving (Eq, Ord, Show)

data JSText = JSText {
            text_voice               :: Int,
            text_position            :: Int,
            text_text                :: String,
            text_style               :: Int
    }
    deriving (Eq, Ord, Show)
data JSClef = JSClef {
            clef_voice               :: Int,
            clef_position            :: Int,
            clef_style               :: Int
    }
    deriving (Eq, Ord, Show)
data JSSlur = JSSlur {
            slur_voice               :: Int,
            slur_position            :: Int,
            slur_duration            :: Int,
            slur_style               :: Int
    }
    deriving (Eq, Ord, Show)
data JSCrescendoLine = JSCrescendoLine {  
            cresc_voice               :: Int,
            cresc_position            :: Int,
            cresc_duration            :: Int,
            cresc_style               :: Int
    }
    deriving (Eq, Ord, Show)
data JSDiminuendoLine = JSDiminuendoLine {
            dim_voice               :: Int,
            dim_position            :: Int,
            dim_duration            :: Int,
            dim_style               :: Int
    }
    deriving (Eq, Ord, Show)
data JSTimeSignature = JSTimeSignature {
            time_voice               :: Int,
            time_position            :: Int,
            time_value               :: Rational,
            time_isCommon            :: Bool,
            time_isAllaBreve         :: Bool
    }
    deriving (Eq, Ord, Show)
data JSKeySignature = JSKeySignature {
            key_voice               :: Int,
            key_position            :: Int,
            key_major               :: Bool,
            key_sharps              :: Int,
            key_isOpen              :: Bool
    }
    deriving (Eq, Ord, Show)
data JSTuplet = JSTuplet {
            tuplet_voice               :: Int,
            tuplet_position            :: Int,
            tuplet_duration            :: Int,
            tuplet_playedDuration      :: Int,
            tuplet_value               :: Rational
    }
    deriving (Eq, Ord, Show)
data JSChord = JSChord { 
            chord_position            :: Int,
            chord_duration            :: Int,
            chord_voice               :: Int,
            chord_articulations       :: [()],
            chord_singleTremolos      :: Int,
            chord_doubleTremolos      :: Int,
            chord_acciaccatura        :: Bool,
            chord_appoggiatura        :: Bool,
            chord_notes               :: [()]
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSChord where
    parseJSON (Object v) = JSChord 
        <$> v .: "position" 
        <*> v .: "duration"
        <*> v .: "voice"
        <*> v .: "articulations"
        <*> v .: "singleTremolos"
        <*> v .: "doubleTremolos"
        <*> v .: "acciaccatura"
        <*> v .: "appoggiatura"
        <*> v .: "notes"

data JSNote = JSNote {
            note_pitch               :: Int,
            note_diatonicPitch       :: Int,
            note_accidental          :: Int,
            note_tied                :: Bool,
            note_style               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSNote where
    parseJSON (Object v) = JSNote 
        <$> v .: "pitch" 
        <*> v .: "diatonicPitch"
        <*> v .: "accidental"
        <*> v .: "tied"
        <*> v .: "style"


-- decode "{\"accidental\":0,\"diatonicPitch\":36,\"pitch\":62,\"style\":0,\"tied\":false}" :: Maybe JSNote



main = do
    json <- B.readFile "test.json"
    let score = fromJson json
    openLy score
    