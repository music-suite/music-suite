
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Main where

import Data.Aeson
import Data.Aeson.Types(parse)
import Control.Applicative
import Data.Aeson.Types(Parser)
import Music.Prelude.Basic

import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HashMap

setTitle :: String -> Score a -> Score a
setTitle = setMeta "title"
setComposer :: String -> Score a -> Score a
setComposer = setMeta "composer"
setInformation :: String -> Score a -> Score a
setInformation = setMeta "information"
setMeta :: String -> String -> Score a -> Score a
setMeta _ _ = id





data JSScore = JSScore {
            score_title             :: String,
            score_composer          :: String,
            score_information       :: String,
            score_staffHeight       :: Double,
            score_transposing       :: Bool,
            score_staves            :: [JSStaff],
            score_systemStaff       :: ()
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSScore where
    parseJSON (Object v) = JSScore
        <$> v .: "title" 
        <*> v .: "composer"
        <*> v .: "information"
        <*> v .: "staffHeight"
        <*> v .: "transposing"
        <*> v .: "staves"          
        -- TODO
        <*> (return ())


data JSStaff = JSStaff {
            staff_bars                :: [JSBar],
            staff_name                :: String,
            staff_shortName           :: String
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSStaff where
    parseJSON (Object v) = JSStaff
        <$> v .: "bars"
        <*> v .: "name"
        <*> v .: "shortName"

data JSBar = JSBar {
            bar_elements            :: [JSElement]
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSBar where
    parseJSON (Object v) = JSBar
        <$> v .: "elements"

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
instance FromJSON JSElement where
    parseJSON x@(Object v) = case HashMap.lookup "type" v of    
        -- TODO
        Just "text"      -> error "JsElementText"
        Just "clef"      -> error "JSElementClef"
        Just "slur"      -> error "JSElementSlur"
        Just "cresc"     -> error "JSElementCrescendoLine"
        Just "dim"       -> error "JSElementDiminuendoLine"
        Just "time"      -> error "JSElementTimeSignature"
        Just "key"       -> error "JSElementKeySignature"
        Just "tuplet"    -> error "JSElementTuplet"
        Just "chord"     -> JSElementChord <$> parseJSON x
        _                -> mempty

data JSText = JSText {
            text_voice               :: Int,
            text_position            :: Int,
            text_text                :: String,
            text_style               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSText where
    parseJSON = error "Not implemented (instance FromJSON JSText)"
     
data JSClef = JSClef {
            clef_voice               :: Int,
            clef_position            :: Int,
            clef_style               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSClef where
    parseJSON = error "Not implemented (instance FromJSON JSClef)"

data JSSlur = JSSlur {
            slur_voice               :: Int,
            slur_position            :: Int,
            slur_duration            :: Int,
            slur_style               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSSlur where
    parseJSON = error "Not implemented (instance FromJSON JSSlur)"

data JSCrescendoLine = JSCrescendoLine {  
            cresc_voice               :: Int,
            cresc_position            :: Int,
            cresc_duration            :: Int,
            cresc_style               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSCrescendoLine where
    parseJSON = error "Not implemented (instance FromJSON JSCrescendoLine)"

data JSDiminuendoLine = JSDiminuendoLine {
            dim_voice               :: Int,
            dim_position            :: Int,
            dim_duration            :: Int,
            dim_style               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSDiminuendoLine where
    parseJSON = error "Not implemented (instance FromJSON JSDiminuendoLine)"

data JSTimeSignature = JSTimeSignature {
            time_voice               :: Int,
            time_position            :: Int,
            time_value               :: Rational,
            time_isCommon            :: Bool,
            time_isAllaBreve         :: Bool
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSTimeSignature where
    parseJSON = error "Not implemented (instance FromJSON JSTimeSignature)"

data JSKeySignature = JSKeySignature {
            key_voice               :: Int,
            key_position            :: Int,
            key_major               :: Bool,
            key_sharps              :: Int,
            key_isOpen              :: Bool
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSKeySignature where
    parseJSON = error "Not implemented (instance FromJSON JSKeySignature)"

data JSTuplet = JSTuplet {
            tuplet_voice               :: Int,
            tuplet_position            :: Int,
            tuplet_duration            :: Int,
            tuplet_playedDuration      :: Int,
            tuplet_value               :: Rational
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSTuplet where
    parseJSON (Object v) = JSTuplet 
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "duration"
        <*> v .: "playedDuration"
        <*> (v .: "value" >>= \[x,y] -> return $ x / y) -- TODO unsafe


data JSChord = JSChord { 
            chord_position            :: Int,
            chord_duration            :: Int,
            chord_voice               :: Int,
            chord_articulations       :: [()], -- TODO
            chord_singleTremolos      :: Int,
            chord_doubleTremolos      :: Int,
            chord_acciaccatura        :: Bool,
            chord_appoggiatura        :: Bool,
            chord_notes               :: [JSNote]
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSChord where
    parseJSON (Object v) = JSChord 
        <$> v .: "position" 
        <*> v .: "duration"
        <*> v .: "voice"
        <*> (return [])
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


{-
decode "{\"bars\":[],\"name\":\"Violin\",\"shortName\":\"Vln.\"}" :: Maybe JSStaff

decode "[{\"bars\":[],\"name\":\"Violin\",\"shortName\":\"Vln.\"}]" :: Maybe [JSStaff]

decode "{\"composer\":\"Hans\",\"information\":\"none\",\"staffHeight\":7,\"staves\":[{\"bars\":[],\"name\":\"Violin\",\"shortName\":\"Vln.\"}],\"systemStaff\":[],\"title\":\"Title\",\"transposing\":false}" :: Maybe JSScore

decode "{\"accidental\":0,\"diatonicPitch\":36,\"pitch\":62,\"style\":0,\"tied\":false}" :: Maybe JSNote
-}




fromJSScore :: JSScore -> Score Note
fromJSScore (JSScore title composer info staffH transp staves systemStaff) =
    foldr (</>) mempty $ fmap fromJSStaff staves

fromJSStaff :: JSStaff -> Score Note
fromJSStaff (JSStaff bars name shortName) =
    removeRests $ scat $ fmap fromJSBar bars
    -- FIXME empty bars!

fromJSBar :: JSBar -> Score (Maybe Note)
fromJSBar (JSBar elems) = 
    (fmap Just $ pcat $ fmap fromJSElem elems) <> (return Nothing)^*1

fromJSElem :: JSElement -> Score Note
fromJSElem = go where
    go (JSElementChord chord) = fromJSChord chord
    -- TODO

fromJSChord :: JSChord -> Score Note
fromJSChord (JSChord pos dur voice ar strem dtrem acci appo notes) = 
    delay (fromIntegral pos / 1024) $ stretch (fromIntegral dur / 1024) $ pcat $ fmap fromJSNote notes
    -- TODO

fromJSNote :: JSNote -> Score Note
fromJSNote (JSNote pitch di acc tied style) = 
    (if tied then fmap beginTie else id) $
    modifyPitches (+ (fromIntegral pitch - 60)) $
    c
    -- TODO

    
main = do
    json <- B.readFile "test.json"
    let jsScore = eitherDecode' json :: Either String JSScore
    case jsScore of
        Left e -> putStrLn $ "Error: " ++ e
        Right x -> do
            writeMidi "test.mid" $ f $ fromJSScore x
            openLy $ f $ fromJSScore x

    -- let score = fromJSScore $ fromJust $ decode' json
    -- openLy score
    
    where
        f = stretch (1)

fromJust (Just x) = x

