
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, 
             ConstraintKinds, FlexibleContexts #-}

module Music.Sibelius where

import Control.Monad.Plus
import Control.Applicative
import Data.Semigroup
import Data.Aeson

import qualified Data.HashMap.Strict as HashMap


{-
setTitle :: String -> Score a -> Score a
setTitle = setMeta "title"
setComposer :: String -> Score a -> Score a
setComposer = setMeta "composer"
setInformation :: String -> Score a -> Score a
setInformation = setMeta "information"
setMeta :: String -> String -> Score a -> Score a
setMeta _ _ = id
-}


data SibScore = SibScore {
            scoreTitle             :: String,
            scoreComposer          :: String,
            scoreInformation       :: String,
            scoreStaffHeight       :: Double,
            scoreTransposing       :: Bool,
            scoreStaves            :: [SibStaff],
            scoreSystemStaff       :: ()
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibScore where
    parseJSON (Object v) = SibScore
        <$> v .: "title" 
        <*> v .: "composer"
        <*> v .: "information"
        <*> v .: "staffHeight"
        <*> v .: "transposing"
        <*> v .: "staves"          
        -- TODO
        <*> return ()


data SibStaff = SibStaff {
            staffBars                :: [SibBar],
            staffName                :: String,
            staffShortName           :: String
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibStaff where
    parseJSON (Object v) = SibStaff
        <$> v .: "bars"
        <*> v .: "name"
        <*> v .: "shortName"

data SibBar = SibBar {
            barElements            :: [SibElement]
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibBar where
    parseJSON (Object v) = SibBar
        <$> v .: "elements"

data SibElement 
    = SibElementText SibText
    | SibElementClef SibClef
    | SibElementSlur SibSlur
    | SibElementCrescendoLine SibCrescendoLine
    | SibElementDiminuendoLine SibDiminuendoLine
    | SibElementTimeSignature SibTimeSignature
    | SibElementKeySignature SibKeySignature
    | SibElementTuplet SibTuplet
    | SibElementChord SibChord
    deriving (Eq, Ord, Show)
instance FromJSON SibElement where
    parseJSON x@(Object v) = case HashMap.lookup "type" v of    
        -- TODO
        Just "text"      -> error "JsElementText"
        Just "clef"      -> error "SibElementClef"
        Just "slur"      -> error "SibElementSlur"
        Just "cresc"     -> error "SibElementCrescendoLine"
        Just "dim"       -> error "SibElementDiminuendoLine"
        Just "time"      -> error "SibElementTimeSignature"
        Just "key"       -> error "SibElementKeySignature"
        Just "tuplet"    -> error "SibElementTuplet"
        Just "chord"     -> SibElementChord <$> parseJSON x
        _                -> mempty

data SibText = SibText {
            textVoice               :: Int,
            textPosition            :: Int,
            textText                :: String,
            textStyle               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibText where
    parseJSON = error "Not implemented (instance FromJSON SibText)"
     
data SibClef = SibClef {
            clefVoice               :: Int,
            clefPosition            :: Int,
            clefStyle               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibClef where
    parseJSON = error "Not implemented (instance FromJSON SibClef)"

data SibSlur = SibSlur {
            slurVoice               :: Int,
            slurPosition            :: Int,
            slurDuration            :: Int,
            slurStyle               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibSlur where
    parseJSON = error "Not implemented (instance FromJSON SibSlur)"

data SibCrescendoLine = SibCrescendoLine {  
            crescVoice               :: Int,
            crescPosition            :: Int,
            crescDuration            :: Int,
            crescStyle               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibCrescendoLine where
    parseJSON = error "Not implemented (instance FromJSON SibCrescendoLine)"

data SibDiminuendoLine = SibDiminuendoLine {
            dimVoice               :: Int,
            dimPosition            :: Int,
            dimDuration            :: Int,
            dimStyle               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibDiminuendoLine where
    parseJSON = error "Not implemented (instance FromJSON SibDiminuendoLine)"

data SibTimeSignature = SibTimeSignature {
            timeVoice               :: Int,
            timePosition            :: Int,
            timeValue               :: Rational,
            timeIsCommon            :: Bool,
            timeIsAllaBreve         :: Bool
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibTimeSignature where
    parseJSON = error "Not implemented (instance FromJSON SibTimeSignature)"

data SibKeySignature = SibKeySignature {
            keyVoice               :: Int,
            keyPosition            :: Int,
            keyMajor               :: Bool,
            keySharps              :: Int,
            keyIsOpen              :: Bool
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibKeySignature where
    parseJSON = error "Not implemented (instance FromJSON SibKeySignature)"

data SibTuplet = SibTuplet {
            tupletVoice               :: Int,
            tupletPosition            :: Int,
            tupletDuration            :: Int,
            tupletPlayedDuration      :: Int,
            tupletValue               :: Rational
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibTuplet where
    parseJSON (Object v) = SibTuplet 
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "duration"
        <*> v .: "playedDuration"
        <*> (v .: "value" >>= \[x,y] -> return $ x / y) -- TODO unsafe

data SibArticulation
    = UpBow
    | DownBow
    | Plus
    | Harmonic
    | Marcato
    | Accent
    | Tenuto
    | Wedge
    | Staccatissimo
    | Staccato
    deriving (Eq, Ord, Show, Enum)

readSibArticulation :: String -> Maybe SibArticulation
readSibArticulation = go
    where
        go "upbow"          = Just UpBow
        go "downBow"        = Just DownBow
        go "plus"           = Just Plus
        go "harmonic"       = Just Harmonic
        go "marcato"        = Just Marcato
        go "accent"         = Just Accent
        go "tenuto"         = Just Tenuto
        go "wedge"          = Just Wedge
        go "staccatissimo"  = Just Staccatissimo
        go "staccato"       = Just Staccato
        go _                = Nothing
    
data SibChord = SibChord { 
            chordPosition            :: Int,
            chordDuration            :: Int,
            chordVoice               :: Int,
            chordArticulations       :: [SibArticulation], -- TODO
            chordSingleTremolos      :: Int,
            chordDoubleTremolos      :: Int,
            chordAcciaccatura        :: Bool,
            chordAppoggiatura        :: Bool,
            chordNotes               :: [SibNote]
    }
    deriving (Eq, Ord, Show)

instance FromJSON SibChord where
    parseJSON (Object v) = SibChord 
        <$> v .: "position" 
        <*> v .: "duration"
        <*> v .: "voice"
        <*> fmap (mmapMaybe readSibArticulation) (v .: "articulations")
        <*> v .: "singleTremolos"
        <*> v .: "doubleTremolos"
        <*> v .: "acciaccatura"
        <*> v .: "appoggiatura"
        <*> v .: "notes"

data SibNote = SibNote {
            notePitch               :: Int,
            noteDiatonicPitch       :: Int,
            noteAccidental          :: Int,
            noteTied                :: Bool,
            noteStyle               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibNote where
    parseJSON (Object v) = SibNote 
        <$> v .: "pitch" 
        <*> v .: "diatonicPitch"
        <*> v .: "accidental"
        <*> v .: "tied"
        <*> v .: "style"



    
-- main = do   
--     result <- readSibEither "test.json"
--     case result of
--         Left e -> putStrLn $ "Error: " ++ e
--         Right x -> do
--             -- writeMidi "test.mid" $ asScore $ f x
--             -- openXml $ f x
--             openLy $ asScore $ f x
-- 
--     -- let score = fromSib $ fromJust $ decode' json
--     -- openLy score
--     
--     where                
--         -- f = id   
--         f = retrograde
--         -- f x = stretch (1/4) $ times 2 x |> times 2 (stretch 2 x)
-- 
--         fromJust (Just x) = x

