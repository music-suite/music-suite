
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, 
             ConstraintKinds, FlexibleContexts #-}

module Data.Music.Sibelius (

        -- * Scores and staves
        SibeliusScore(..),
        SibeliusStaff(..),
        SibeliusSystemStaff(..),
        SibeliusBar(..),
        
        -- * Bar objects
        SibeliusBarObject(..),
        isTimeSignature,
        
        -- ** Notes
        SibeliusChord(..),
        SibeliusNote(..),
        
        -- ** Lines
        SibeliusSlur(..),
        SibeliusCrescendoLine(..),
        SibeliusDiminuendoLine(..),
        
        -- ** Tuplets
        SibeliusTuplet(..),
        SibeliusArticulation(..),
        readSibeliusArticulation,
        
        -- ** Miscellaneous
        SibeliusClef(..),
        SibeliusKeySignature(..),
        SibeliusTimeSignature(..),
        SibeliusText(..),

  ) where

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


data SibeliusScore = SibeliusScore {
            scoreTitle             :: String,
            scoreComposer          :: String,
            scoreInformation       :: String,
            scoreStaffHeight       :: Double,
            scoreTransposing       :: Bool,
            scoreStaves            :: [SibeliusStaff],
            scoreSystemStaff       :: SibeliusSystemStaff
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibeliusScore where
    parseJSON (Object v) = SibeliusScore
        <$> v .: "title" 
        <*> v .: "composer"
        <*> v .: "information"
        <*> v .: "staffHeight"
        <*> v .: "transposing"
        <*> v .: "staves"          
        <*> v .: "systemStaff"          

data SibeliusSystemStaff = SibeliusSystemStaff {
            systemStaffBars                :: [SibeliusBar]
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibeliusSystemStaff where
    parseJSON (Object v) = SibeliusSystemStaff
        <$> v .: "bars"

data SibeliusStaff = SibeliusStaff {
            staffBars                :: [SibeliusBar],
            staffName                :: String,
            staffShortName           :: String
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibeliusStaff where
    parseJSON (Object v) = SibeliusStaff
        <$> v .: "bars"
        <*> v .: "name"
        <*> v .: "shortName"

data SibeliusBar = SibeliusBar {
            barElements            :: [SibeliusBarObject]
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibeliusBar where
    parseJSON (Object v) = SibeliusBar
        <$> v .: "elements"

data SibeliusBarObject 
    = SibeliusBarObjectText SibeliusText
    | SibeliusBarObjectClef SibeliusClef
    | SibeliusBarObjectSlur SibeliusSlur
    | SibeliusBarObjectCrescendoLine SibeliusCrescendoLine
    | SibeliusBarObjectDiminuendoLine SibeliusDiminuendoLine
    | SibeliusBarObjectTimeSignature SibeliusTimeSignature
    | SibeliusBarObjectKeySignature SibeliusKeySignature
    | SibeliusBarObjectTuplet SibeliusTuplet
    | SibeliusBarObjectChord SibeliusChord
    | SibeliusBarObjectUnknown String -- type
    deriving (Eq, Ord, Show)
-- TODO highlights, lyric, barlines, comment, other lines and symbols

isTimeSignature (SibeliusBarObjectTimeSignature _) = True
isTimeSignature _ = False

instance FromJSON SibeliusBarObject where
    parseJSON x@(Object v) = case HashMap.lookup "type" v of    
        -- TODO
        Just "text"      -> SibeliusBarObjectText <$> parseJSON x
        Just "clef"      -> SibeliusBarObjectClef <$> parseJSON x
        Just "slur"      -> SibeliusBarObjectSlur <$> parseJSON x
        Just "cresc"     -> SibeliusBarObjectCrescendoLine <$> parseJSON x
        Just "dim"       -> SibeliusBarObjectDiminuendoLine <$> parseJSON x
        Just "time"      -> SibeliusBarObjectTimeSignature <$> parseJSON x
        Just "key"       -> SibeliusBarObjectKeySignature <$> parseJSON x
        Just "tuplet"    -> SibeliusBarObjectTuplet <$> parseJSON x
        Just "chord"     -> SibeliusBarObjectChord <$> parseJSON x
        Just typ         -> SibeliusBarObjectUnknown <$> (return $ show typ)
        _                -> mempty -- failure: no type field

data SibeliusText = SibeliusText {
            textVoice               :: Int,
            textPosition            :: Int,
            textText                :: String,
            textStyle               :: Maybe String
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibeliusText where
    parseJSON (Object v) = SibeliusText
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "text"
        <*> v .: "style"
     
data SibeliusClef = SibeliusClef {
            clefVoice               :: Int,
            clefPosition            :: Int,
            clefStyle               :: Maybe String
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibeliusClef where
    parseJSON (Object v) = SibeliusClef
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "style"

data SibeliusSlur = SibeliusSlur {
            slurVoice               :: Int,
            slurPosition            :: Int,
            slurDuration            :: Int,
            slurStyle               :: Maybe String
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibeliusSlur where
    parseJSON (Object v) = SibeliusSlur
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "duration"
        <*> v .: "style"

data SibeliusCrescendoLine = SibeliusCrescendoLine {  
            crescVoice               :: Int,
            crescPosition            :: Int,
            crescDuration            :: Int,
            crescStyle               :: Maybe String
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibeliusCrescendoLine where
    parseJSON (Object v) = SibeliusCrescendoLine
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "duration"
        <*> v .: "style"

data SibeliusDiminuendoLine = SibeliusDiminuendoLine {
            dimVoice               :: Int,
            dimPosition            :: Int,
            dimDuration            :: Int,
            dimStyle               :: Maybe String
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibeliusDiminuendoLine where
    parseJSON (Object v) = SibeliusDiminuendoLine
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "duration"
        <*> v .: "style"

data SibeliusTimeSignature = SibeliusTimeSignature {
            timeVoice               :: Int,
            timePosition            :: Int,
            timeValue               :: [Int],
            timeIsCommon            :: Bool,
            timeIsAllaBreve         :: Bool
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibeliusTimeSignature where
    parseJSON (Object v) = SibeliusTimeSignature
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "value"
        <*> v .: "common"
        <*> v .: "allaBreve"

data SibeliusKeySignature = SibeliusKeySignature {
            keyVoice               :: Int,
            keyPosition            :: Int,
            keyMajor               :: Bool,
            keySharps              :: Int,
            keyIsOpen              :: Bool
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibeliusKeySignature where
    parseJSON (Object v) = SibeliusKeySignature
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "major"
        <*> v .: "sharps"
        <*> v .: "isOpen"

data SibeliusTuplet = SibeliusTuplet {
            tupletVoice               :: Int,
            tupletPosition            :: Int,
            tupletDuration            :: Int,
            tupletPlayedDuration      :: Int,
            tupletValue               :: [Int]
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibeliusTuplet where
    parseJSON (Object v) = SibeliusTuplet 
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "duration"
        <*> v .: "playedDuration"
        <*> v .: "value"

data SibeliusArticulation
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

readSibeliusArticulation :: String -> Maybe SibeliusArticulation
readSibeliusArticulation = go
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
    
data SibeliusChord = SibeliusChord { 
            chordPosition            :: Int,
            chordDuration            :: Int,
            chordVoice               :: Int,
            chordArticulations       :: [SibeliusArticulation], -- TODO
            chordSingleTremolos      :: Int,
            chordDoubleTremolos      :: Int,
            chordAcciaccatura        :: Bool,
            chordAppoggiatura        :: Bool,
            chordNotes               :: [SibeliusNote]
    }
    deriving (Eq, Ord, Show)

instance FromJSON SibeliusChord where
    parseJSON (Object v) = SibeliusChord 
        <$> v .: "position" 
        <*> v .: "duration"
        <*> v .: "voice"
        <*> fmap (mmapMaybe readSibeliusArticulation) (v .: "articulations")
        <*> v .: "singleTremolos"
        <*> v .: "doubleTremolos"
        <*> v .: "acciaccatura"
        <*> v .: "appoggiatura"
        <*> v .: "notes"

data SibeliusNote = SibeliusNote {
            notePitch               :: Int,
            noteDiatonicPitch       :: Int,
            noteAccidental          :: Int,
            noteTied                :: Bool,
            noteStyle               :: Maybe Int -- not String?
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibeliusNote where
    parseJSON (Object v) = SibeliusNote 
        <$> v .: "pitch" 
        <*> v .: "diatonicPitch"
        <*> v .: "accidental"
        <*> v .: "tied"
        <*> v .: "style"



    
