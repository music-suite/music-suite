
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, 
             ConstraintKinds, FlexibleContexts #-}

module Music.Sibelius (

        -- * Scores and staves
        SibScore(..),
        SibStaff(..),
        SibBar(..),
        
        -- * Bar objects
        SibBarObject(..),

        -- ** Notes
        SibChord(..),
        SibNote(..),

        -- ** Lines
        SibSlur(..),
        SibCrescendoLine(..),
        SibDiminuendoLine(..),

        -- ** Tuplets
        SibTuplet(..),
        SibArticulation(..),
        readSibArticulation,

        -- ** Miscellaneous
        SibClef(..),
        SibKeySignature(..),
        SibTimeSignature(..),
        SibText(..),

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
        -- TODO system staff
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
            barElements            :: [SibBarObject]
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibBar where
    parseJSON (Object v) = SibBar
        <$> v .: "elements"

data SibBarObject 
    = SibBarObjectText SibText
    | SibBarObjectClef SibClef
    | SibBarObjectSlur SibSlur
    | SibBarObjectCrescendoLine SibCrescendoLine
    | SibBarObjectDiminuendoLine SibDiminuendoLine
    | SibBarObjectTimeSignature SibTimeSignature
    | SibBarObjectKeySignature SibKeySignature
    | SibBarObjectTuplet SibTuplet
    | SibBarObjectChord SibChord
    deriving (Eq, Ord, Show)
instance FromJSON SibBarObject where
    parseJSON x@(Object v) = case HashMap.lookup "type" v of    
        -- TODO
        Just "text"      -> SibBarObjectText <$> parseJSON x
        Just "clef"      -> SibBarObjectClef <$> parseJSON x
        Just "slur"      -> SibBarObjectSlur <$> parseJSON x
        Just "cresc"     -> SibBarObjectCrescendoLine <$> parseJSON x
        Just "dim"       -> SibBarObjectDiminuendoLine <$> parseJSON x
        Just "time"      -> SibBarObjectTimeSignature <$> parseJSON x
        Just "key"       -> SibBarObjectKeySignature <$> parseJSON x
        Just "tuplet"    -> SibBarObjectTuplet <$> parseJSON x
        Just "chord"     -> SibBarObjectChord <$> parseJSON x
        _                -> mempty -- failure

data SibText = SibText {
            textVoice               :: Int,
            textPosition            :: Int,
            textText                :: String,
            textStyle               :: String
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibText where
    parseJSON (Object v) = SibText
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "text"
        <*> v .: "style"
     
data SibClef = SibClef {
            clefVoice               :: Int,
            clefPosition            :: Int,
            clefStyle               :: String
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibClef where
    parseJSON (Object v) = SibClef
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "style"

data SibSlur = SibSlur {
            slurVoice               :: Int,
            slurPosition            :: Int,
            slurDuration            :: Int,
            slurStyle               :: String
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibSlur where
    parseJSON (Object v) = SibSlur
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "duration"
        <*> v .: "style"

data SibCrescendoLine = SibCrescendoLine {  
            crescVoice               :: Int,
            crescPosition            :: Int,
            crescDuration            :: Int,
            crescStyle               :: String
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibCrescendoLine where
    parseJSON (Object v) = SibCrescendoLine
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "duration"
        <*> v .: "style"

data SibDiminuendoLine = SibDiminuendoLine {
            dimVoice               :: Int,
            dimPosition            :: Int,
            dimDuration            :: Int,
            dimStyle               :: String
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibDiminuendoLine where
    parseJSON (Object v) = SibDiminuendoLine
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "duration"
        <*> v .: "style"

data SibTimeSignature = SibTimeSignature {
            timeVoice               :: Int,
            timePosition            :: Int,
            timeValue               :: Rational,
            timeIsCommon            :: Bool,
            timeIsAllaBreve         :: Bool
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibTimeSignature where
    parseJSON (Object v) = SibTimeSignature
        <$> v .: "voice" 
        <*> v .: "position"
        <*> fmap (\[x,y] -> (x::Rational) / (y::Rational)) (v .: "value")
        <*> v .: "common"
        <*> v .: "allaBreve"

data SibKeySignature = SibKeySignature {
            keyVoice               :: Int,
            keyPosition            :: Int,
            keyMajor               :: Bool,
            keySharps              :: Int,
            keyIsOpen              :: Bool
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibKeySignature where
    parseJSON (Object v) = SibKeySignature
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "major"
        <*> v .: "sharps"
        <*> v .: "isOpen"

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
            noteStyle               :: Int -- not String?
    }
    deriving (Eq, Ord, Show)
instance FromJSON SibNote where
    parseJSON (Object v) = SibNote 
        <$> v .: "pitch" 
        <*> v .: "diatonicPitch"
        <*> v .: "accidental"
        <*> v .: "tied"
        <*> v .: "style"



    
