
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Music.Sibelius where

import Data.Aeson
import Control.Applicative
import Data.Aeson.Types(parse, Parser)
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
            scoreTitle             :: String,
            scoreComposer          :: String,
            scoreInformation       :: String,
            scoreStaffHeight       :: Double,
            scoreTransposing       :: Bool,
            scoreStaves            :: [JSStaff],
            scoreSystemStaff       :: ()
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
        <*> return ()


data JSStaff = JSStaff {
            staffBars                :: [JSBar],
            staffName                :: String,
            staffShortName           :: String
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSStaff where
    parseJSON (Object v) = JSStaff
        <$> v .: "bars"
        <*> v .: "name"
        <*> v .: "shortName"

data JSBar = JSBar {
            barElements            :: [JSElement]
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
            textVoice               :: Int,
            textPosition            :: Int,
            textText                :: String,
            textStyle               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSText where
    parseJSON = error "Not implemented (instance FromJSON JSText)"
     
data JSClef = JSClef {
            clefVoice               :: Int,
            clefPosition            :: Int,
            clefStyle               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSClef where
    parseJSON = error "Not implemented (instance FromJSON JSClef)"

data JSSlur = JSSlur {
            slurVoice               :: Int,
            slurPosition            :: Int,
            slurDuration            :: Int,
            slurStyle               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSSlur where
    parseJSON = error "Not implemented (instance FromJSON JSSlur)"

data JSCrescendoLine = JSCrescendoLine {  
            crescVoice               :: Int,
            crescPosition            :: Int,
            crescDuration            :: Int,
            crescStyle               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSCrescendoLine where
    parseJSON = error "Not implemented (instance FromJSON JSCrescendoLine)"

data JSDiminuendoLine = JSDiminuendoLine {
            dimVoice               :: Int,
            dimPosition            :: Int,
            dimDuration            :: Int,
            dimStyle               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSDiminuendoLine where
    parseJSON = error "Not implemented (instance FromJSON JSDiminuendoLine)"

data JSTimeSignature = JSTimeSignature {
            timeVoice               :: Int,
            timePosition            :: Int,
            timeValue               :: Rational,
            timeIsCommon            :: Bool,
            timeIsAllaBreve         :: Bool
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSTimeSignature where
    parseJSON = error "Not implemented (instance FromJSON JSTimeSignature)"

data JSKeySignature = JSKeySignature {
            keyVoice               :: Int,
            keyPosition            :: Int,
            keyMajor               :: Bool,
            keySharps              :: Int,
            keyIsOpen              :: Bool
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSKeySignature where
    parseJSON = error "Not implemented (instance FromJSON JSKeySignature)"

data JSTuplet = JSTuplet {
            tupletVoice               :: Int,
            tupletPosition            :: Int,
            tupletDuration            :: Int,
            tupletPlayedDuration      :: Int,
            tupletValue               :: Rational
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSTuplet where
    parseJSON (Object v) = JSTuplet 
        <$> v .: "voice" 
        <*> v .: "position"
        <*> v .: "duration"
        <*> v .: "playedDuration"
        <*> (v .: "value" >>= \[x,y] -> return $ x / y) -- TODO unsafe

data JSArticulation
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

readJSArticulation :: String -> Maybe JSArticulation
readJSArticulation = go
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
    
data JSChord = JSChord { 
            chordPosition            :: Int,
            chordDuration            :: Int,
            chordVoice               :: Int,
            chordArticulations       :: [JSArticulation], -- TODO
            chordSingleTremolos      :: Int,
            chordDoubleTremolos      :: Int,
            chordAcciaccatura        :: Bool,
            chordAppoggiatura        :: Bool,
            chordNotes               :: [JSNote]
    }
    deriving (Eq, Ord, Show)

instance FromJSON JSChord where
    parseJSON (Object v) = JSChord 
        <$> v .: "position" 
        <*> v .: "duration"
        <*> v .: "voice"
        <*> doThing (v .: "articulations")
        <*> v .: "singleTremolos"
        <*> v .: "doubleTremolos"
        <*> v .: "acciaccatura"
        <*> v .: "appoggiatura"
        <*> v .: "notes"

doThing = (=<<) (sequence . fmap (returnMaybe readJSArticulation))


data JSNote = JSNote {
            notePitch               :: Int,
            noteDiatonicPitch       :: Int,
            noteAccidental          :: Int,
            noteTied                :: Bool,
            noteStyle               :: Int
    }
    deriving (Eq, Ord, Show)
instance FromJSON JSNote where
    parseJSON (Object v) = JSNote 
        <$> v .: "pitch" 
        <*> v .: "diatonicPitch"
        <*> v .: "accidental"
        <*> v .: "tied"
        <*> v .: "style"



fromJSScore :: JSScore -> Score Note
fromJSScore (JSScore title composer info staffH transp staves systemStaff) =
    foldr (</>) mempty $ fmap fromJSStaff staves
    -- TODO meta information

fromJSStaff :: JSStaff -> Score Note
fromJSStaff (JSStaff bars name shortName) =
    removeRests $ scat $ fmap fromJSBar bars
    -- TODO bar length hardcoded
    -- TODO meta information

fromJSBar :: JSBar -> Score (Maybe Note)
fromJSBar (JSBar elems) = 
    fmap Just (pcat $ fmap fromJSElem elems) <> return Nothing^*1

fromJSElem :: JSElement -> Score Note
fromJSElem = go where
    go (JSElementChord chord) = fromJSChord chord
    -- TODO tuplet, key/time signature, line and text support

fromJSChord :: JSChord -> Score Note
fromJSChord (JSChord pos dur voice ar strem dtrem acci appo notes) = 
    setTime $ setDur $ every setArt ar $ tremolo strem $ pcat $ fmap fromJSNote notes
    where
        setTime = delay (fromIntegral pos / 1024)
        setDur  = stretch (fromIntegral dur / 1024)
        setArt Marcato         = marcato
        setArt Accent          = accent
        setArt Tenuto          = tenuto
        setArt Staccato        = staccato
        setArt a               = error $ "fromJSChord: Unsupported articulation" ++ show a        
    -- TODO tremolo and appogiatura/acciaccatura support

fromJSNote :: JSNote -> Score Note
fromJSNote (JSNote pitch di acc tied style) =
    (if tied then fmap beginTie else id)
    $ modifyPitches (+ (fromIntegral pitch - 60)) def
    where
        def = c

readSib :: FilePath -> IO (Score Note)
readSib path = fmap (either (\x -> error $ "Could not read score" ++ x) id) $ readSibEither path

readSibMaybe :: FilePath -> IO (Maybe (Score Note))
readSibMaybe path = fmap (either (const Nothing) Just) $ readSibEither path

readSibEither :: FilePath -> IO (Either String (Score Note))
readSibEither path = do
    json <- B.readFile path
    return $ fmap fromJSScore $ eitherDecode' json
    
main = do   
    result <- readSibEither "test.json"
    case result of
        Left e -> putStrLn $ "Error: " ++ e
        Right x -> do
            writeMidi "test.mid" $ f x
            openXml $ f x
            -- openLy  $ f x

    -- let score = fromJSScore $ fromJust $ decode' json
    -- openLy score
    
    where                
        f = id   
        -- f = retrograde
        -- f x = stretch (1/4) $ times 2 x |> times 2 (stretch 2 x)

fromJust (Just x) = x

returnMaybe :: MonadPlus m => (a -> Maybe b) -> a -> m b
returnMaybe f = mmapMaybe f . return                    

every :: (a -> b -> b) -> [a] -> b -> b
every f x = foldr (.) id (fmap f x)

