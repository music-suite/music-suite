
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, 
             ConstraintKinds, FlexibleContexts, TypeFamilies, CPP, ViewPatterns #-}

module Music.Score.Import.Sibelius (
        IsSibelius(..),
        fromSibelius,
        readSibelius,
        readSibeliusMaybe,
        readSibeliusEither
  ) where

import Control.Lens
import Music.Sibelius
import qualified Data.Maybe
import qualified Music.Score as S
import Data.Aeson
import qualified Music.Prelude
import Music.Pitch.Literal (IsPitch)
import Music.Score hiding (Pitch, Interval, Articulation, Part)
import Music.Pitch
import Music.Articulation
import Music.Dynamics
import Music.Parts
#ifdef GHCI
import qualified System.Process
import Music.Prelude
#endif

import qualified Music.Pitch.Literal as Pitch
import qualified Data.ByteString.Lazy as ByteString

-- |
-- Read a Sibelius score from a file. Fails if the file could not be read or if a parsing
-- error occurs.
-- 
readSibelius :: IsSibelius a => FilePath -> IO (Score a)
readSibelius path = fmap (either (\x -> error $ "Could not read score: " ++ x) id) $ readSibeliusEither path

-- |
-- Read a Sibelius score from a file. Fails if the file could not be read, and returns
-- @Nothing@ if a parsing error occurs.
-- 
readSibeliusMaybe :: IsSibelius a => FilePath -> IO (Maybe (Score a))
readSibeliusMaybe path = fmap (either (const Nothing) Just) $ readSibeliusEither path

-- |
-- Read a Sibelius score from a file. Fails if the file could not be read, and returns
-- @Left m@ if a parsing error occurs.
-- 
readSibeliusEither :: IsSibelius a => FilePath -> IO (Either String (Score a))
readSibeliusEither path = do
    json <- ByteString.readFile path
    return $ fmap fromSibelius $ eitherDecode' json
readSibeliusEither' :: FilePath -> IO (Either String SibeliusScore)
readSibeliusEither' path = do
    json <- ByteString.readFile path
    return $ eitherDecode' json

-- Get the eventual time signature changes in each bar
getSibeliusTimeSignatures :: SibeliusSystemStaff -> [Maybe TimeSignature]
getSibeliusTimeSignatures x = fmap (getTimeSignatureInBar) 
  $ systemStaffBars x
  where
    getTimeSignatureInBar = fmap convertTimeSignature . Data.Maybe.listToMaybe . filter isTimeSignature . barElements

convertTimeSignature :: SibeliusBarObject -> TimeSignature
convertTimeSignature (SibeliusBarObjectTimeSignature (SibeliusTimeSignature voice position [m,n] isCommon isAllaBReve)) = 
    (fromIntegral m / fromIntegral n)  

-- |
-- Convert a score from a Sibelius representation.
--
fromSibelius :: IsSibelius a => SibeliusScore -> Score a
fromSibelius (SibeliusScore title composer info staffH transp staves systemStaff) =
    timeSig $ pcat $ fmap (\staff -> set (parts') (partFromSibeliusStaff staff) (fromSibeliusStaff barDur staff)) $ staves
    -- TODO meta information
        where
            -- FIXME only reads TS in first bar
            barDur = case head (getSibeliusTimeSignatures systemStaff) of
              Nothing -> 1
              Just ts -> barDuration ts
            timeSig = case head (getSibeliusTimeSignatures systemStaff) of
              Nothing -> id
              Just ts -> timeSignature ts
          
            partFromSibeliusStaff (SibeliusStaff bars name shortName) = partFromName (name, shortName)

            -- TODO something more robust (in part library...)
            partFromName ("Piccolo",_) = piccoloFlutes
            partFromName ("Piccolo Flute",_) = piccoloFlutes
            partFromName ("Flute",_) = flutes
            partFromName ("Oboe",_) = oboes
            partFromName ("Cor Anglais",_) = tutti corAnglais
            partFromName ("Clarinet",_) = clarinets
            partFromName ("Clarinet in Bb",_) = clarinets
            partFromName ("Clarinet in A",_) = clarinets
            partFromName ("Bassoon",_) = bassoons
            partFromName ("Bassoon (a)",_) = (!! 0) $ divide 2 cellos
            partFromName ("Bassoon (b)",_) = (!! 1) $ divide 2 cellos
            partFromName ("Horn",_) = horns
            partFromName ("Horn (a)",_) = (!! 0) $ divide 4 $ horns
            partFromName ("Horn (b)",_) = (!! 1) $ divide 4 $ horns
            partFromName ("Horn (c)",_) = (!! 2) $ divide 4 $ horns
            partFromName ("Horn (d)",_) = (!! 3) $ divide 4 $ horns
            partFromName ("Horn in F",_) = horns
            partFromName ("Horn in E",_) = horns
            partFromName ("Trumpet",_) = trumpets
            partFromName ("Trumpet (a)",_) = (!! 0) $ divide 4 $ trumpets
            partFromName ("Trumpet (b)",_) = (!! 1) $ divide 4 $ trumpets
            partFromName ("Trumpet (c)",_) = (!! 2) $ divide 4 $ trumpets
            partFromName ("Trumpet (d)",_) = (!! 3) $ divide 4 $ trumpets
            partFromName ("Trombone",_) = trombones
            partFromName ("Timpani",_) = tutti timpani

            partFromName ("Strings (a)",_) = (!! 0) $ divide 8 violins
            partFromName ("Strings (b)",_) = (!! 0) $ divide 8 cellos
            partFromName ("Strings (c)",_) = (!! 1) $ divide 8 violins
            partFromName ("Strings (d)",_) = (!! 1) $ divide 8 cellos
            partFromName ("Strings (e)",_) = (!! 2) $ divide 8 violins
            partFromName ("Strings (f)",_) = (!! 2) $ divide 8 cellos
            partFromName ("Strings (g)",_) = (!! 3) $ divide 8 violins
            partFromName ("Strings (h)",_) = (!! 3) $ divide 8 cellos
            partFromName ("Strings (i)",_) = (!! 4) $ divide 8 violins
            partFromName ("Strings (j)",_) = (!! 4) $ divide 8 cellos
            partFromName ("Strings (k)",_) = (!! 5) $ divide 8 violins
            partFromName ("Strings (l)",_) = (!! 5) $ divide 8 cellos
            partFromName ("Strings (m)",_) = (!! 6) $ divide 8 violins
            partFromName ("Strings (n)",_) = (!! 6) $ divide 8 cellos
            partFromName ("Strings (o)",_) = (!! 7) $ divide 8 violins
            partFromName ("Strings (p)",_) = (!! 7) $ divide 8 cellos
            -- partFromName ("Strings (q)",_) = (!! 0) $ divide 2 violins
            
            partFromName ("Violin I",_) = violins1
            partFromName ("Violin II",_) = violins2
            partFromName ("Viola",_) = violas
            partFromName ("Violin",_) = violins
            partFromName ("Violoncello",_) = cellos
            partFromName ("Violoncello (a)",_) = (!! 0) $ divide 2 cellos
            partFromName ("Violoncello (b)",_) = (!! 1) $ divide 2 cellos
            partFromName ("Contrabass",_) = doubleBasses
            partFromName ("Piano",_)       = tutti piano
            partFromName ("Piano (a)",_)       = tutti piano
            partFromName ("Piano (b)",_)       = tutti piano

            partFromName ("Soprano",_) = violins1
            partFromName ("Mezzo-Soprano",_) = violins2
            partFromName ("Mezzo-soprano",_) = violins2
            partFromName ("Alto",_) = violas
            partFromName ("Tenor",_) = (!! 0) $ divide 2 cellos
            partFromName ("Baritone",_) = (!! 1) $ divide 2 cellos
            partFromName ("Bass",_) = doubleBasses

            partFromName (n,_) = error $ "Unknown instrument: " ++ n
-- TODO move to Score.Meta.TimeSignature

barDuration :: TimeSignature -> Duration
barDuration (getTimeSignature -> (as,b)) =  realToFrac (sum as) / realToFrac b

fromSibeliusStaff :: IsSibelius a => Duration -> SibeliusStaff -> Score a
fromSibeliusStaff d (SibeliusStaff bars name shortName) =
    removeRests $ scat $ fmap (fromSibeliusBar d) bars
    -- TODO meta information
    -- NOTE slur pos/dur always "stick" to an adjacent note, regardless of visual position
    --      for other lines (cresc etc) this might not be the case
    -- WARNING key sig changes goes at end of previous bar

fromSibeliusBar :: IsSibelius a => Duration -> SibeliusBar -> Score (Maybe a)
fromSibeliusBar d (SibeliusBar elems) = 
    fmap Just (pcat $ fmap fromSibeliusChordElem chords) <> return Nothing^*d
    where
        chords   = filter isChord elems
        tuplets  = filter isTuplet elems -- TODO use these
        floating = filter isFloating elems

fromSibeliusChordElem :: IsSibelius a => SibeliusBarObject -> Score a
fromSibeliusChordElem = go where
    go (SibeliusBarObjectChord chord) = fromSibeliusChord chord
    go _                         = error "fromSibeliusChordElem: Expected chord"

-- handleFloatingElem :: IsSibelius a => SibeliusBarObject -> [Score a] -> [Score a]



-- In Sibelius, bar objects are either chords, tuplet or a "floating" object (i.e. one that has no duraion)
isChord :: SibeliusBarObject -> Bool
isChord (SibeliusBarObjectChord _) = True
isChord _                     = False

isTuplet :: SibeliusBarObject -> Bool
isTuplet (SibeliusBarObjectTuplet _) = True
isTuplet _                      = False

isFloating :: SibeliusBarObject -> Bool
isFloating x = not (isChord x) && not (isTuplet x) 

    

fromSibeliusChord :: (
  IsSibelius a
  ) => SibeliusChord -> Score a
fromSibeliusChord (SibeliusChord pos dur voice ar strem dtrem acci appo notes) = 
    showVals $ setTime $ setDur $ every setArt ar $ tremolo strem $ pcat $ fmap fromSibeliusNote notes
    where     
        -- showVals = text (show pos ++ " " ++ show dur) -- TODO DEBUG
        showVals = id
        -- WARNING for tuplets, positions are absolute (sounding), but durations are relative (written)
        -- To retrieve sounding duration we must find floating tuplet objects and use
        -- the duration/playedDuration fields
        setTime = delay (fromIntegral pos / kTicksPerWholeNote)
        setDur  = stretch (fromIntegral dur / kTicksPerWholeNote)
        setArt Marcato         = marcato
        setArt Accent          = accent
        setArt Tenuto          = tenuto
        setArt Staccato        = staccato
        setArt a               = error $ "fromSibeliusChord: Unsupported articulation" ++ show a        
    -- TODO tremolo and appogiatura/acciaccatura support


fromSibeliusNote :: (IsSibelius a, Tiable a) => SibeliusNote -> Score a
fromSibeliusNote (SibeliusNote pitch diatonicPitch acc tied style) =
    (if tied then fmap beginTie else id)
    $ fromPitch'' actualPitch
    -- TODO spell correctly if this is Common.Pitch (how to distinguish)
    where
      actualPitch = midiOrigin .+^ (d2^*fromIntegral diatonicPitch ^+^ _A1^*fromIntegral pitch)
      midiOrigin = octavesDown 5 Pitch.c -- As middle C is (60 = 5*12)
      
fromPitch'' :: IsPitch a => Music.Prelude.Pitch -> a
fromPitch'' x = let i = x .-. c in 
  fromPitch $ PitchL ((fromIntegral $ i^._steps) `mod` 7, Just (fromIntegral (i^._alteration)), fromIntegral $ octaves i)

-- |
-- This constraint includes all note types that can be constructed from a Sibelius representation.
--
type IsSibelius a = (
    HasPitches' a, 
    IsPitch a, 

    HasPart' a, 
    S.Part a ~ Part,

    HasArticulation' a,
    S.Articulation a ~ Articulation,

    HasDynamic' a,
    S.Dynamic a ~ Dynamics,
    
    HasText a, 
    HasTremolo a,
    Tiable a
    -- Num (Pitch a), 
    -- HasTremolo a, 
    -- HasText a,
    -- Tiable a
    )


-- Util

every :: (a -> b -> b) -> [a] -> b -> b
every f = flip (foldr f)

kTicksPerWholeNote = 1024 -- Always in Sibelius

-- Debug
#ifdef GHCI
openAudacity :: Score StandardNote -> IO ()    
openAudacity x = do
    void $ writeMidi "test.mid" $ x
    void $ System.Process.system "timidity -Ow test.mid"
    void $ System.Process.system "open -a Audacity test.wav"
#endif