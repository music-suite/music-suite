
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, 
             ConstraintKinds, FlexibleContexts, TypeFamilies, CPP #-}

module Music.Score.Import.Sibelius (
        IsSibelius(..),
        -- fromSibelius,
        -- readSibelius,
        -- readSibeliusMaybe,
        -- readSibeliusEither
  ) where

import Control.Lens
import Music.Sibelius
import Music.Score
import Music.Pitch hiding (Pitch, Interval)
import Data.Aeson
import qualified Music.Prelude
import Music.Pitch.Literal (IsPitch)
#ifdef GHCI
import qualified System.Process
import Music.Prelude (StandardNote)
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



-- |
-- Convert a score from a Sibelius representation.
--
fromSibelius :: IsSibelius a => SibeliusScore -> Score a
fromSibelius (SibeliusScore title composer info staffH transp staves systemStaff) =
    foldr (</>) mempty $ fmap fromSibeliusStaff staves
    -- TODO meta information

fromSibeliusStaff :: IsSibelius a => SibeliusStaff -> Score a
fromSibeliusStaff (SibeliusStaff bars name shortName) =
    removeRests $ scat $ fmap fromSibeliusBar bars
    -- TODO bar length hardcoded
    -- TODO meta information
    -- NOTE slur pos/dur always "stick" to an adjacent note, regardless of visual position
    --      for other lines (cresc etc) this might not be the case
    -- WARNING key sig changes goes at end of previous bar

fromSibeliusBar :: IsSibelius a => SibeliusBar -> Score (Maybe a)
fromSibeliusBar (SibeliusBar elems) = 
    fmap Just (pcat $ fmap fromSibeliusChordElem chords) <> return Nothing^*1
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
      midiOrigin = octavesDown 5 (Pitch.c :: Music.Prelude.Pitch) -- As middle C is (60 = 5*12)
      
fromPitch'' :: IsPitch a => Music.Prelude.Pitch -> a
fromPitch'' x = let i = x .-. (c :: Music.Prelude.Pitch) in 
  fromPitch $ PitchL ((fromIntegral $ i^._steps) `mod` 7, Just (fromIntegral (i^._alteration)), fromIntegral $ octaves i)

-- |
-- This constraint includes all note types that can be constructed from a Sibelius representation.
--
type IsSibelius a = (
    IsPitch a, 
    HasPitches' a, 
    HasPart' a, 
    HasArticulation' a,
    HasDynamic' a,
    Pitch a ~ Behavior Music.Prelude.Pitch,
    
    HasText a, 
    Ord (Part a), 
    Articulation a ~ Music.Prelude.Articulation,
    HasTremolo a,
    Tiable a,
    
    Enum (Part a) 
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