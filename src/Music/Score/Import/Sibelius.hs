
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, 
             ConstraintKinds, FlexibleContexts #-}

module Music.Score.Import.Sibelius (
        IsSibelius(..),
        fromSib,
        readSib,
        readSibMaybe,
        readSibEither
  ) where

import Music.Sibelius
import Music.Score
import Data.Aeson
import Music.Pitch.Literal (IsPitch)

import qualified Music.Pitch.Literal as Pitch
import qualified Data.ByteString.Lazy as ByteString

-- |
-- Convert a score from a Sibelius representation.
--
fromSib :: IsSibelius a => SibScore -> Score a
fromSib (SibScore title composer info staffH transp staves systemStaff) =
    foldr (</>) mempty $ fmap fromSibStaff staves
    -- TODO meta information

fromSibStaff :: IsSibelius a => SibStaff -> Score a
fromSibStaff (SibStaff bars name shortName) =
    removeRests $ scat $ fmap fromSibBar bars
    -- TODO bar length hardcoded
    -- TODO meta information

fromSibBar :: IsSibelius a => SibBar -> Score (Maybe a)
fromSibBar (SibBar elems) = 
    fmap Just (pcat $ fmap fromSibChordElem chords) <> return Nothing^*1
    where
        chords   = filter isChord elems
        tuplets  = filter isTuplet elems -- TODO use these
        floating = filter isFloating elems

fromSibChordElem :: IsSibelius a => SibBarObject -> Score a
fromSibChordElem = go where
    go (SibBarObjectChord chord) = fromSibChord chord
    go _                         = error "fromSibChordElem: Expected chord"

-- handleFloatingElem :: IsSibelius a => SibBarObject -> [Score a] -> [Score a]

isChord (SibBarObjectChord _) = True
isChord _                     = False

isTuplet (SibBarObjectTuplet _) = True
isTuplet _                      = False

isFloating x = not (isChord x) && not (isTuplet x) 
    

fromSibChord :: IsSibelius a => SibChord -> Score a
fromSibChord (SibChord pos dur voice ar strem dtrem acci appo notes) = 
    setTime $ setDur $ every setArt ar $ tremolo strem $ pcat $ fmap fromSibNote notes
    where
        setTime = delay (fromIntegral pos / kTicksPerWholeNote)
        setDur  = stretch (fromIntegral dur / kTicksPerWholeNote)
        setArt Marcato         = marcato
        setArt Accent          = accent
        setArt Tenuto          = tenuto
        setArt Staccato        = staccato
        setArt a               = error $ "fromSibChord: Unsupported articulation" ++ show a        
    -- TODO tremolo and appogiatura/acciaccatura support

fromSibNote :: IsSibelius a => SibNote -> Score a
fromSibNote (SibNote pitch diatonicPitch acc tied style) =
    (if tied then fmap beginTie else id)
    $ up' (fromIntegral pitch - 60) Pitch.c
    -- TODO spell correctly if this is Common.Pitch (how to distinguish)
    where
        up' x = mapPitch' (+ x)

-- |
-- Read a Sibelius score from a file. Fails if the file could not be read or if a parsing
-- error occurs.
-- 
readSib :: IsSibelius a => FilePath -> IO (Score a)
readSib path = fmap (either (\x -> error $ "Could not read score " ++ x) id) $ readSibEither path

-- |
-- Read a Sibelius score from a file. Fails if the file could not be read, and returns
-- @Nothing@ if a parsing error occurs.
-- 
readSibMaybe :: IsSibelius a => FilePath -> IO (Maybe (Score a))
readSibMaybe path = fmap (either (const Nothing) Just) $ readSibEither path

-- |
-- Read a Sibelius score from a file. Fails if the file could not be read, and returns
-- @Left m@ if a parsing error occurs.
-- 
readSibEither :: IsSibelius a => FilePath -> IO (Either String (Score a))
readSibEither path = do
    json <- ByteString.readFile path
    return $ fmap fromSib $ eitherDecode' json

-- |
-- This constraint includes all note types that can be constructed from a Sibelius representation.
--
type IsSibelius a = (
    IsPitch a, 
    HasPart' a, 
    Enum (Part a), 
    HasPitch a, 
    Num (Pitch a), 
    HasTremolo a, 
    HasArticulation a,
    Tiable a
    )


-- Util

every :: (a -> b -> b) -> [a] -> b -> b
every f = flip (foldr f)

kTicksPerWholeNote = 1024 -- Always in Sibelius

