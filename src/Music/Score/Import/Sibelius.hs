
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
import Data.Music.Sibelius
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
    timeSig $ pcat $ fmap (\staff -> set parts' (partFromSibeliusStaff staff) (fromSibeliusStaff barDur staff)) $ staves
    -- TODO meta information
        where
            -- FIXME only reads TS in first bar
            barDur = case head (getSibeliusTimeSignatures systemStaff) of
              Nothing -> 1
              Just ts -> barDuration ts
            timeSig = case head (getSibeliusTimeSignatures systemStaff) of
              Nothing -> id
              Just ts -> timeSignature ts
            
            partFromSibeliusStaff = either (error . ("Unknown instrument: " ++)) id . partFromSibeliusStaff'
            
            partFromSibeliusStaff' :: SibeliusStaff -> Either String Part
            partFromSibeliusStaff' (SibeliusStaff bars name shortName) = partFromName (name, shortName)

            -- TODO something more robust (in part library...)
            partFromName ("Piccolo",_)                            = Right $ piccoloFlutes
            partFromName ("Piccolo Flute",_)                      = Right $ piccoloFlutes
            partFromName ("Flute",_)                              = Right $ flutes
            partFromName ("Flutes (a)",_)                         = Right $ (!! 0) $ divide 4 $ flutes
            partFromName ("Flutes (b)",_)                         = Right $ (!! 1) $ divide 4 $ flutes
            partFromName ("Flutes (c)",_)                         = Right $ (!! 2) $ divide 4 $ flutes
            partFromName ("Flutes (d)",_)                         = Right $ (!! 3) $ divide 4 $ flutes
            partFromName ("Flute (a)",_)                          = Right $ (!! 0) $ divide 4 $ flutes
            partFromName ("Flute (b)",_)                          = Right $ (!! 1) $ divide 4 $ flutes
            partFromName ("Flute (c)",_)                          = Right $ (!! 2) $ divide 4 $ flutes
            partFromName ("Flute (d)",_)                          = Right $ (!! 3) $ divide 4 $ flutes
            partFromName ("Flute I",_)                            = Right $ (!! 0) $ divide 4 $ flutes
            partFromName ("Flute II",_)                           = Right $ (!! 1) $ divide 4 $ flutes
            partFromName ("Flute III",_)                          = Right $ (!! 2) $ divide 4 $ flutes
            partFromName ("Flute IV",_)                           = Right $ (!! 3) $ divide 4 $ flutes
            partFromName ("Alto Flute",_)                         = Right $ tutti altoFlute
            
            partFromName ("Oboe",_)                               = Right $ oboes
            partFromName ("Oboes (a)",_)                          = Right $ (!! 0) $ divide 4 $ oboes
            partFromName ("Oboes (b)",_)                          = Right $ (!! 1) $ divide 4 $ oboes
            partFromName ("Oboes (c)",_)                          = Right $ (!! 2) $ divide 4 $ oboes
            partFromName ("Oboes (d)",_)                          = Right $ (!! 3) $ divide 4 $ oboes
            partFromName ("Oboe (a)",_)                          = Right $ (!! 0) $ divide 4 $ oboes
            partFromName ("Oboe (b)",_)                          = Right $ (!! 1) $ divide 4 $ oboes
            partFromName ("Oboe (c)",_)                          = Right $ (!! 2) $ divide 4 $ oboes
            partFromName ("Oboe (d)",_)                          = Right $ (!! 3) $ divide 4 $ oboes
            partFromName ("Oboe I",_)                            = Right $ (!! 0) $ divide 4 $ oboes
            partFromName ("Oboe II",_)                           = Right $ (!! 1) $ divide 4 $ oboes
            partFromName ("Oboe III",_)                          = Right $ (!! 2) $ divide 4 $ oboes
            partFromName ("Oboe IV",_)                           = Right $ (!! 3) $ divide 4 $ oboes
            partFromName ("Cor Anglais",_)                        = Right $ tutti corAnglais
            
            partFromName ("Clarinet",_)                           = Right $ clarinets
            partFromName ("Clarinet in Bb",_)                     = Right $ clarinets
            partFromName ("Clarinet in A",_)                      = Right $ clarinets
            partFromName ("Clarinets",_)                          = Right $ clarinets
            partFromName ("Clarinets in Bb",_)                    = Right $ clarinets
            partFromName ("Clarinets in Bb (a)",_)                = Right $ (!! 0) $ divide 3 clarinets
            partFromName ("Clarinets in Bb (b)",_)                = Right $ (!! 1) $ divide 3 clarinets
            partFromName ("Clarinets in Bb (c)",_)                = Right $ (!! 2) $ divide 3 clarinets
            partFromName ("Clarinet in Bb I",_)                   = Right $ (!! 0) $ divide 3 clarinets
            partFromName ("Clarinet in Bb II",_)                  = Right $ (!! 1) $ divide 3 clarinets
            partFromName ("Clarinet in Bb III",_)                 = Right $ (!! 2) $ divide 3 clarinets
            partFromName ("Clarinet in Bb IV",_)                  = Right $ (!! 3) $ divide 3 clarinets
            partFromName ("Clarinets in Bb I",_)                   = Right $ (!! 0) $ divide 3 clarinets
            partFromName ("Clarinets in Bb II",_)                  = Right $ (!! 1) $ divide 3 clarinets
            partFromName ("Clarinets in Bb III",_)                 = Right $ (!! 2) $ divide 3 clarinets
            partFromName ("Clarinets in Bb IV",_)                  = Right $ (!! 3) $ divide 3 clarinets
            partFromName ("Bass Clarinet in Bb",_)                = Right $ (!! 0) $ divide 3 clarinets
            
            partFromName ("Bassoon",_)                            = Right $ bassoons
            partFromName ("Bassoon (a)",_)                        = Right $ (!! 0) $ divide 4 bassoons
            partFromName ("Bassoon (b)",_)                        = Right $ (!! 1) $ divide 4 bassoons
            partFromName ("Bassoon (c)",_)                        = Right $ (!! 2) $ divide 4 bassoons
            partFromName ("Bassoon (d)",_)                        = Right $ (!! 3) $ divide 4 bassoons
            partFromName ("Bassoon I",_)                          = Right $ (!! 0) $ divide 4 bassoons
            partFromName ("Bassoon II",_)                         = Right $ (!! 1) $ divide 4 bassoons
            partFromName ("Bassoon III",_)                        = Right $ (!! 2) $ divide 4 bassoons
            partFromName ("Bassoon IV",_)                         = Right $ (!! 3) $ divide 4 bassoons
            partFromName ("Horn",_)                               = Right $ horns
            partFromName ("Horn (a)",_)                           = Right $ (!! 0) $ divide 4 $ horns
            partFromName ("Horn (b)",_)                           = Right $ (!! 1) $ divide 4 $ horns
            partFromName ("Horn (c)",_)                           = Right $ (!! 2) $ divide 4 $ horns
            partFromName ("Horn (d)",_)                           = Right $ (!! 3) $ divide 4 $ horns

            partFromName ("Horns",_)                              = Right $ horns
            partFromName ("Horns (a)",_)                          = Right $ (!! 0) $ divide 4 $ horns
            partFromName ("Horns (b)",_)                          = Right $ (!! 1) $ divide 4 $ horns
            partFromName ("Horns (c)",_)                          = Right $ (!! 2) $ divide 4 $ horns
            partFromName ("Horns (d)",_)                          = Right $ (!! 3) $ divide 4 $ horns
            partFromName ("Horns in F",_)                         = Right $ horns
            partFromName ("Horns in F (a)",_)                     = Right $ (!! 0) $ divide 4 $ horns
            partFromName ("Horns in F (b)",_)                     = Right $ (!! 1) $ divide 4 $ horns
            partFromName ("Horns in F (c)",_)                     = Right $ (!! 2) $ divide 4 $ horns
            partFromName ("Horns in F (d)",_)                     = Right $ (!! 3) $ divide 4 $ horns
            partFromName ("Horn in F (a)",_)                      = Right $ (!! 0) $ divide 4 $ horns
            partFromName ("Horn in F (b)",_)                      = Right $ (!! 1) $ divide 4 $ horns
            partFromName ("Horn in F (c)",_)                      = Right $ (!! 2) $ divide 4 $ horns
            partFromName ("Horn in F (d)",_)                      = Right $ (!! 3) $ divide 4 $ horns
            partFromName ("Horn in F",_)                          = Right $ horns
            partFromName ("Horn in E",_)                          = Right $ horns
            
            partFromName ("Trumpet (a)",_)                        = Right $ (!! 0) $ divide 4 $ trumpets
            partFromName ("Trumpet (b)",_)                        = Right $ (!! 1) $ divide 4 $ trumpets
            partFromName ("Trumpet (c)",_)                        = Right $ (!! 2) $ divide 4 $ trumpets
            partFromName ("Trumpet (d)",_)                        = Right $ (!! 3) $ divide 4 $ trumpets
            partFromName ("Trumpet in C (a)",_)                   = Right $ (!! 0) $ divide 4 $ trumpets
            partFromName ("Trumpet in C (b)",_)                   = Right $ (!! 1) $ divide 4 $ trumpets
            partFromName ("Trumpet in C (c)",_)                   = Right $ (!! 2) $ divide 4 $ trumpets
            partFromName ("Trumpet in C (d)",_)                   = Right $ (!! 3) $ divide 4 $ trumpets
            partFromName ('T':'r':'u':'m':'p':'e':'t':_,_)        = Right $ trumpets
            partFromName ("Trombone",_)                           = Right $ trombones

            partFromName ("Trombone (a)",_)                       = Right $ (!! 0) $ divide 4 $ trombones
            partFromName ("Trombone (b)",_)                       = Right $ (!! 1) $ divide 4 $ trombones
            partFromName ("Trombone (c)",_)                       = Right $ (!! 2) $ divide 4 $ trombones
            partFromName ("Trombone (d)",_)                       = Right $ (!! 3) $ divide 4 $ trombones
            partFromName ("Trombones",_)                          = Right $ trombones

            partFromName ("Tuba",_)                               = Right $ (!! 0) $ divide 4 $ trombones

            partFromName ("Timpani",_)                            = Right $ tutti timpani
            partFromName ("Percussion I (a)",_)                   = Right $ (!! 1) $ divide 3 $ tutti piano
            partFromName ("Percussion I (b)",_)                   = Right $ (!! 2) $ divide 3 $ tutti piano

            partFromName ("Crotales",_)                           = Right $ tutti $ fromMusicXmlSoundId "metal.crotales"
            partFromName ("Tubular Bells",_)                      = Right $ tutti $ tubularBells
            partFromName ("Vibraphone",_)                         = Right $ tutti $ vibraphone

            partFromName ("Harp",_)                               = Right $ harp
            partFromName ("Harp (a)",_)                           = Right $ (!! 0) $ divide 2 harp
            partFromName ("Harp (b)",_)                           = Right $ (!! 1) $ divide 2 harp

            partFromName ("Strings (a)",_)                        = Right $ (!! 0) $ divide 8 violins
            partFromName ("Strings (b)",_)                        = Right $ (!! 0) $ divide 8 cellos
            partFromName ("Strings (c)",_)                        = Right $ (!! 1) $ divide 8 violins
            partFromName ("Strings (d)",_)                        = Right $ (!! 1) $ divide 8 cellos
            partFromName ("Strings (e)",_)                        = Right $ (!! 2) $ divide 8 violins
            partFromName ("Strings (f)",_)                        = Right $ (!! 2) $ divide 8 cellos
            partFromName ("Strings (g)",_)                        = Right $ (!! 3) $ divide 8 violins
            partFromName ("Strings (h)",_)                        = Right $ (!! 3) $ divide 8 cellos
            partFromName ("Strings (i)",_)                        = Right $ (!! 4) $ divide 8 violins
            partFromName ("Strings (j)",_)                        = Right $ (!! 4) $ divide 8 cellos
            partFromName ("Strings (k)",_)                        = Right $ (!! 5) $ divide 8 violins
            partFromName ("Strings (l)",_)                        = Right $ (!! 5) $ divide 8 cellos
            partFromName ("Strings (m)",_)                        = Right $ (!! 6) $ divide 8 violins
            partFromName ("Strings (n)",_)                        = Right $ (!! 6) $ divide 8 cellos
            partFromName ("Strings (o)",_)                        = Right $ (!! 7) $ divide 8 violins
            partFromName ("Strings (p)",_)                        = Right $ (!! 7) $ divide 8 cellos
            -- partFromName ("Strings (q)",_) = Right $ (!! 0)    $ divide 2 violins
            
            partFromName ("Violin",_)                             = Right $ violins

            partFromName ("Violin I",_)                           = Right $ violins1
            partFromName ("Violin I (a)",_)                       = Right $ (!! 0) $ divide 20 violins1
            partFromName ("Violin I (b)",_)                       = Right $ (!! 1) $ divide 20 violins1
            partFromName ("Violin I (c)",_)                       = Right $ (!! 2) $ divide 20 violins1
            partFromName ("Violin I (d)",_)                       = Right $ (!! 3) $ divide 20 violins1
            partFromName ("Violin I (e)",_)                       = Right $ (!! 4) $ divide 20 violins1
            partFromName ("Violin I (f)",_)                       = Right $ (!! 5) $ divide 20 violins1
            partFromName ("Violin I (g)",_)                       = Right $ (!! 6) $ divide 20 violins1
            partFromName ("Violin I (h)",_)                       = Right $ (!! 7) $ divide 20 violins1
            partFromName ("Violin I (i)",_)                       = Right $ (!! 8) $ divide 20 violins1
            partFromName ("Violin I (j)",_)                       = Right $ (!! 10) $ divide 20 violins1
            partFromName ("Violin I (k)",_)                       = Right $ (!! 11) $ divide 20 violins1
            partFromName ("Violin I (l)",_)                       = Right $ (!! 12) $ divide 20 violins1
            partFromName ("Violin I (m)",_)                       = Right $ (!! 13) $ divide 20 violins1
            partFromName ("Violin I (n)",_)                       = Right $ (!! 14) $ divide 20 violins1
            partFromName ("Violin I (o)",_)                       = Right $ (!! 15) $ divide 20 violins1
            partFromName ("Violin I (p)",_)                       = Right $ (!! 16) $ divide 20 violins1
            partFromName ("Violin II",_)                          = Right $ violins2
            partFromName ("Violin II (a)",_)                      = Right $ (!! 1) $ divide 20 violins2
            partFromName ("Violin II (b)",_)                      = Right $ (!! 2) $ divide 20 violins2
            partFromName ("Violin II (c)",_)                      = Right $ (!! 3) $ divide 20 violins2
            partFromName ("Violin II (d)",_)                      = Right $ (!! 4) $ divide 20 violins2
            partFromName ("Violin II (e)",_)                      = Right $ (!! 5) $ divide 20 violins2
            partFromName ("Violin II (f)",_)                      = Right $ (!! 6) $ divide 20 violins2
            partFromName ("Violin II (g)",_)                      = Right $ (!! 7) $ divide 20 violins2
            partFromName ("Violin II (h)",_)                      = Right $ (!! 8) $ divide 20 violins2
            partFromName ("Violin II (i)",_)                      = Right $ (!! 9) $ divide 20 violins2
            partFromName ("Violin II (j)",_)                      = Right $ (!! 10) $ divide 20 violins2
            partFromName ("Violin II (k)",_)                      = Right $ (!! 11) $ divide 20 violins2
            partFromName ("Violin II (l)",_)                      = Right $ (!! 12) $ divide 20 violins2
            partFromName ("Violin II (m)",_)                      = Right $ (!! 13) $ divide 20 violins2
            partFromName ("Violin II (n)",_)                      = Right $ (!! 14) $ divide 20 violins2
            partFromName ("Viola",_)                              = Right $ violas
            partFromName ("Viola (a)",_)                          = Right $ (!! 1) $ divide 20 violas
            partFromName ("Viola (b)",_)                          = Right $ (!! 2) $ divide 20 violas
            partFromName ("Viola (c)",_)                          = Right $ (!! 3) $ divide 20 violas
            partFromName ("Viola (d)",_)                          = Right $ (!! 4) $ divide 20 violas
            partFromName ("Viola (e)",_)                          = Right $ (!! 5) $ divide 20 violas
            partFromName ("Viola (f)",_)                          = Right $ (!! 6) $ divide 20 violas
            partFromName ("Viola (g)",_)                          = Right $ (!! 7) $ divide 20 violas
            partFromName ("Viola (h)",_)                          = Right $ (!! 8) $ divide 20 violas
            partFromName ("Viola (i)",_)                          = Right $ (!! 9) $ divide 20 violas
            partFromName ("Viola (j)",_)                          = Right $ (!! 10) $ divide 20 violas
            partFromName ("Violoncello",_)                        = Right $ cellos
            partFromName ("Violoncello (a)",_)                    = Right $ (!! 1) $ divide 20 cellos
            partFromName ("Violoncello (b)",_)                    = Right $ (!! 2) $ divide 20 cellos
            partFromName ("Violoncello (c)",_)                    = Right $ (!! 3) $ divide 20 cellos
            partFromName ("Violoncello (d)",_)                    = Right $ (!! 4) $ divide 20 cellos
            partFromName ("Violoncello (e)",_)                    = Right $ (!! 5) $ divide 20 cellos
            partFromName ("Violoncello (f)",_)                    = Right $ (!! 6) $ divide 20 cellos
            partFromName ("Violoncello (g)",_)                    = Right $ (!! 7) $ divide 20 cellos
            partFromName ("Violoncello (h)",_)                    = Right $ (!! 8) $ divide 20 cellos
            partFromName ("Contrabass",_)                         = Right $ doubleBasses
            partFromName ("Contrabass (a)",_)                     = Right $ (!! 1) $ divide 20 doubleBasses
            partFromName ("Contrabass (b)",_)                     = Right $ (!! 2) $ divide 20 doubleBasses
            partFromName ("Contrabass (c)",_)                     = Right $ (!! 3) $ divide 20 doubleBasses
            partFromName ("Contrabass (d)",_)                     = Right $ (!! 4) $ divide 20 doubleBasses
            partFromName ("Contrabass soli (a)",_)                     = Right $ (!! 1) $ divide 20 $ solo doubleBass
            partFromName ("Contrabass soli (b)",_)                     = Right $ (!! 2) $ divide 20 $ solo doubleBass
            partFromName ("Contrabass soli (c)",_)                     = Right $ (!! 3) $ divide 20 $ solo doubleBass
            partFromName ("Contrabass soli (d)",_)                     = Right $ (!! 4) $ divide 20 $ solo doubleBass
            partFromName ("Contrabass (e)",_)                     = Right $ (!! 5) $ divide 20 doubleBasses
            partFromName ("Contrabass (f)",_)                     = Right $ (!! 6) $ divide 20 doubleBasses


            partFromName ("Double Bass",_)                        = Right $ doubleBasses
            partFromName ("Piano",_)                              = Right $ tutti piano
            partFromName ("Piano (a)",_)                          = Right $ tutti piano
            partFromName ("Piano (b)",_)                          = Right $ tutti piano

            partFromName ("Soprano",_)                            = Right $ violins1
            partFromName ("Mezzo-Soprano",_)                      = Right $ violins2
            partFromName ("Mezzo-soprano",_)                      = Right $ violins2
            partFromName ("Alto",_)                               = Right $ violas
            partFromName ("Tenor",_)                              = Right $ (!! 0) $ divide 2 cellos
            partFromName ("Baritone",_)                           = Right $ (!! 1) $ divide 2 cellos
            partFromName ("Bass",_)                               = Right $ doubleBasses

            partFromName ("S",_)                                  = Right $ violins1
            partFromName ("Mez",_)                                = Right $ violins2
            partFromName ("A",_)                                  = Right $ violas
            partFromName ("T",_)                                  = Right $ (!! 0) $ divide 2 cellos
            partFromName ("Bar",_)                                = Right $ (!! 1) $ divide 2 cellos
            partFromName ("B",_)                                  = Right $ doubleBasses
            partFromName ("S.",_)                                 = Right $ violins1
            partFromName ("Mez.",_)                               = Right $ violins2
            partFromName ("A.",_)                                 = Right $ violas
            partFromName ("T.",_)                                 = Right $ (!! 0) $ divide 2 cellos
            partFromName ("Bar.",_)                               = Right $ (!! 1) $ divide 2 cellos
            partFromName ("B.",_)                                 = Right $ doubleBasses

            partFromName (n,_)                                    = Left n

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
    fmap Just (pcat $ fmap fromSibeliusChordElem chords) <> stretch d rest
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

        setArt _ = id
        -- setArt :: SibeliusArticulation -> Score a -> Score a
        -- setArt Marcato         = marcato
        -- setArt Accent          = accent
        -- setArt Tenuto          = tenuto
        -- setArt Staccato        = staccato
        -- setArt Harmonic        = harmonic 0
        -- setArt a               = error $ "fromSibeliusChord: Unsupported articulation " ++ show a        
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
    
    HasHarmonic a,
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
