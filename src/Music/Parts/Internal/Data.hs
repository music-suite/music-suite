
{-# LANGUAGE TypeSynonymInstances #-}  -- because clef
{-# LANGUAGE FlexibleInstances #-}  -- because clef
{-# LANGUAGE CPP #-}

module Music.Parts.Internal.Data where

import           Data.Map                        (Map)
import Control.Monad.Plus
#ifndef GHCI
#define GET_DATA_FILE Paths_music_parts.getDataFileName
import qualified Paths_music_parts
#else
#define GET_DATA_FILE (return . ("../music-parts/"++))
#endif

import Control.Applicative
import           Control.Lens                    (toListOf, (^.))
import qualified System.IO.Unsafe
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import Data.Csv (FromField(..), FromRecord(..), (.!))
import qualified Data.Csv
import           Data.Traversable                (traverse)
import Data.VectorSpace
import Data.AffineSpace
import qualified Data.List

import Music.Pitch.Clef
import Music.Pitch.Ambitus
import Music.Pitch






type SoundId = String

#ifndef GHCI
instance Num Clef where
  fromInteger 0 = trebleClef
  fromInteger 1 = altoClef
  fromInteger 2 = bassClef
#endif

data InstrumentTopCategory
  = Woodwind
  | Brass
  | Keyboard
  | Fretted
  | Percussion
  | Vocal
  | Strings
  | Other
  deriving (Show)


data InstrumentDef = InstrumentDef {

    _soundId                   :: SoundId,     -- ID
    _generalMidiProgram        :: [Int], -- GM Program
    _generalMidiPercussionNote :: [Int], -- GM Percussion Note
    _defaultMidiChannel        :: Maybe Int,   -- Default MIDI Channel
    _scoreOrder                :: Double, -- Score Order


    _allowedClefs              :: [Clef], -- Allowed Clefs
    _standardClef              :: [Clef], -- Standard Clef (1 elem for single staff, more otherwise, never empty)

    _transposition             :: Interval, -- Transposition
    _playableRange             :: Maybe (Ambitus Pitch), -- Playable Range
    _comfortableRange          :: Maybe (Ambitus Pitch), -- Comfortable Range
    _longName                  :: Maybe String,
    _shortName                 :: Maybe String,
    _sibeliusName              :: Maybe String
    }
    deriving (Show)


getInstrumentDefById :: String -> Maybe InstrumentDef
getInstrumentDefById a = Data.List.find (\x -> _soundId x == a) defs
  where
    -- Safe as this file never change
    defs = System.IO.Unsafe.unsafePerformIO getInstrumentData
getInstrumentDefByGeneralMidiProgram :: Int -> Maybe InstrumentDef
getInstrumentDefByGeneralMidiProgram a = Data.List.find (\x -> a `elem` _generalMidiProgram x) defs
  where
    -- Safe as this file never change
    defs = System.IO.Unsafe.unsafePerformIO getInstrumentData
getInstrumentDefByGeneralMidiPercussionNote :: Int -> Maybe InstrumentDef
getInstrumentDefByGeneralMidiPercussionNote a = Data.List.find (\x -> a `elem` _generalMidiPercussionNote x) defs
  where
    -- Safe as this file never change
    defs = System.IO.Unsafe.unsafePerformIO getInstrumentData



-- TODO move
pitchFromSPN :: String -> Maybe Pitch
pitchFromSPN x = fmap (\on -> (.+^ _P8^*(on-4))) (safeRead octS) <*> pc pcS
  where
    pc "C" = Just c
    pc "D" = Just d
    pc "E" = Just e
    pc "F" = Just f
    pc "G" = Just g
    pc "A" = Just a
    pc "B" = Just b

    pc "Cs" = Just cs
    pc "Ds" = Just ds
    pc "Es" = Just es
    pc "Fs" = Just fs
    pc "Gs" = Just gs
    pc "As" = Just as
    pc "Bs" = Just bs

    pc "Cb" = Just cb
    pc "Db" = Just db
    pc "Eb" = Just eb
    pc "Fb" = Just fb
    pc "Gb" = Just gb
    pc "Ab" = Just ab
    pc "Bb" = Just bb
    pc _ = Nothing
    pcS = init x
    octS = pure $ last x

safeRead x = Just (read x) -- TODO catch exception

readClef :: String -> Maybe Clef
readClef = go where
  -- go "french" = Just trebleClef
  go "treble" = Just trebleClef
  go "sop"    = Just sopranoClef
  go "mez"    = Just mezzoSopranoClef
  go "alto"   = Just altoClef
  go "ten"    = Just tenorClef
  go "bar"    = Just baritoneClef
  go "bass"   = Just bassClef
  go "perc"   = Just percClef
  go _        = Nothing
  
  percClef = (PercClef, 0, 0) -- TODO move

{-
Can't stop these instances from being reexported (https://www.haskell.org/onlinereport/modules.html)
so they are transitively exported by all modules depending on the Suite!

Drats!
-}
instance FromField [Int] where
  parseField v = fmap (mcatMaybes . map safeRead) $ fmap (splitBy ',') $ parseField v
instance FromField Pitch where
  parseField v = mcatMaybes $ fmap pitchFromSPN $ parseField v
instance FromField (Maybe (Ambitus Pitch)) where
  parseField v = fmap (listToAmbitus . mcatMaybes . map pitchFromSPN) $ fmap (splitBy ',') $ parseField v
    where
      listToAmbitus [a,b] = Just $ (a,b)^.ambitus
      listToAmbitus _     = Nothing
instance FromField Clef where
  parseField v = mcatMaybes $ fmap readClef $ parseField v
instance FromField [Clef] where
  parseField v = fmap (mcatMaybes . map readClef) $ fmap (splitBy ',') $ parseField v

instance FromRecord InstrumentDef where
  parseRecord v = InstrumentDef
    <$> v .! 0
    <*> v .! 1
    <*> v .! 2
    <*> v .! 3
    <*> v .! 4

    <*> v .! 5
    <*> v .! 6
    
    <*> fmap (.-.(c::Pitch)) (v .! 7) -- sounding - written, i.e. -P8 for double bass
    <*> v .! 8
    <*> v .! 9
    <*> v .! 10
    <*> v .! 11
    <*> v .! 12


{-
Don't edit data files!
Original here
  https://docs.google.com/spreadsheets/d/1I7lCGd8u4ggqqa_ATMVb87V10Vc8J8TP9w-vXu0M18o/edit#gid=0
-}
getInstrumentData' :: IO [Map String String]
getInstrumentData' = do
  fp <- GET_DATA_FILE "data/instruments.csv"
  d <- Data.ByteString.Lazy.readFile fp
  return $ case Data.Csv.decodeByName d of
    Left e -> error $ "Could not read data/instruments.csv "++show e
    Right (_header, x) -> toListOf traverse x

getInstrumentData :: IO [InstrumentDef]
getInstrumentData = do
  fp <- GET_DATA_FILE "data/instruments.csv"
  d <- Data.ByteString.Lazy.readFile fp
  return $ case Data.Csv.decode Data.Csv.HasHeader d of
    Left e -> error $ "Could not read data/instruments.csv "++show e
    Right (x) -> toListOf traverse x






-- TODO move
splitBy1 :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy x xs = splitBy1 x xs
splitBy1 delimiter = foldr f [[]] 
            where f c l@(x:xs) | c == delimiter = []:l
                             | otherwise = (c:x):xs
                             