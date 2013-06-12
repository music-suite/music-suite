
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving,
    FlexibleContexts,
    ConstraintKinds,
    TypeOperators,
    OverloadedStrings,
    NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Score.Export.MusicXml (
        XmlScore,
        XmlMusic,
        HasMusicXml(..),
        toXml,
        writeXml,
        openXml,
        toXmlVoice,
        toXmlSingle,
        writeXmlSingle,
        openXmlSingle,
) where

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
import Data.Ratio
import Data.String
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Plus
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Typeable
import Data.Traversable
import Data.Function (on)
import Data.Ord (comparing)
import Data.VectorSpace
import Data.AffineSpace
import Data.Basis

import Music.Time
import Music.Pitch.Literal
import Music.Dynamics.Literal
import Music.Score.Rhythm
import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Zip
import Music.Score.Pitch
import Music.Score.Ties
import Music.Score.Part
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Ornaments
import Music.Score.Export.Util

import qualified Codec.Midi as Midi
import qualified Music.MusicXml.Simple as Xml
import qualified Music.Lilypond as Lilypond
import qualified Text.Pretty as Pretty
import qualified Data.Map as Map
import qualified Data.List as List


type XmlScore = Xml.Score
type XmlMusic = Xml.Music

-- |
-- Class of types that can be converted to MusicXML.
--
class Tiable a => HasMusicXml a where
    -- |
    -- Convert a value to MusicXML.
    --
    -- Typically, generates a 'XmlMusic' value using 'Xml.note' or 'Xml.chord', and transforms it
    -- to add beams, slurs, dynamics, articulation etc.
    --
    getMusicXml :: Duration -> a -> XmlMusic

instance HasMusicXml Int                        where   getMusicXml d = getMusicXml d . toInteger
instance HasMusicXml Float                      where   getMusicXml d = getMusicXml d . toInteger . round
instance HasMusicXml Double                     where   getMusicXml d = getMusicXml d . toInteger . round
instance Integral a => HasMusicXml (Ratio a)    where   getMusicXml d = getMusicXml d . toInteger . round
-- instance HasMusicXml a => HasMusicXml (Maybe a) where   getMusicXml d = ?

instance HasMusicXml Integer where
    getMusicXml d p = Xml.note (spellXml (fromIntegral p)) . fromDuration $ d



-- |
-- Convert a score to MusicXML and write to a file.
--
writeXml :: (HasMusicXml a, HasPart' a, Show (Part a)) => FilePath -> Score a -> IO ()
writeXml path sc = writeFile path (Xml.showXml $ toXml sc)

-- |
-- Convert a score to MusicXML and open it.
--
openXml :: (HasMusicXml a, HasPart' a, Show (Part a)) => Score a -> IO ()
openXml sc = do
    writeXml "test.xml" sc
    execute "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]
    -- FIXME hardcoded

-- |
-- Convert a score to MusicXML and write to a file.
--
writeXmlSingle :: HasMusicXml a => FilePath -> Score a -> IO ()
writeXmlSingle path sc = writeFile path (Xml.showXml $ toXmlSingle sc)

-- |
-- Convert a score to MusicXML and open it.
--
openXmlSingle :: HasMusicXml a => Score a -> IO ()
openXmlSingle sc = do
    writeXmlSingle "test.xml" sc
    execute "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]
    -- FIXME hardcoded


-- |
-- Convert a score to a MusicXML representation.
--
toXml :: (HasMusicXml a, HasPart' a, Show (Part a)) => Score a -> XmlScore
toXml sc = Xml.fromParts "Title" "Composer" pl . fmap (toXmlVoice' . scoreToVoice) . extract $ sc
    where
        pl = Xml.partList (fmap show $ getParts sc)

-- |
-- Convert a single-voice score to a MusicXML representation.
--
toXmlSingle :: HasMusicXml a => Score a -> XmlScore
toXmlSingle = toXmlVoice . scoreToVoice

-- |
-- Convert a single-voice score to a MusicXML representation.
--
toXmlVoice :: HasMusicXml a => Voice (Maybe a) -> XmlScore
toXmlVoice = Xml.fromPart "Title" "Composer" "Voice" . toXmlVoice'

-- |
-- Convert a voice score to a list of bars.
--
toXmlVoice' :: HasMusicXml a => Voice (Maybe a) -> [XmlMusic]
toXmlVoice' =
    addDefaultSignatures . fmap barToXml . voiceToBars
    where
        addDefaultSignatures []     = []
        addDefaultSignatures (x:xs) = (defaultSignatures <> x):xs
        defaultSignatures = mempty
            <> Xml.defaultKey
            <> Xml.defaultDivisions
            <> Xml.metronome (1/4) 60
            <> Xml.commonTime


barToXml :: HasMusicXml a => [(Duration, Maybe a)] -> Xml.Music
barToXml bar = case quantize bar of
    Left e   -> error $ "barToXml: Could not quantize this bar: " ++ show e
    Right rh -> rhythmToXml rh

rhythmToXml :: HasMusicXml a => Rhythm (Maybe a) -> Xml.Music
rhythmToXml (Beat d x)            = noteRestToXml d x
rhythmToXml (Group rs)            = mconcat $ map rhythmToXml rs
rhythmToXml (Dotted n (Beat d x)) = noteRestToXml (dotMod n * d) x
rhythmToXml (Tuplet m r)          = Xml.tuplet b a (rhythmToXml r)
    where (a,b) = both fromIntegral fromIntegral $ unRatio $ fromDuration m

noteRestToXml :: HasMusicXml a => Duration -> Maybe a -> Xml.Music
noteRestToXml d Nothing  = setDefaultVoice $ Xml.rest $ fromDuration d
noteRestToXml d (Just p) = setDefaultVoice $ getMusicXml d p

-- FIXME only works for single-voice parts
setDefaultVoice :: Xml.Music -> Xml.Music
setDefaultVoice = Xml.setVoice 1

-- FIXME arbitrary spelling, please modularize...
spellXml :: Integer -> Xml.Pitch
spellXml p = (
    toEnum $ fromIntegral pc,
    if alt == 0 then Nothing else Just (fromIntegral alt),
    fromIntegral oct
    )
    where (pc,alt,oct) = spellPitch p






