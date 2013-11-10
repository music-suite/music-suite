
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving,
    ScopedTypeVariables,
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
        toXmlString,
        writeXml,
        openXml,
        -- toXmlVoice,
        -- toXmlSingle,
        -- writeXmlSingle,
        -- openXmlSingle,
) where

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
import Data.Ratio
import Data.String
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Plus
import Control.Arrow
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Typeable
import Data.Traversable
import Data.Function (on)
import Data.Ord (comparing)
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Basis

import Music.Time
import Music.Time.Reactive (initial, (?))
import Music.Pitch.Literal
import Music.Dynamics.Literal
import Music.Score.Rhythm
import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Meta
import Music.Score.Chord
import Music.Score.Combinators
import Music.Score.Convert
import Music.Score.Pitch
import Music.Score.Ties
import Music.Score.Part
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Ornaments
import Music.Score.Instances
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
    getMusicXml      :: Duration -> a -> XmlMusic

    getMusicXmlChord :: Duration -> [a] -> XmlMusic
    getMusicXmlChord d = error "getMusicXmlChord: Not implemented"

instance HasMusicXml Int                        where   getMusicXml d = getMusicXml d . toInteger
instance HasMusicXml Float                      where   getMusicXml d = getMusicXml d . toInteger . round
instance HasMusicXml Double                     where   getMusicXml d = getMusicXml d . toInteger . round
instance Integral a => HasMusicXml (Ratio a)    where   getMusicXml d = getMusicXml d . toInteger . round

instance HasMusicXml Integer where
    getMusicXml      d = (`Xml.note` realToFrac d)  . spellXml . fromIntegral
    getMusicXmlChord d = (`Xml.chord` realToFrac d) . fmap (spellXml . fromIntegral)

instance HasMusicXml a => HasMusicXml (ChordT a) where
    getMusicXml d = getMusicXmlChord d . getChordT

instance HasMusicXml a => HasMusicXml (PartT n a) where
    getMusicXml d (PartT (_,x))                     = getMusicXml d x

instance HasMusicXml a => HasMusicXml (TieT a) where
    getMusicXml d (TieT (ta,x,tb))                  = addTies $ getMusicXml d x
        where
            addTies | ta && tb                      = Xml.endTie . Xml.beginTie
                    | tb                            = Xml.beginTie
                    | ta                            = Xml.endTie
                    | otherwise                     = id

instance HasMusicXml a => HasMusicXml (DynamicT a) where
    getMusicXml d (DynamicT (ec,ed,l,a,bc,bd))  = notate $ getMusicXml d a
        where
            notate x = nec <> ned <> nl <> nbc <> nbd <> x
            nec    = if ec then Xml.endCresc    else mempty
            ned    = if ed then Xml.endDim      else mempty
            nbc    = if bc then Xml.beginCresc  else mempty
            nbd    = if bd then Xml.beginDim    else mempty
            nl     = case l of
                Nothing  -> mempty
                Just lvl -> Xml.dynamic (fromDynamics (DynamicsL (Just lvl, Nothing)))

instance HasMusicXml a => HasMusicXml (ArticulationT a) where
    getMusicXml d (ArticulationT (es,us,al,sl,a,bs))    = notate $ getMusicXml d a
        where
            notate = nes . nal . nsl . nbs
            nes    = if es then Xml.endSlur else id
            nal    = case al of
                0    -> id
                1    -> Xml.accent
                2    -> Xml.strongAccent
            nsl    = case sl of
                (-2) -> Xml.tenuto
                (-1) -> Xml.tenuto . Xml.staccato
                0    -> id
                1    -> Xml.staccato
                2    -> Xml.staccatissimo
            nbs    = if bs then Xml.beginSlur else id

instance HasMusicXml a => HasMusicXml (TremoloT a) where
    getMusicXml d (TremoloT (n,x))      = notate $ getMusicXml d x
        where
            notate = case n of
                0 -> id
                _ -> Xml.tremolo n

instance HasMusicXml a => HasMusicXml (TextT a) where
    getMusicXml d (TextT (s,x))                     = notate s $ getMusicXml d x
        where
            notate ts a = mconcat (fmap Xml.text ts) <> a

instance HasMusicXml a => HasMusicXml (HarmonicT a) where
    getMusicXml d (HarmonicT (n,x))                 = notate $ getMusicXml d x
        where
            notate | n /= 0     = Xml.setNoteHead Xml.DiamondNoteHead
                   | otherwise  = id
    -- TODO adjust pitch etc

instance HasMusicXml a => HasMusicXml (SlideT a) where
    getMusicXml d (SlideT (eg,es,a,bg,bs))    = notate $ getMusicXml d a
        where
            notate = neg . nes . nbg . nbs
            neg    = if eg then Xml.endGliss else id
            nes    = if es then Xml.endSlide else id
            nbg    = if bg then Xml.beginGliss else id
            nbs    = if bs then Xml.beginSlide else id

-- |
-- Convert a score to MusicXML and write to a file.
--
writeXml :: (HasMusicXml a, HasPart' a, Show (Part a), Semigroup a) => FilePath -> Score a -> IO ()
writeXml path sc = writeFile path (Xml.showXml $ toXml sc)

-- |
-- Convert a score to MusicXML and open it.
--
openXml :: (HasMusicXml a, HasPart' a, Show (Part a), Semigroup a) => Score a -> IO ()
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
-- Convert a score to a MusicXML string.
--
toXmlString :: (HasMusicXml a, HasPart' a, Show (Part a), Semigroup a) => Score a -> String
toXmlString = Xml.showXml . toXml

-- |
-- Convert a score to a MusicXML representation.
--
toXml :: forall a . (HasMusicXml a, HasPart' a, Show (Part a), Semigroup a) => Score a -> XmlScore
toXml sc = Xml.fromParts title "Composer" pl . fmap (toXmlVoice' . scoreToVoice . simultaneous) . extractParts $ sc
    where                      
        title = fromMaybe "" $ flip titleAtLevel 0 $ (? 0) $ runMeta (Nothing :: Maybe a) $ getScoreMeta sc
        pl = Xml.partList (fmap show $ getParts sc)
        -- asScore a = (a :: Score a)

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
    where (a,b) = fromIntegral *** fromIntegral $ unRatio $ realToFrac m

noteRestToXml :: HasMusicXml a => Duration -> Maybe a -> Xml.Music
noteRestToXml d Nothing  = setDefaultVoice $ Xml.rest $ realToFrac d
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




-- TODO move
withTitle :: HasPart' a => (Title -> Score a -> Score a) -> Score a -> Score a
withTitle = withMeta


