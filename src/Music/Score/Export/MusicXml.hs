
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving,
    FlexibleContexts,
    ConstraintKinds,
    ViewPatterns,
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

        showXml,
        openXml,
        writeXml,
) where

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Arrow
import Data.Semigroup
import Data.Monoid.WithSemigroup
import Data.Ratio
import Data.String
import Data.Pointed
import Data.Maybe
import Data.Function (on)
import Data.Ord (comparing)
import System.Process

import Music.Time
import Music.Time.Reactive (Reactive, initial, (?))
import Music.Pitch.Literal
import Music.Dynamics.Literal
import Music.Score.Rhythm
import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Meta
import Music.Score.Meta.Clef
import Music.Score.Meta.Time
import Music.Score.Meta.Attribution
import Music.Score.Meta.Title
import Music.Score.Clef
import Music.Score.Chord
import Music.Score.Combinators
import Music.Score.Convert
import Music.Score.Convert
import Music.Score.Pitch
import Music.Score.Ties
import Music.Score.Part
import Music.Score.Util
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Ornaments
import Music.Score.Instances
import Music.Score.Export.Common

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

instance HasMusicXml a => HasMusicXml (ClefT a) where
    getMusicXml d (ClefT (c, a)) = notate $ getMusicXml d a
        where
            notate = case fmap getLast $ getOption c of
                Nothing -> id
                Just GClef -> (Xml.trebleClef <>)
                Just CClef -> (Xml.altoClef <>)
                Just FClef -> (Xml.bassClef <>)


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
    void $ rawSystem "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]

-- -- |
-- -- Convert a score to MusicXML and write to a file.
-- --
-- writeXmlSingle :: HasMusicXml a => FilePath -> Score a -> IO ()
-- writeXmlSingle path sc = writeFile path (Xml.showXml $ toXmlSingle sc)

-- -- |
-- -- Convert a score to MusicXML and open it.
-- --
-- openXmlSingle :: HasMusicXml a => Score a -> IO ()
-- openXmlSingle sc = do
--     writeXmlSingle "test.xml" sc
--     void $ rawSystem "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]

-- |
-- Convert a score to MusicXML and print it on the standard output.
--
showXml :: (HasMusicXml a, HasPart' a, Show (Part a), Semigroup a) => Score a -> IO ()
showXml = putStrLn . toXmlString

-- |
-- Convert a score to a MusicXML string.
--
toXmlString :: (HasMusicXml a, HasPart' a, Show (Part a), Semigroup a) => Score a -> String
toXmlString = Xml.showXml . toXml

-- -- |
-- -- Convert a single-voice score to a MusicXML representation.
-- --
-- toXmlSingle :: HasMusicXml a => Score a -> XmlScore
-- toXmlSingle = voiceToXml . scoreToVoice
-- 
-- -- |
-- -- Convert a single-voice score to a MusicXML representation.
-- --
-- voiceToXml :: HasMusicXml a => Voice (Maybe a) -> XmlScore
-- voiceToXml = Xml.fromPart "Title" "Composer" "Voice" . voiceToXml'

-- |
-- Convert a score to a MusicXML representation.
--
toXml :: (HasMusicXml a, HasPart' a, Show (Part a), Semigroup a) => Score a -> XmlScore
toXml sc = 
           -- Score structure
           Xml.fromParts title composer pl

                -- Main notation pipeline
                . fmap (voiceToXml' barTimeSigs barDurations . scoreToVoice . simultaneous 

                -- Meta-event expansion
                . addClefs
                )

        . extractParts $ sc

    where
        addClefT :: a -> ClefT a
        addClefT = point
        
        addClefs = setClef . fmap addClefT
        setClef  = withMeta $ \x -> applyClefOption (fmap getLast x)

        timeSigs = getTimeSignature (4/4) sc -- 4/4 is default
        barTimeSigs  = retainUpdates $ getBarTimeSignatures $ fmap swap $ getVoice $ reactiveToVoice (duration sc) timeSigs        
        barDurations = getBarDurations $ fmap swap $ getVoice $ reactiveToVoice (duration sc) timeSigs

        title    = fromMaybe "" $ flip getTitleAt 0              $ metaAtStart sc
        composer = fromMaybe "" $ flip getAttribution "composer" $ metaAtStart sc

        pl = Xml.partList (fmap show $ getParts sc)

mergeBars :: [XmlMusic] -> XmlMusic
mergeBars [x] = x
mergeBars _   = error "mergeBars: Not supported"

-- |
-- Convert a voice score to a list of bars.
--
voiceToXml' :: HasMusicXml a => [Maybe TimeSignature] -> [Duration] -> Voice (Maybe a) -> [XmlMusic]
voiceToXml' barTimeSigs barDurations = addStartInfo . zipWith setBarTimeSig barTimeSigs . fmap barToXml . voiceToBars' barDurations
-- TODO attach key signatures in each bar (basically zip)

--
-- This is where notation of a single voice takes place
--      * voiceToBars is generic for most notations outputs: it handles bar splitting and ties
--      * barToXml is specific: it handles quantization and notation
--
    where                          
        -- FIXME compounds                      
        setBarTimeSig Nothing x = x
        setBarTimeSig (Just (unTime -> (m:_, n))) x = Xml.time (fromInteger m) (fromInteger n) <> x

        addStartInfo []     = []
        addStartInfo (x:xs) = (startInfo <> x):xs
        startInfo = mempty
            <> Xml.defaultKey
            <> Xml.defaultDivisions
            <> Xml.metronome (1/4) 60
            -- <> Xml.commonTime
            -- TODO explicit time sig


barToXml :: HasMusicXml a => [(Duration, Maybe a)] -> XmlMusic
barToXml bar = case (fmap rewrite . quantize) bar of
    Left e   -> error $ "barToXml: Could not quantize this bar: " ++ show e
    Right rh -> rhythmToXml rh

rhythmToXml :: HasMusicXml a => Rhythm (Maybe a) -> XmlMusic
rhythmToXml (Beat d x)            = noteRestToXml d x
rhythmToXml (Group rs)            = mconcat $ map rhythmToXml rs
rhythmToXml (Dotted n (Beat d x)) = noteRestToXml (dotMod n * d) x
rhythmToXml (Tuplet m r)          = Xml.tuplet b a (rhythmToXml r)
    where (a,b) = fromIntegral *** fromIntegral $ unRatio $ realToFrac m

noteRestToXml :: HasMusicXml a => Duration -> Maybe a -> XmlMusic
noteRestToXml d Nothing  = setDefaultVoice $ Xml.rest $ realToFrac d
noteRestToXml d (Just p) = setDefaultVoice $ getMusicXml d p

setDefaultVoice :: XmlMusic -> XmlMusic
setDefaultVoice = Xml.setVoice 1

spellXml :: Integer -> Xml.Pitch
spellXml p = (
    toEnum $ fromIntegral pc,
    if alt == 0 then Nothing else Just (fromIntegral alt),
    fromIntegral oct
    )
    where (pc,alt,oct) = spellPitch p



swap (x,y) = (y,x)