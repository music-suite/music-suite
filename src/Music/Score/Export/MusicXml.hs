
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

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

        toMusicXml,
        toMusicXmlString,

        showMusicXml,
        openMusicXml,
        writeMusicXml,
) where

import           Prelude                      hiding (concat, concatMap, foldl,
                                               foldr, mapM, maximum, minimum,
                                               sum)

import           Control.Applicative
import           Control.Arrow
import           Control.Lens                 hiding (rewrite)
import           Control.Monad                hiding (mapM)
import           Data.Function                (on)
import           Data.Maybe
import           Data.Monoid.WithSemigroup
import           Data.Ord                     (comparing)
import           Data.Ratio
import           Data.Semigroup
import           Data.String
import           System.Process

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score.Articulation
-- import           Music.Score.Chord
import           Music.Score.Clef
import           Music.Score.Convert (reactiveToVoice')
import           Music.Score.Dynamics
import           Music.Score.Export.Common
import           Music.Score.Instances
import           Music.Score.Meta.Attribution
import           Music.Score.Meta
import           Music.Score.Meta.Clef
import           Music.Score.Meta.Time
import           Music.Score.Meta.Title
import           Music.Score.Ornaments
import           Music.Score.Part
import           Music.Score.Pitch
import           Music.Score.Rhythm
import           Music.Score.Ties
import           Music.Score.Util
import           Music.Time hiding (time)

import qualified Codec.Midi                   as Midi
import qualified Data.List                    as List
import qualified Data.Map                     as Map
import qualified Music.Lilypond               as Lilypond
import qualified Music.MusicXml.Simple        as Xml
import qualified Text.Pretty                  as Pretty


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
    getMusicXml      d = (`Xml.note` realToFrac d)  . spellMusicXml . fromIntegral
    getMusicXmlChord d = (`Xml.chord` realToFrac d) . fmap (spellMusicXml . fromIntegral)

instance HasMusicXml a => HasMusicXml [a] where
    getMusicXml d = getMusicXmlChord d

instance HasMusicXml a => HasMusicXml (PartT n a) where
    getMusicXml d (PartT (_,x))                     = getMusicXml d x

instance HasMusicXml a => HasMusicXml (TieT a) where
    getMusicXml d (TieT ((Any ta,Any tb),x)) = addTies $ getMusicXml d x
        where
            addTies | ta && tb                      = Xml.endTie . Xml.beginTie
                    | tb                            = Xml.beginTie
                    | ta                            = Xml.endTie
                    | otherwise                     = id

instance (Tiable d, HasMusicXml a) => HasMusicXml (DynamicT d a) where
  getMusicXml d (DynamicT (_,a)) = getMusicXml d a
--     getMusicXml d (DynamicT (((Any ec,Any ed),Option l,(Any bc,Any bd)), a)) = notate $ getMusicXml d a
--         where
--             notate x = nec <> ned <> nl <> nbc <> nbd <> x
--             nec    = if ec then Xml.endCresc    else mempty
--             ned    = if ed then Xml.endDim      else mempty
--             nbc    = if bc then Xml.beginCresc  else mempty
--             nbd    = if bd then Xml.beginDim    else mempty
--             nl     = case l of
--                 Nothing          -> mempty
--                 Just (First lvl) -> Xml.dynamic (fromDynamics (DynamicsL (Just lvl, Nothing)))
-- 

instance (Tiable d, HasMusicXml a) => HasMusicXml (ArticulationT d a) where
  getMusicXml d (ArticulationT (_,a)) = getMusicXml d a
-- instance HasMusicXml a => HasMusicXml (ArticulationT a) where
--     getMusicXml d (ArticulationT (((Any es, Any us, Any bs), (Sum al, Sum sl)), a)) = notate $ getMusicXml d a
--         where
--             notate = nes . nal . nsl . nbs
--             nes    = if es then Xml.endSlur else id
--             nal    = case al of
--                 0    -> id
--                 1    -> Xml.accent
--                 2    -> Xml.strongAccent
--             nsl    = case sl of
--                 (-2) -> Xml.tenuto
--                 (-1) -> Xml.tenuto . Xml.staccato
--                 0    -> id
--                 1    -> Xml.staccato
--                 2    -> Xml.staccatissimo
--             nbs    = if bs then Xml.beginSlur else id

instance HasMusicXml a => HasMusicXml (TremoloT a) where
    getMusicXml d (TremoloT (Max n, x))      = notate $ getMusicXml d x
        where
            notate = case n of
                0 -> id
                n -> Xml.tremolo (fromIntegral n)

instance HasMusicXml a => HasMusicXml (TextT a) where
    getMusicXml d (TextT (s,x))                     = notate s $ getMusicXml d x
        where
            notate ts a = mconcat (fmap Xml.text ts) <> a

instance HasMusicXml a => HasMusicXml (HarmonicT a) where
    getMusicXml d (HarmonicT ((view _Wrapped' -> isNat, view _Wrapped' -> n),x)) = notate isNat n $ getMusicXml d x
        where
            notate _     0 = id
            notate True  n = notateNatural n
            notate False n = notateArtificial n

            -- notateNatural n = Xml.harmonic -- openString?
            notateNatural n = Xml.setNoteHead Xml.DiamondNoteHead
            -- Most programs do not recognize the harmonic tag
            -- We set a single diamond notehead instead, which can be manually replaced
            notateArtificial n = id -- TODO

instance HasMusicXml a => HasMusicXml (SlideT a) where
    getMusicXml d (SlideT (((eg,es),(bg,bs)),a))    = notate $ getMusicXml d a
        where
            notate = neg . nes . nbg . nbs
            neg    = if view _Wrapped' eg then Xml.endGliss else id
            nes    = if view _Wrapped' es then Xml.endSlide else id
            nbg    = if view _Wrapped' bg then Xml.beginGliss else id
            nbs    = if view _Wrapped' bs then Xml.beginSlide else id

instance HasMusicXml a => HasMusicXml (ClefT a) where
    getMusicXml d (ClefT (c, a)) = notate $ getMusicXml d a
        where
            notate = case fmap getLast $ getOption c of
                Nothing -> id
                Just GClef -> (Xml.trebleClef <>)
                Just CClef -> (Xml.altoClef <>)
                Just FClef -> (Xml.bassClef <>)

instance HasMusicXml a => HasMusicXml (Behavior a) where
    getMusicXml d      = getMusicXml d . (! 0)
    getMusicXmlChord d = getMusicXmlChord d . fmap (! 0)


-- |
-- Convert a score to MusicXML and open it.
--
-- /Note/ This is simple wrapper around 'writeMusicXml' that may not work well on all platforms.
--
openMusicXml :: (HasMusicXml2 a, HasPart2 a, Semigroup a) => Score a -> IO ()
openMusicXml sc = do
    writeMusicXml "test.xml" sc
    -- FIXME find out which program to use etc...
    void $ rawSystem "open" ["-a", "Sibelius 7", "test.xml"]

-- |
-- Convert a score to MusicXML and write to a file.
--
writeMusicXml :: (HasMusicXml2 a, HasPart2 a, Semigroup a) => FilePath -> Score a -> IO ()
writeMusicXml path = writeFile path . toMusicXmlString

-- |
-- Convert a score to MusicXML and print it on the standard output.
--
showMusicXml :: (HasMusicXml2 a, HasPart2 a, Semigroup a) => Score a -> IO ()
showMusicXml = putStrLn . toMusicXmlString

-- |
-- Convert a score to a MusicXML string.
--
toMusicXmlString :: (HasMusicXml2 a, HasPart2 a, Semigroup a) => Score a -> String
toMusicXmlString = Xml.showXml . toMusicXml


-- |
-- Convert a score to a MusicXML representation.
--
toMusicXml :: (HasMusicXml2 a, HasPart2 a, Semigroup a) => Score a -> XmlScore
toMusicXml sc =
           -- Score structure
           Xml.fromParts title composer partList

                -- Main notation pipeline
                . fmap (
                mvoiceToMusicXml barTimeSigs barDurations . temporaryClefFix . toMVoice

                -- Meta-event expansion
                . addClefs
                )

        . extractParts $ sc

    where
        -- TODO temporary to make most tests pass
        temporaryClefFix = over (_Wrapped._head.traverse.traverse) $ applyClef GClef
        -- temporaryClefFix = id

        addClefT :: a -> ClefT a
        addClefT = return

        addClefs = setClef . fmap addClefT
        setClef  = withClef def $ \c x -> applyClef c x where def = GClef -- TODO use part default

        -- Despite mergeEqual above we need retainUpdates here to prevent redundant repetition of time signatures
        barTimeSigs  :: [Maybe TimeSignature]
        barDurations :: [Duration]
        barTimeSigs  = retainUpdates $ getBarTimeSignatures $ timeSigs
        barDurations =                 getBarDurations      $ timeSigs

        title, composer :: String
        title    = fromMaybe "" $ flip getTitleAt 0              $ metaAtStart sc
        composer = fromMaybe "" $ flip getAttribution "composer" $ metaAtStart sc

        timeSigs = fmap swap $ unvoice $ fuse 
          $ reactiveToVoice' (0 <-> _offset sc) $ getTimeSignatures def sc
          where
            def = 4/4

        partList :: Xml.PartList
        partList = Xml.partList (fmap show $ allParts sc)

-- |
-- Convert a voice score to a list of bars.
--
mvoiceToMusicXml :: HasMusicXml2 a => [Maybe TimeSignature] -> [Duration] -> Voice (Maybe a) -> [XmlMusic]
mvoiceToMusicXml barTimeSigs barDurations = addStartInfo . zipWith setBarTimeSig barTimeSigs . fmap barToMusicXml . voiceToBars' barDurations
-- TODO attach key signatures in each bar (basically zip)

--
-- This is where notation of a single voice takes place
--      * voiceToBars is generic for most notations outputs: it handles bar splitting and ties
--      * barToMusicXml is specific: it handles quantization and notation
--
    where
        setBarTimeSig :: Maybe TimeSignature -> Xml.Music -> Xml.Music
        setBarTimeSig Nothing x = x
        setBarTimeSig (Just (getTimeSignature -> (m:_, n))) x = Xml.time (fromInteger m) (fromInteger n) <> x
        -- TODO compounds

        addStartInfo :: [Xml.Music] -> [Xml.Music]
        addStartInfo []     = []
        addStartInfo (x:xs) = (startInfo <> x):xs
        
        startInfo :: Xml.Music
        startInfo = mempty
            <> Xml.defaultKey
            <> Xml.defaultDivisions
            <> Xml.metronome (1/4) 60
            -- <> Xml.commonTime


barToMusicXml :: HasMusicXml2 a => [(Duration, Maybe a)] -> XmlMusic
barToMusicXml bar = case (fmap rewrite . quantize) bar of
    Left e   -> error $ "barToMusicXml: Could not quantize this bar: " ++ show e
    Right rh -> rhythmToMusicXml rh

rhythmToMusicXml :: HasMusicXml2 a => Rhythm (Maybe a) -> XmlMusic
rhythmToMusicXml (Beat d x)            = noteRestToMusicXml d x
rhythmToMusicXml (Group rs)            = mconcat $ map rhythmToMusicXml rs
rhythmToMusicXml (Dotted n (Beat d x)) = noteRestToMusicXml (dotMod n * d) x
rhythmToMusicXml (Tuplet m r)          = Xml.tuplet b a (rhythmToMusicXml r)
    where (a,b) = fromIntegral *** fromIntegral $ unRatio $ realToFrac m

noteRestToMusicXml :: HasMusicXml2 a => Duration -> Maybe a -> XmlMusic
noteRestToMusicXml d Nothing  = setDefaultVoice $ Xml.rest $ realToFrac d
noteRestToMusicXml d (Just p) = setDefaultVoice $ getMusicXml d p

setDefaultVoice :: XmlMusic -> XmlMusic
setDefaultVoice = Xml.setVoice 1

spellMusicXml :: Integer -> Xml.Pitch
spellMusicXml p = (
    toEnum $ fromIntegral pc,
    if alt == 0 then Nothing else Just (fromIntegral alt),
    fromIntegral oct
    )
    where (pc,alt,oct) = spellPitch (p + 60)

-- TODO remove
type HasPart2 a = (HasPart' a, Ord (Part a), Show (Part a))
type HasMusicXml2 a = (HasMusicXml a, Transformable a)

{-
foo
  :: (Semigroup a, Transformable a, HasClef a, HasMusicXml a) =>
     [Maybe TimeSignature] 
      -> [Duration] 
      -> Score a 
      -> [XmlMusic]
foo barTimeSigs barDurations  = mvoiceToMusicXml barTimeSigs barDurations . temporaryClefFix . toMVoice
  where
    temporaryClefFix = over (_Wrapped._head.traverse.traverse) $ applyClef GClef
-}
