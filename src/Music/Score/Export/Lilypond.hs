
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

module Music.Score.Export.Lilypond (
        Lilypond,
        HasLilypond(..),
        toLy,
        writeLy,
        openLy,
        -- toLySingle,
        -- writeLySingle,
        -- openLySingle,
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
import Data.AffineSpace.Point
import Data.Basis
import System.Process

import Music.Time
import Music.Pitch.Literal
import Music.Dynamics.Literal
import Music.Score.Rhythm
import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Convert
import Music.Score.Chord
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


type Lilypond = Lilypond.Music

-- |
-- Class of types that can be converted to Lilypond.
--
class Tiable a => HasLilypond a where
    -- |
    -- Convert a value to a Lilypond music expression.
    --
    getLilypond :: DurationT -> a -> Lilypond

instance HasLilypond Int                        where   getLilypond d = getLilypond d . toInteger
instance HasLilypond Float                      where   getLilypond d = getLilypond d . toInteger . round
instance HasLilypond Double                     where   getLilypond d = getLilypond d . toInteger . round
instance Integral a => HasLilypond (Ratio a)    where   getLilypond d = getLilypond d . toInteger . round

instance HasLilypond Integer where
    getLilypond d p = Lilypond.note (spellLy $ p+12) ^*(realToFrac d*4)

instance HasLilypond a => HasLilypond (PartT n a) where
    getLilypond d (PartT (_,x))                     = getLilypond d x

-- FIXME should use Lilypond chord notation
instance HasLilypond a => HasLilypond (ChordT a) where
    getLilypond d = pcatLy . fmap (getLilypond d) . getChordT

instance HasLilypond a => HasLilypond (TieT a) where
    getLilypond d (TieT (ta,x,tb))                  = addTies $ getLilypond d x
        where
            addTies | ta && tb                      = id . Lilypond.beginTie
                    | tb                            = Lilypond.beginTie
                    | ta                            = id
                    | otherwise                     = id

instance HasLilypond a => HasLilypond (DynamicT a) where
    getLilypond d (DynamicT (ec,ed,l,a,bc,bd))  = notate $ getLilypond d a
        where
            notate x = nec . ned . nl . nbc . nbd $ x
            nec    = if ec then Lilypond.endCresc    else id
            ned    = if ed then Lilypond.endDim      else id
            nbc    = if bc then Lilypond.beginCresc  else id
            nbd    = if bd then Lilypond.beginDim    else id
            nl     = case l of
                Nothing  -> id
                Just lvl -> Lilypond.addDynamics (fromDynamics (DynamicsL (Just lvl, Nothing)))

instance HasLilypond a => HasLilypond (ArticulationT a) where
    getLilypond d (ArticulationT (es,us,al,sl,a,bs))    = notate $ getLilypond d a
        where
            notate = nes . nal . nsl . nbs
            nes    = if es then Lilypond.endSlur else id
            nal    = case al of
                0    -> id
                1    -> Lilypond.addAccent
                2    -> Lilypond.addMarcato
            nsl    = case sl of
                (-2) -> Lilypond.addTenuto
                (-1) -> Lilypond.addPortato
                0    -> id
                1    -> Lilypond.addStaccato
                2    -> Lilypond.addStaccatissimo
            nbs    = if bs then Lilypond.beginSlur else id

instance HasLilypond a => HasLilypond (TremoloT a) where
    getLilypond d (TremoloT (n,x)) = notate $ getLilypond newDur x
        where            
            --           
            scale   = 2^n                    
            newDur  = (d `min` (1/4)) / scale
            repeats = d / newDur                                
            -- d / newDur == repeats
            notate = case n of
                0 -> id
                _ -> Lilypond.Tremolo (round repeats)
                -- FIXME wrong number?

-- n  scale     d        newDur repeats
-- 1  2         (1/16)   (1/32)     2     
-- 1  2         (1/8)    (1/16)     2     
-- 1  2         (1/4)    (1/8)      2
-- 1  2         (1/2)    (1/8)      4
-- 1  2         2        (1/8)      8



-- n  scale     d        newDur repeats
-- 2  4         (1/8)    (1/32)     4     
-- 2  4         (1/4)    (1/16)     4
-- 2  4         (1/2)    (1/16)     8
-- 2  4         2        (1/16)     8



instance HasLilypond a => HasLilypond (TextT a) where
    getLilypond d (TextT (s,x)) = notate s $ getLilypond d x
        where
            notate ts = foldr (.) id (fmap Lilypond.addText ts)

instance HasLilypond a => HasLilypond (HarmonicT a) where
    getLilypond d (HarmonicT (n,x))                 = notate $ getLilypond d x
        where
            notate = id
            -- FIXME

instance HasLilypond a => HasLilypond (SlideT a) where
    getLilypond d (SlideT (eg,es,a,bg,bs))    = notate $ getLilypond d a
        where
            notate = id
            -- FIXME




pcatLy :: [Lilypond] -> Lilypond
pcatLy = foldr Lilypond.simultaneous e
    where
        e = Lilypond.Simultaneous False []

scatLy :: [Lilypond] -> Lilypond
scatLy = foldr Lilypond.sequential e
    where
        e = Lilypond.Sequential []


-- |
-- Convert a score to a Lilypond representation and write to a file.
--
writeLy :: (HasLilypond a, HasPart' a, Show (Part a)) => FilePath -> Score a -> IO ()
writeLy path sc = writeFile path ((header ++) $ show $ Pretty.pretty $ toLy sc)
    where
        header = mempty                                                ++
            "\\include \"lilypond-book-preamble.ly\"\n"                ++
            "\\paper {\n"                                              ++
            "  #(define dump-extents #t)\n"                            ++
            "\n"                                                       ++
            "  indent = 0\\mm\n"                                       ++
            "  line-width = 210\\mm - 2.0 * 0.4\\in\n"                 ++
            "  ragged-right = ##t\n"                                   ++
            "  force-assignment = #\"\"\n"                             ++
            "  line-width = #(- line-width (* mm  3.000000))\n"        ++
            "}\n"                                                      ++
            "\\layout {\n"                                             ++
            "}\n"

-- |
-- Typeset a score using Lilypond and open it.
--
openLy :: (HasLilypond a, HasPart' a, Show (Part a)) => Score a -> IO ()
openLy sc = do
    writeLy "test.ly" sc
    runLy
    cleanLy
    openLy'

runLy   = runCommand "lilypond -f pdf test.ly" >>= waitForProcess >> return ()
cleanLy = runCommand "rm -f test-*.tex test-*.texi test-*.count test-*.eps test-*.pdf test.eps"
openLy' = runCommand "open test.pdf" >> return ()
    -- FIXME hardcoded

-- |
-- Convert a score to a Lilypond representation.
--
toLy :: (HasLilypond a, HasPart' a, Show (Part a)) => Score a -> Lilypond
toLy sc = pcatLy . fmap (addStaff . scatLy . prependName . second toLyVoice' . second scoreToVoice) . extractParts' $ sc
    where
        addStaff x = Lilypond.New "Staff" Nothing x
        prependName (v,x) = [Lilypond.Set "Staff.instrumentName" (Lilypond.toValue $ show v)] ++ x

-- |
-- Convert a voice score to a list of bars.
--
toLyVoice' :: HasLilypond a => Voice (Maybe a) -> [Lilypond]
toLyVoice' = fmap barToLy . voiceToBars

barToLy :: HasLilypond a => [(DurationT, Maybe a)] -> Lilypond
barToLy bar = case quantize bar of
    Left e   -> error $ "barToLy: Could not quantize this bar: " ++ show e
    Right rh -> rhythmToLy rh

rhythmToLy :: HasLilypond a => Rhythm (Maybe a) -> Lilypond
rhythmToLy (Beat d x)            = noteRestToLy d x
rhythmToLy (Group rs)            = scatLy $ map rhythmToLy rs
rhythmToLy (Dotted n (Beat d x)) = noteRestToLy (dotMod n * d) x
rhythmToLy (Tuplet m r)          = Lilypond.Times (m) (rhythmToLy r)
    where (a,b) = both fromIntegral fromIntegral $ unRatio $ m

noteRestToLy :: HasLilypond a => DurationT -> Maybe a -> Lilypond
noteRestToLy d Nothing  = Lilypond.rest^*(realToFrac d*4)
noteRestToLy d (Just p) = getLilypond d p

spellLy :: Integer -> Lilypond.Note
spellLy a = Lilypond.NotePitch (spellLy' a) Nothing

spellLy' :: Integer -> Lilypond.Pitch
spellLy' p = Lilypond.Pitch (
    toEnum $ fromIntegral pc,
    fromIntegral alt,
    fromIntegral oct
    )
    where (pc,alt,oct) = spellPitch p

