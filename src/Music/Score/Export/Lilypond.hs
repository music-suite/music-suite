
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving,
    FlexibleContexts,
    UndecidableInstances, -- for Clef
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
        toLyString,
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
import Data.Pointed
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
import Music.Score.Meta
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
    getLilypond      :: Duration -> a -> Lilypond

    getLilypondChord :: Duration -> [a] -> Lilypond
    getLilypondChord d = pcatLy . fmap (getLilypond d)

instance HasLilypond Int                        where   getLilypond d = getLilypond d . toInteger
instance HasLilypond Float                      where   getLilypond d = getLilypond d . toInteger . round
instance HasLilypond Double                     where   getLilypond d = getLilypond d . toInteger . round
instance Integral a => HasLilypond (Ratio a)    where   getLilypond d = getLilypond d . toInteger . round

instance HasLilypond Integer where
    getLilypond      d = (^*realToFrac (d*4)) . Lilypond.note  . spellLy . (+ 12)
    getLilypondChord d = (^*realToFrac (d*4)) . Lilypond.chord . fmap (spellLy . (+ 12))

instance HasLilypond a => HasLilypond (ChordT a) where
    getLilypond d = getLilypondChord d . getChordT

instance HasLilypond a => HasLilypond (PartT n a) where
    getLilypond d (PartT (_,x))                     = getLilypond d x

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
    getLilypond d (TremoloT (0, x)) = getLilypond d x
    getLilypond d (TremoloT (n, x)) = notate $ getLilypond newDur x
        where            
            scale   = 2^n                    
            newDur  = (d `min` (1/4)) / scale
            repeats = d / newDur                                
            notate = Lilypond.Tremolo (round repeats)

instance HasLilypond a => HasLilypond (TextT a) where
    getLilypond d (TextT (s,x)) = notate s $ getLilypond d x
        where
            notate ts = foldr (.) id (fmap Lilypond.addText ts)

instance HasLilypond a => HasLilypond (HarmonicT a) where
    getLilypond d (HarmonicT (n,x)) = notate $ getLilypond d x
        where
            notate = id
            -- FIXME

instance HasLilypond a => HasLilypond (SlideT a) where
    getLilypond d (SlideT (eg,es,a,bg,bs)) = notate $ getLilypond d a
        where          
            notate = if bg || bs then Lilypond.beginGlissando else id


-- TODO move
-- Put the given clef in front of the note
newtype ClefT a = ClefT { getClefT :: (Option (Last Clef), a) }
    deriving (Functor, Semigroup, Monoid)

instance HasPart a => HasPart (ClefT a) where
    type Part (ClefT a) = Part a
    getPart (ClefT (_,a)) = getPart a
    modifyPart f (ClefT (a,b)) = ClefT (a, modifyPart f b)

instance Pointed ClefT where
    point x = ClefT (mempty, x)

instance Tiable a => Tiable (ClefT a) where
    beginTie = fmap beginTie
    endTie = fmap endTie

instance HasLilypond a => HasLilypond (ClefT a) where
    getLilypond d (ClefT (c, a)) = notate $ getLilypond d a
        where
            notate = case fmap getLast $ getOption c of
                Nothing -> id
                Just GClef -> \x -> Lilypond.Sequential [Lilypond.Clef Lilypond.Treble, x]
                Just CClef -> \x -> Lilypond.Sequential [Lilypond.Clef Lilypond.Alto, x]
                Just FClef -> \x -> Lilypond.Sequential [Lilypond.Clef Lilypond.Bass, x]

addClefT :: a -> ClefT a
addClefT = point

-- TODO
kDefClef = GClef

class HasClef a where
    applyClef :: Clef -> a -> a
    applyClefOption :: Option Clef -> a -> a
    applyClefOption c = case getOption c of
        Nothing -> applyClef kDefClef
        Just c  -> applyClef c
    applyClefMaybe :: Maybe Clef -> a -> a
    applyClefMaybe c = case c of
        Nothing -> applyClef kDefClef
        Just c  -> applyClef c
instance HasClef (ClefT a) where
    applyClef c (ClefT (_,a)) = ClefT (Option $ Just $ Last c,a)
instance HasClef a => HasClef (b,a) where
    applyClef c = fmap (applyClef c)
instance (HasPart' a, HasClef a) => HasClef (Score a) where
    applyClef c = mapFirst (applyClef c) id


pcatLy :: [Lilypond] -> Lilypond
pcatLy = pcatLy' False

pcatLy' :: Bool -> [Lilypond] -> Lilypond
pcatLy' p = foldr Lilypond.simultaneous e
    where
        e = Lilypond.Simultaneous p []

scatLy :: [Lilypond] -> Lilypond
scatLy = foldr Lilypond.sequential e
    where
        e = Lilypond.Sequential []

-- |
-- Convert a score to a Lilypond representation and write to a file.
--
writeLy :: (HasLilypond a, HasPart' a, Show (Part a), Semigroup a) => FilePath -> Score a -> IO ()
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
openLy :: (HasLilypond a, HasPart' a, Show (Part a), Semigroup a) => Score a -> IO ()
openLy sc = do
    writeLy "test.ly" sc
    runLy
    cleanLy
    openLy'

runLy   = void $ runCommand "lilypond -f pdf test.ly" >>= waitForProcess
cleanLy = void $ runCommand "rm -f test-*.tex test-*.texi test-*.count test-*.eps test-*.pdf test.eps"
openLy' = void $ runCommand "open test.pdf"
    -- FIXME hardcoded

-- |
-- Convert a score to a Lilypond string.
--
toLyString :: (HasLilypond a, HasPart' a, Show (Part a), Semigroup a) => Score a -> String
toLyString = show . Pretty.pretty . toLy

-- |
-- Convert a score to a Lilypond representation.
--
toLy :: (HasLilypond a, HasPart' a, Show (Part a), Semigroup a) => Score a -> Lilypond
toLy sc = pcatLy . fmap (addStaff . scatLy . prependName . second (toLyVoice' . scoreToVoice . addClefs . simultaneous)) . extractParts' $ sc
    where                 
        -- FIXME somthing to the left of addClef is killing the meta-events                                                                            
        -- I think simultaneous is guilty
        -- No withMeta is killing it *itself*
        addClefs = setCl . fmap addClefT

        -- Option (Last Clef)
        -- TODO replace Nothing with default clef
        setCl = withMeta $ \x -> applyClefOption (fmap getLast x)

        addStaff = Lilypond.New "Staff" Nothing
        prependName (v,x) = Lilypond.Set "Staff.instrumentName" (Lilypond.toValue $ show v) : x

-- |
-- Convert a voice score to a list of bars.
--
toLyVoice' :: HasLilypond a => Voice (Maybe a) -> [Lilypond]
toLyVoice' = fmap barToLy . voiceToBars

barToLy :: HasLilypond a => [(Duration, Maybe a)] -> Lilypond
barToLy bar = case quantize bar of
    Left e   -> error $ "barToLy: Could not quantize this bar: " ++ show e
    Right rh -> rhythmToLy rh

rhythmToLy :: HasLilypond a => Rhythm (Maybe a) -> Lilypond
rhythmToLy (Beat d x)            = noteRestToLy d x
rhythmToLy (Group rs)            = scatLy $ map rhythmToLy rs
rhythmToLy (Dotted n (Beat d x)) = noteRestToLy (dotMod n * d) x
rhythmToLy (Tuplet m r)          = Lilypond.Times (realToFrac m) (rhythmToLy r)
    where (a,b) = fromIntegral *** fromIntegral $ unRatio $ realToFrac m

noteRestToLy :: HasLilypond a => Duration -> Maybe a -> Lilypond
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

