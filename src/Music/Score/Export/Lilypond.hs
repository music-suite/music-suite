                              
{-# LANGUAGE
    CPP,
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,     
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving,
    FlexibleInstances,
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

module Music.Score.Export.Lilypond -- (
--  ) 
where

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

import Control.Reactive
import Control.Reactive.Midi

import Music.Time.Absolute
import Music.Time.Relative
import Music.Score.Rhythm
import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score (Score, note, rest, perform)
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

import System.Posix
import System.IO.Unsafe
import Music.Pitch.Literal
import Music.Dynamics.Literal

type Lilypond = Lilypond.Music

-- |
-- Class of types that can be converted to Lilypond.
--
class Tiable a => HasLilypond a where          
    -- | 
    -- Convert a value to a Lilypond music expression.
    --
    getLilypond :: Duration -> a -> Lilypond

instance HasLilypond Int                        where   getLilypond d = getLilypond d . toInteger    
instance HasLilypond Float                      where   getLilypond d = getLilypond d . toInteger . round
instance HasLilypond Double                     where   getLilypond d = getLilypond d . toInteger . round
instance Integral a => HasLilypond (Ratio a)    where   getLilypond d = getLilypond d . toInteger . round    

instance HasLilypond Integer where
    getLilypond d p = Lilypond.note (spellLy $ p+12) ^*(fromDuration $ d*4)

-- TODO rename                            
pcatLy :: [Lilypond.Music] -> Lilypond.Music
pcatLy = foldr Lilypond.pcat (Lilypond.Simultaneous False [])

scatLy :: [Lilypond.Music] -> Lilypond.Music
scatLy = foldr Lilypond.scat (Lilypond.Sequential [])


-- |
-- Convert a score to MusicXML and write to a file. 
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
-- Convert a score to MusicXML and open it. 
-- 
openLy :: (HasLilypond a, HasPart' a, Show (Part a)) => Score a -> IO ()
openLy sc = do
    writeLy "test.ly" sc                       
    runLy
    
runLy = execute "lilypond" ["-f", "png", "test.ly"]
    -- FIXME hardcoded

-- |
-- Convert a score to a Lilypond representation. 
-- 
toLy :: (HasLilypond a, HasPart' a, Show (Part a)) => Score a -> Lilypond.Music
toLy sc = pcatLy . fmap (addStaff . scatLy . prependName . second toLyVoice' . second scoreToVoice) . extractWithNames $ sc
    where                           
        addStaff x = Lilypond.New "Staff" Nothing x
        prependName (v,x) = [Lilypond.Set "Staff.instrumentName" (Lilypond.toValue $ show v)] ++ x

-- |
-- Convert a voice score to a list of bars. 
-- 
toLyVoice' :: HasLilypond a => Voice (Maybe a) -> [Lilypond.Music]
toLyVoice' = fmap barToLy . voiceToBars
                    
barToLy :: HasLilypond a => [(Duration, Maybe a)] -> Lilypond.Music
barToLy bar = case quantize bar of
    Left e   -> error $ "barToLy: Could not quantize this bar: " ++ show e
    Right rh -> rhythmToLy rh

rhythmToLy :: HasLilypond a => Rhythm (Maybe a) -> Lilypond.Music
rhythmToLy (Beat d x)            = noteRestToLy d x
rhythmToLy (Group rs)            = foldr Lilypond.scat (Lilypond.Sequential []) $ map rhythmToLy rs
rhythmToLy (Dotted n (Beat d x)) = noteRestToLy (dotMod n * d) x
rhythmToLy (Tuplet m r)          = Lilypond.Times (fromDuration m) (rhythmToLy r)
    where (a,b) = both fromIntegral fromIntegral $ unRatio $ getDuration m

noteRestToLy :: HasLilypond a => Duration -> Maybe a -> Lilypond.Music
noteRestToLy d Nothing  = Lilypond.rest^*(fromDuration $ d*4)   
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

