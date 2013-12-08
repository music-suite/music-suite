
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    GeneralizedNewtypeDeriving #-}

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

module Music.Score.Export.Common (
        voiceToBars,
        voiceToBars',
        -- separateBars,
        spellPitch,
        toRelative,
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

import Music.Time
import Music.Score.Rhythm
import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Convert
import Music.Score.Combinators
import Music.Score.Pitch
import Music.Score.Ties
import Music.Score.Part
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Ornaments

import qualified Codec.Midi as Midi
import qualified Music.MusicXml.Simple as Xml
import qualified Music.Lilypond as Lilypond
import qualified Text.Pretty as Pretty
import qualified Data.Map as Map
import qualified Data.List as List

import System.Process
import System.IO.Unsafe
import Music.Score.Util
import Music.Pitch.Literal
import Music.Dynamics.Literal

-- |
-- Convert a single-voice score to a list of bars.
--
voiceToBars :: Tiable a => Voice (Maybe a) -> [[(Duration, Maybe a)]]
voiceToBars = voiceToBars' (repeat 1)

voiceToBars' :: Tiable a => [Duration] -> Voice (Maybe a) -> [[(Duration, Maybe a)]]
voiceToBars' barDurs = fmap getVoice . splitTiesVoiceAt barDurs

-- voiceToBars = separateBars . splitTiesVoice

-- -- |
-- -- Given a set of absolute-time occurences, separate at each zero-time occurence.
-- -- Note that this require every bar to start with a zero-time occurence.
-- --
-- separateBars :: Voice (Maybe a) -> [[(Duration, Maybe a)]]
-- separateBars =
--     fmap (removeTime . fmap discardBarNumber) .
--         splitAtTimeZero . fmap separateTime . perform . voiceToScore
--     where
--         separateTime (t,d,x)            = ((bn,bt),d,x) where (bn,bt) = properFraction (t .-. origin)
--         splitAtTimeZero                 = splitWhile ((== 0) . getBarTime) where getBarTime ((bn,bt),_,_) = bt
--         discardBarNumber ((bn,bt),d,x)  = (realToFrac bt / kDur, d, x)
--         removeTime                      = fmap g where g (t,d,x) = (d,x)
--         kDur = 1      

-- |
-- Convert absolute to relative durations.
--
toRelative :: [(Time, Duration, b)] -> [(Time, Duration, b)]
toRelative = snd . mapAccumL g origin
    where
        g now (t,d,x) = (t, (origin .+^ (t .-. now),d,x))


-- | 
-- Basic spelling for integral types.
-- 
spellPitch :: Integral a => a -> (a, a, a)
spellPitch p = (
    pitchClass,
    alteration,
    octave
    )
    where
        octave     = (p `div` 12) - 1
        semitone   = p `mod` 12
        pitchClass = fromStep major semitone
        alteration = semitone - step major pitchClass

        step xs p = xs !! (fromIntegral p `mod` length xs)
        fromStep xs p = fromIntegral $ fromMaybe (length xs - 1) $ List.findIndex (>= p) xs
        scaleFromSteps = snd . List.mapAccumL add 0
            where
                add a x = (a + x, a + x)
        major = scaleFromSteps [0,2,2,1,2,2,2,1]

