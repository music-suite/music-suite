
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

module Music.Score.Export.Util -- (
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

import Music.Time
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

-- |
-- Convert a single-voice score to a list of bars.
--
voiceToBars :: Tiable a => Voice (Maybe a) -> [[(DurationT, Maybe a)]]
voiceToBars = separateBars . splitTiesVoice

-- |
-- Given a set of absolute-time occurences, separate at each zero-time occurence.
-- Note that this require every bar to start with a zero-time occurence.
--
separateBars :: Voice (Maybe a) -> [[(DurationT, Maybe a)]]
separateBars =
    fmap (removeTime . fmap discardBarNumber) .
        splitAtTimeZero . fmap separateTime . perform . voiceToScore
    where
        separateTime (t,d,x)            = ((bn,bt),d,x) where (bn,bt) = properFraction (toRational t)
        splitAtTimeZero                 = splitWhile ((== 0) . getBarTime) where getBarTime ((bn,bt),_,_) = bt
        discardBarNumber ((bn,bt),d,x)  = (fromRational bt, d, x)
        removeTime                      = fmap g where g (t,d,x) = (d,x)

-- |
-- Convert absolute to relative durations.
--
toRelative :: [(TimeT, DurationT, b)] -> [(TimeT, DurationT, b)]
toRelative = snd . mapAccumL g 0
    where
        g now (t,d,x) = (t, (t-now,d,x))


-- FIXME arbitrary spelling, please modularize...
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

        step xs p = xs !! ((fromIntegral p) `mod` length xs)
        fromStep xs p = fromIntegral $ fromMaybe (length xs - 1) $ List.findIndex (>= p) xs
        scaleFromSteps = snd . List.mapAccumL add 0
            where
                add a x = (a + x, a + x)
        major = scaleFromSteps [0,2,2,1,2,2,2,1]


-- |
-- Group a list into sublists whereever a predicate holds. The matched element
-- is the first in the sublist.
--
-- > splitWhile isSpace "foo bar baz"
-- >    ===> ["foo"," bar"," baz"]
-- >
-- > splitWhile (> 3) [1,5,4,7,0,1,2]
-- >    ===> [[1],[5],[4],[7,0,1,2]]
--
splitWhile :: (a -> Bool) -> [a] -> [[a]]
splitWhile p xs = case splitWhile' p xs of
    []:xss -> xss
    xss    -> xss
    where
        splitWhile' p []     = [[]]
        splitWhile' p (x:xs) = case splitWhile' p xs of
            (xs:xss) -> if p x then []:(x:xs):xss else (x:xs):xss

execute :: FilePath -> [String] -> IO ()
execute program args = do
    forkProcess $ executeFile program True args Nothing
    return ()

unRatio x = (numerator x, denominator x)
first f (x, y) = (f x, y)
second f (x, y) = (x, f y)
both f g = first f . second g
