
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

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
        voiceToBars',
        -- separateBars,
        spellPitch,
        toRelative,
        MVoice,
        toMVoice,
        unvoice,
        openCommand
          ) where

import           Prelude                  hiding (concat, concatMap, foldl,
                                           foldr, mapM, maximum, minimum, sum)

import           Control.Applicative
import           Control.Lens
import           Control.Monad            hiding (mapM)
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Basis
import           Data.Either
import           Data.Foldable
import           Data.Function            (on)
import           Data.Maybe
import           Data.Ord                 (comparing)
import           Data.Ratio
import           Data.Semigroup
import           Data.String
import           Data.Traversable
import           Data.Typeable
import           Data.VectorSpace

import           Music.Score.Articulation
import           Music.Score.Convert      (reactiveToVoice', scoreToVoice)
import           Music.Score.Dynamics
import           Music.Score.Part
import           Music.Score.Pitch
import           Music.Score.Rhythm
import           Music.Score.Ties
import           Music.Time

import qualified Codec.Midi               as Midi
import qualified Data.List                as List
import qualified Data.Map                 as Map
import qualified Music.Lilypond           as Lilypond
import qualified Music.MusicXml.Simple    as Xml
import qualified System.Info              as Info
import qualified Text.Pretty              as Pretty

import           Control.Exception
import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score.Util
import           System.IO.Unsafe
import           System.Process

-- | Convert a voice to a list of bars using the given bar durations.
voiceToBars' :: Tiable a => [Duration] -> Voice (Maybe a) -> [[(Duration, Maybe a)]]
voiceToBars' barDurs = fmap (map (^. from stretched) . (^. stretcheds)) . splitTiesVoiceAt barDurs
-- TODO remove prime from name

-- | Convert absolute to relative durations.
toRelative :: [(Time, Duration, b)] -> [(Time, Duration, b)]
toRelative = snd . mapAccumL g 0
    where
        g now (t,d,x) = (t, (0 .+^ (t .-. now),d,x))

-- | Basic spelling for integral types.
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


type MVoice a = Voice (Maybe a)

toMVoice :: (Semigroup a, Transformable a) => Score a -> MVoice a
toMVoice = scoreToVoice . simultaneous

unvoice :: Voice b -> [(Duration, b)]
unvoice = toListOf (stretcheds . traverse . from stretched)
-- unvoice = fmap (^. from stretched) . (^. stretcheds)
{-# DEPRECATED unvoice "Use 'unsafeEventsV'" #-}


openCommand :: String
openCommand = case Info.os of
  "darwin" -> "open"
  "linux"  -> "xdg-open"

{-
-- TODO any version and/or OS
hasMuseScore = do
  result <- try (readProcess "ls" ["/Applications/MuseScore.app"] "")
  return $ case result of
    Left e   -> (e::SomeException) `assumed` False
    Right _ ->  True

hasSibelius = do
  result <- try (readProcess "ls" ["/Applications/Sibelius 7.app"] "")
  return $ case result of
    Left e   -> (e::SomeException) `assumed` False
    Right _ ->  True


assumed = flip const
-}
