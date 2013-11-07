
{-# LANGUAGE
    TypeFamilies,
    ViewPatterns,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    GeneralizedNewtypeDeriving,
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
-- Provides conversion betwen 'Score', 'Voice' and 'Track'.
--
-------------------------------------------------------------------------------------


module Music.Score.Convert (
        -- * Conversion
        noteToVoice,
        noteToScore,
        voiceToScore,
        voicesToScore,
        trackToScore,
        scoreToVoice,
  ) where

import Control.Monad
import Control.Monad.Plus
import Data.Semigroup
import Data.String
import Data.Foldable (Foldable(..))
import Data.Traversable
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Ratio
import Data.Pointed
import Data.Ord

import Music.Score.Note
import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Part
import Music.Time

import qualified Data.List as List
import qualified Data.Foldable as Foldable


--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

-- | Convert a note to a voice.
noteToVoice :: Note a -> Voice a
noteToVoice (unnote -> (s,x)) = stretchTo (duration s) $ return x

-- | Convert a note to a score.
noteToScore :: Note a -> Score a
noteToScore (unnote -> (s,x)) = s `sapp` return x

-- |
-- Convert a score to a voice. Fails if the score contain overlapping events.
--
scoreToVoice :: Score a -> Voice (Maybe a)
scoreToVoice = voice . fmap throwTime . addRests . perform
    where
       throwTime (t,d,x) = (d,x)
       addRests = concat . snd . mapAccumL g origin
           where
               g u (t, d, x)
                   | u == t    = (t .+^ d, [(t, d, Just x)])
                   | u <  t    = (t .+^ d, [(u, t .-. u, Nothing), (t, d, Just x)])
                   | otherwise = error "addRests: Strange prevTime"
       

-- |
-- Convert a voice to a score.
--
voiceToScore :: Voice a -> Score a
voiceToScore = scat . fmap g . getVoice
    where
        g (d,x) = stretch d (return x)

-- | Join voices in a given part into a score.
voicesToScore :: HasPart a => [(Part a, Voice a)] -> Score a
voicesToScore = pcat . fmap (voiceToScore . uncurry setParts)

-- |
-- Convert a voice which may contain rests to a score.
--
voiceToScore' :: Voice (Maybe a) -> Score a
voiceToScore' = mcatMaybes . voiceToScore

-- |
-- Convert a track to a score where each event is given a fixed duration.
--
trackToScore :: Duration -> Track a -> Score a
trackToScore d = compose . fmap (\(t,x) -> (t,d,x)) . getTrack

