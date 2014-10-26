
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides miscellaneous conversions.
--
-------------------------------------------------------------------------------------

module Music.Time.Internal.Convert (
        voiceToScore,
        voiceToScore',
        scoreToVoice,
        reactiveToVoice',
  ) where

import           Control.Applicative
import           Control.Lens hiding (transform, time)
import           Control.Monad
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Foldable          (Foldable (..))
import           Data.Ord
import           Data.Ratio
import           Data.Semigroup
import           Data.String
import           Data.Traversable
import           Data.VectorSpace

import           Music.Time

import qualified Data.Foldable          as Foldable
import qualified Data.List              as List


reactiveToVoice' :: Span -> Reactive a -> Voice a
reactiveToVoice' (view range -> (u,v)) r = (^. voice) $ fmap (^. note) $ durs `zip` (fmap (r `atTime`) times)
    where
        times = 0 : filter (\t -> u < t && t < v) (occs r)
        durs  = toRelativeTimeN' v times
{-# DEPRECATED reactiveToVoice' "" #-}

-- |
-- Convert a score to a voice. Fails if the score contain overlapping events.
--
scoreToVoice :: Transformable a => Score a -> Voice (Maybe a)
scoreToVoice = (^. voice) . fmap (^. note) . fmap throwTime . addRests . (^. triples)
    where
       throwTime (t,d,x) = (d,x)
       addRests = concat . snd . mapAccumL g 0
           where
               g u (t, d, x)
                   | u == t    = (t .+^ d, [(t, d, Just x)])
                   | u <  t    = (t .+^ d, [(u, t .-. u, Nothing), (t, d, Just x)])
                   | otherwise = error "scoreToVoice: Strange prevTime"
{-# DEPRECATED scoreToVoice "" #-}

-- |
-- Convert a voice to a score.
--
voiceToScore :: Voice a -> Score a
voiceToScore = scat . fmap g . (^. notes) where g = (^. notee) . fmap return
{-# DEPRECATED voiceToScore "" #-}

{-
-- | Join voices in a given part into a score.
voicesToScore :: HasPart a => [(Part a, Voice a)] -> Score a
voicesToScore = pcat . fmap (voiceToScore . uncurry (\n -> fmap (setPart n)))
-}

-- |
-- Convert a voice which may contain rests to a score.
--
voiceToScore' :: Voice (Maybe a) -> Score a
voiceToScore' = mcatMaybes . voiceToScore
{-# DEPRECATED voiceToScore' "" #-}

