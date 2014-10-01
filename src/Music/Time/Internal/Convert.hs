
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


{-
-- | Convert a note to an _onset and a voice.
noteToVoice :: Note a -> (Time, Voice a)
noteToVoice (view (from note) -> (s,x)) = (_onset s, stretchTo (_duration s) $ return x)
-}

{-
-- | Convert a note to a score.
noteToScore :: Note a -> Score a
noteToScore (view (from note) -> (s,x)) = s `transform` return x
-}

-- scoreToNotes :: Score a -> [Note a]
-- scoreToNotes = Foldable.toList . reifyScore

-- notesToScore :: [Note a] -> Score a
-- notesToScore = pcat . fmap noteToScore

{-
reactiveToVoice :: Duration -> Reactive a -> Voice a
reactiveToVoice d r = (^. voice) $ fmap (^. stretched) $ durs `zip` (fmap (r `atTime`) times)
    where
        times = 0 : filter (\t -> 0 < t && t < 0 .+^ d) (occs r)
        durs  = toRelativeTimeN' (0 .+^ d) times
-}

reactiveToVoice' :: Span -> Reactive a -> Voice a
reactiveToVoice' (view range -> (u,v)) r = (^. voice) $ fmap (^. stretched) $ durs `zip` (fmap (r `atTime`) times)
    where
        times = 0 : filter (\t -> u < t && t < v) (occs r)
        durs  = toRelativeTimeN' v times
{-# DEPRECATED reactiveToVoice' "" #-}

-- |
-- Convert a score to a voice. Fails if the score contain overlapping events.
--
scoreToVoice :: Transformable a => Score a -> Voice (Maybe a)
scoreToVoice = (^. voice) . fmap (^. stretched) . fmap throwTime . addRests . (^. events)
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
voiceToScore = scat . fmap g . (^. stretcheds) where g = (^. stretchee) . fmap return
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

{-
-- |
-- Convert a track to a score where each event is given a fixed duration.
--
trackToScore :: Transformable a => Duration -> Track a -> Score a
trackToScore x = trackToScore' (const x)

-- |
-- Convert a track to a score, using durations determined by the values.
--
trackToScore' :: Transformable a => (a -> Duration) -> Track a -> Score a
trackToScore' f = (^. from events) . fmap (\(t,x) -> (t,f x,x)) . map (^. from delayed) . (^. delayeds)
-}


-- TODO rename during
noteToReactive :: Monoid a => Note a -> Reactive a
noteToReactive n = (pure <$> n) `activate` pure mempty

-- | Split a reactive into mkNotes, as well as the values before and after the first/last update
splitReactive :: Reactive a -> Either a ((a, Time), [Note a], (Time, a))
splitReactive r = case updates r of
    []          -> Left  (initial r)
    (t,x):[]    -> Right ((initial r, t), [], (t, x))
    (t,x):xs    -> Right ((initial r, t), fmap mkNote $ mrights (res $ (t,x):xs), head $ mlefts (res $ (t,x):xs))

    where

        mkNote (t,u,x) = (t <-> u, x)^.note

        -- Always returns a 0 or more Right followed by one left
        res :: [(Time, a)] -> [Either (Time, a) (Time, Time, a)]
        res rs = let (ts,xs) = unzip rs in
            flip fmap (withNext ts `zip` xs) $
                \ ((t, mu), x) -> case mu of
                    Nothing -> Left (t, x)
                    Just u  -> Right (t, u, x)

        -- lenght xs == length (withNext xs)
        withNext :: [a] -> [(a, Maybe a)]
        withNext = go
            where
                go []       = []
                go [x]      = [(x, Nothing)]
                go (x:y:rs) = (x, Just y) : withNext (y : rs)

activate :: Note (Reactive a) -> Reactive a -> Reactive a
activate (view (from note) -> (view range -> (start,stop), x)) y = y `turnOn` (x `turnOff` y)
  where
    turnOn  = switchR start
    turnOff = switchR stop

