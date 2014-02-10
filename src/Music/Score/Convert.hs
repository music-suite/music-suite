
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
-- Provides conversion betwen temporal container types.
--
-------------------------------------------------------------------------------------


module Music.Score.Convert (
        -- * Conversion
        noteToVoice,
        noteToScore,
        -- scoreToNotes,
        -- notesToScore,
        voiceToScore,
        voicesToScore,
        trackToScore,
        trackToScore',
        scoreToVoice,
        reactiveToVoice,
        reactiveToVoice',
        noteToReactive,
        splitReactive,
        activate,
  ) where

import Control.Applicative
import Control.Lens
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
import Data.Ord

import Music.Score.Note
import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Part
import Music.Time
import Music.Time.Reactive

import qualified Data.List as List
import qualified Data.Foldable as Foldable


-- | Convert a note to an onset and a voice.
noteToVoice :: Note a -> (Time, Voice a)
noteToVoice (getNote -> (s,x)) = (onset s, stretchTo (duration s) $ return x)

-- | Convert a note to a score.
noteToScore :: Note a -> Score a
noteToScore (getNote -> (s,x)) = s `sapp` return x

-- scoreToNotes :: Score a -> [Note a]
-- scoreToNotes = Foldable.toList . reifyScore

-- notesToScore :: [Note a] -> Score a
-- notesToScore = pcat . fmap noteToScore

reactiveToVoice :: Duration -> Reactive a -> Voice a
reactiveToVoice d r = (^. voice) $ durs `zip` (fmap (r ?) times)
    where
        times = origin : filter (\t -> origin < t && t < origin .+^ d) (occs r)
        durs  = toRelN' (origin .+^ d) times

reactiveToVoice' :: Span -> Reactive a -> Voice a
reactiveToVoice' (view range -> (u,v)) r = (^. voice) $ durs `zip` (fmap (r ?) times)
    where
        times = origin : filter (\t -> u < t && t < v) (occs r)
        durs  = toRelN' v times

-- |
-- Convert a score to a voice. Fails if the score contain overlapping events.
--
scoreToVoice :: Score a -> Voice (Maybe a)
scoreToVoice = (^. voice) . fmap throwTime . addRests . (^. events)
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
voiceToScore = scat . fmap g . (^. from voice)
    where
        g (d,x) = stretch d (return x)

-- | Join voices in a given part into a score.
voicesToScore :: HasPart a => [(Part a, Voice a)] -> Score a
voicesToScore = pcat . fmap (voiceToScore . uncurry (\n -> fmap (setPart n)))

-- |
-- Convert a voice which may contain rests to a score.
--
voiceToScore' :: Voice (Maybe a) -> Score a
voiceToScore' = mcatMaybes . voiceToScore

-- |
-- Convert a track to a score where each event is given a fixed duration.
--
trackToScore :: Duration -> Track a -> Score a
trackToScore x = trackToScore' (const x)

-- |
-- Convert a track to a score, using durations determined by the values.
--
trackToScore' :: (a -> Duration) -> Track a -> Score a
trackToScore' f = (^. from events) . fmap (\(t,x) -> (t,f x,x)) . (^. from track)


-- Convert to delta (time to wait before this note)
toRel :: [Time] -> [Duration]
toRel = snd . mapAccumL g origin where g prev t = (t, t .-. prev)

-- Convert to delta (time to wait before next note)
toRelN :: [Time] -> [Duration]                
toRelN [] = []
toRelN xs = snd $ mapAccumR g (last xs) xs where g prev t = (t, prev .-. t)

-- Convert to delta (time to wait before next note)
toRelN' :: Time -> [Time] -> [Duration]                
toRelN' end xs = snd $ mapAccumR g end xs where g prev t = (t, prev .-. t)

-- 0 x,1 x,1 x,1 x
  -- x 1,x 1,x 1,x 0

-- Convert from delta (time to wait before this note)
toAbs :: [Duration] -> [Time]
toAbs = snd . mapAccumL g origin where g now d = (now .+^ d, now .+^ d)


-- TODO rename during
noteToReactive :: Monoid a => Note a -> Reactive a
noteToReactive n = (pure <$> n) `activate` pure mempty

-- | Split a reactive into notes, as well as the values before and after the first/last update
splitReactive :: Reactive a -> Either a ((a, Time), [Note a], (Time, a))
splitReactive r = case updates r of
    []          -> Left  (initial r)
    (t,x):[]    -> Right ((initial r, t), [], (t, x))
    (t,x):xs    -> Right ((initial r, t), fmap note $ mrights (res $ (t,x):xs), head $ mlefts (res $ (t,x):xs))

    where

        note (t,u,x) = t <-> u =: x

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
activate (getNote -> (view range -> (start,stop), x)) y = y `turnOn` (x `turnOff` y)
    where
        turnOn  = switch start
        turnOff = switch stop

