
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleInstances,
    OverloadedStrings,
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
-- Provides a musical score represenation.
--
-------------------------------------------------------------------------------------


module Music.Score.Zip (
        -- ** Zipper
        apply,
        sample,
        -- trig,
        applySingle,
        sampleSingle, 
        -- before,
        -- first,
        -- butFirst,
  ) where

import Control.Monad (ap, mfilter, join, liftM, MonadPlus(..))
import Data.Semigroup
import Data.String
import Data.Foldable
import Data.Traversable
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace
import Data.Ratio  
import Data.Ord

import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Duration
import Music.Score.Time
import Music.Score.Part
import Music.Score.Combinators

-------------------------------------------------------------------------------------
-- Analysis

apply :: (Ord v, v ~ Part a, HasPart a) => Voice (Score a -> Score b) -> Score a -> Score b
apply x = mapParts (fmap $ applySingle x)

sample :: (Ord v, v ~ Part a, HasPart a) => Score b -> Score a -> Score (b, Score a)
sample x = mapParts (fmap $ sampleSingle x)

trig :: Score a -> Score b -> Score b
trig p as = mconcat $ toList $ fmap snd $ sampleSingle p as

applySingle :: Voice (Score a -> Score b) -> Score a -> Score b
applySingle fs as = notJoin $ fmap (\(f,s) -> f s) $ sampled
    where            
        -- This is not join; we simply concatenate all inner scores in parallel
        notJoin = mconcat . toList
        sampled = sampleSingle (voiceToScore fs) as

-- |
-- Get all notes that start during a given note.
--
sampleSingle :: Score a -> Score b -> Score (a, Score b)
sampleSingle as bs = mapEvents ( \t d a -> g a (onsetIn t d bs) ) $ as
    where
        -- g Nothing  z = Nothing
        g = (,)


-- | Filter out events that has its onset in the given time interval (inclusive start).
--   For example, onset in 1 2 filters events such that (1 <= onset x < 3)
onsetIn :: Time -> Duration -> Score a -> Score a
onsetIn a b = chordDelayStretch . filt (\(t,d,x) -> a <= t && t < a .+^ b) . perform 
    where
        -- filt = mfilter
        filt = takeUntil
        -- more lazy than mfilter
                                                                              
-- Take until predicate goes from True to False.
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p as = List.takeWhile p (List.dropWhile (not . p) as)


before :: Duration -> Score a -> Score a
before d = trig (note ()^*d)

first :: Score a -> a
first = get3 . head . perform
    where get3 (a,b,c) = c

butFirst :: Score a -> Score a
butFirst = undefined -- FIXME
-- butFirst = Score . tail . getScore


