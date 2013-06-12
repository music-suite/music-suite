
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
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
-- Provides zippers over scores.
--
-------------------------------------------------------------------------------------


module Music.Score.Zip (
        -- ** Zipper
        apply,
        snapshot,
        -- trig,
        applySingle,
        snapshotSingle, 
        -- before,
        -- first,
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
import Music.Time
import Music.Score.Part
import Music.Score.Combinators

-------------------------------------------------------------------------------------
-- Analysis

-- |
-- Apply a time-varying function to all events in score.
--
apply :: HasPart' a => Voice (Score a -> Score b) -> Score a -> Score b
apply x = mapParts (fmap $ applySingle x)

-- |
-- Get all notes that start during a given note.
--
snapshot :: HasPart' a => Score b -> Score a -> Score (b, Score a)
snapshot x = mapParts (fmap $ snapshotSingle x)

trig :: Score a -> Score b -> Score b
trig p as = mconcat $ toList $ fmap snd $ snapshotSingle p as

-- |
-- Apply a time-varying function to all events in score.
--
applySingle :: Voice (Score a -> Score b) -> Score a -> Score b
applySingle fs as = notJoin $ fmap (\(f,s) -> f s) sampled
    where            
        -- This is not join; we simply concatenate all inner scores in parallel
        notJoin = mconcat . toList
        sampled = snapshotSingle (voiceToScore fs) as

-- |
-- Get all notes that start during a given note.
--
snapshotSingle :: Score a -> Score b -> Score (a, Score b)
snapshotSingle as bs = mapEventsSingle ( \t d a -> g a (onsetIn t d bs) ) as
    where
        -- g Nothing  z = Nothing
        g = (,)


-- | 
-- Filter out events that has its onset in the given time interval (inclusive start).
-- For example, onset in 1 2 filters events such that (1 <= onset x < 3)
onsetIn :: Time -> Duration -> Score a -> Score a
onsetIn a b = recompose . filt (\(t,d,x) -> a <= t && t < a .+^ b) . perform 
    where
        -- filt = mfilter
        filt = filterOnce
        -- more lazy than mfilter
                                                                              
-- | 
-- Extract the first consecutive sublist for which the predicate returns true, or
-- the empty list if no such sublist exists.
filterOnce :: (a -> Bool) -> [a] -> [a]
filterOnce p = List.takeWhile p . List.dropWhile (not . p)


before :: Duration -> Score a -> Score a
before d = trig (return () `stretchedBy` d)

first :: Score a -> a
first = value . head . perform
    where value (a,b,c) = c


stretchedBy = flip stretch