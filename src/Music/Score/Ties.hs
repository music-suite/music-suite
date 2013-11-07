
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
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
-- Provides a representation for tied notes, and a way to split a single note
-- into a pair of tied notes.
--
-------------------------------------------------------------------------------------

module Music.Score.Ties (
        -- * Tiable class
        Tiable(..),
        TieT(..),        
        
        -- * Splitting tied notes in scores
        splitTies,
        splitTiesVoice,
        -- splitTiesSingle,
  ) where

import Control.Monad
import Control.Monad.Plus
import Data.Default
import Data.Maybe
import Data.Ratio
import Data.Foldable hiding (concat)
import Data.Typeable
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace

import Music.Score.Voice
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Convert
import Music.Score.Part
import Music.Time

-- |
-- Class of types that can be tied.
--
-- Minimal definition: 'toTied', or both 'beginTie' and 'endTie'.
--
class Tiable a where
    -- |
    -- Modify a note to be the first note in a tied note pair.
    -- 
    beginTie :: a -> a
    beginTie = fst . toTied

    -- |
    -- Modify a note to be the second note in a tied note pair.
    -- 
    endTie :: a -> a
    endTie = snd . toTied

    -- | 
    -- Split a single note into a pair of tied notes.
    --
    -- The first returned element should have the original 'onset' and the second
    -- element should have the original 'offset'. Formally
    --
    -- > (onset . fst . toTied) a = onset a
    -- > (offset . snd . toTied) a = offset a
    --
    toTied    :: a -> (a, a)
    toTied a = (beginTie a, endTie a)

newtype TieT a = TieT { getTieT :: (Bool, a, Bool) }
    deriving (Eq, Ord, Show, Functor, Foldable, Typeable)

instance Tiable Double      where { beginTie = id ; endTie = id }
instance Tiable Float       where { beginTie = id ; endTie = id }
instance Tiable Int         where { beginTie = id ; endTie = id }
instance Tiable Integer     where { beginTie = id ; endTie = id }
instance Tiable ()          where { beginTie = id ; endTie = id }
instance Tiable (Ratio a)   where { beginTie = id ; endTie = id }

instance Tiable a => Tiable (Maybe a) where
    beginTie = fmap beginTie
    endTie = fmap endTie
    toTied Nothing  = (Nothing, Nothing)
    toTied (Just a) = (Just b, Just c) where (b,c) = toTied a

instance Tiable a => Tiable (TieT a) where
    beginTie (TieT (prevTie, a, nextTie)) = TieT (prevTie, a, True)
    endTie   (TieT (prevTie, a, nextTie)) = TieT (True, a, nextTie)
    toTied (TieT (prevTie, a, nextTie))   = (TieT (prevTie, b, True), TieT (True, c, nextTie))
         where (b,c) = toTied a

-- |
-- Split all notes that cross a barlines into a pair of tied notes.
--
splitTies :: (HasPart' a, Tiable a) => Score a -> Score a
splitTies = mapParts splitTiesSingle

-- |
-- Equivalent to `splitTies` for single-voice scores.
-- Fails if the score contains overlapping events.
--
splitTiesSingle :: Tiable a => Score a -> Score a
splitTiesSingle = removeRests . voiceToScore . splitTiesVoice . scoreToVoice

-- |
-- Split all notes that cross a barlines into a pair of tied notes.
--
splitTiesVoice :: Tiable a => Voice a -> Voice a
splitTiesVoice = voice . concat . snd . List.mapAccumL g 0 . getVoice
    where
        g t (d, x) = (t + d, occs)
            where
                (_, barTime) = properFraction t
                remBarTime   = 1 - barTime
                occs         = splitDur remBarTime 1 (d,x)

-- |
-- Split an event into one chunk of the duration @s@, followed parts shorter than duration @t@.
--
-- The returned list is always non-empty. All elements but the first and the last must have duration @t@.
--
-- > sum $ fmap fst $ splitDur s (x,a) = x
--
splitDur :: Tiable a => Duration -> Duration -> (Duration, a) -> [(Duration, a)]
splitDur s t x = case splitDur' s x of
    (a, Nothing) -> [a]
    (a, Just b)  -> a : splitDur t t b

-- |
-- Extract the the first part of a given duration. If the note is shorter than the given duration,
-- return it and @Nothing@. Otherwise return the extracted part, and the rest.
--
-- > splitDur s (d,a)
--
splitDur' :: Tiable a => Duration -> (Duration, a) -> ((Duration, a), Maybe (Duration, a))
splitDur' s (d,a) 
    | d <= s     =  ((d,a), Nothing)
    | otherwise  =  ((s,b), Just (d-s, c)) where (b,c) = toTied a




