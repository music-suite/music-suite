                              
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleInstances,
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


module Music.Score.Ties (
        Tiable(..),
        TieT(..),
        splitTies,
        splitTiesSingle,
        splitTiesPart,
  ) where

import Data.Ratio
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace


import Music.Score.Part
import Music.Score.Score
import Music.Score.Duration
import Music.Score.Time



-- |
-- Class of types that can be tied.
--
class Tiable a where
    -- | Split elements into beginning and end and add tie.
    --   Begin properties goes to the first tied note, and end properties to the latter.

    --   The first returned element will have the original onset.
    --   
    toTied    :: a -> (a, a)

newtype TieT a = TieT { getTieT :: (Bool, a, Bool) }    
    deriving (Eq, Ord, Show, Functor)

-- These are note really tiable..., but Tiable a => (Bool,a,Bool) would be
instance Tiable Double      where toTied x = (x,x)
instance Tiable Float       where toTied x = (x,x)
instance Tiable Int         where toTied x = (x,x)
instance Tiable Integer     where toTied x = (x,x)
instance Tiable ()          where toTied x = (x,x)
instance Tiable (Ratio a)   where toTied x = (x,x)

instance Tiable a => Tiable (Maybe a) where
    toTied Nothing  = (Nothing, Nothing)
    toTied (Just a) = (Just b, Just c) where (b,c) = toTied a
    
-- instance Tiable a => Tiable (String, a) where
--     toTied (v,a) = ((v,b),(v,c)) where (b,c) = toTied a
-- instance Tiable a => Tiable (Bool, a, Bool) where
--     toTied (prevTie, a, _) = ((prevTie, b, True), (True, c, False))

-- instance Tiable a => Tiable (VoiceT a) where
-- Note: This instance is in the Voice module to break the recursive dependency
instance Tiable a => Tiable (TieT a) where
    toTied (TieT (prevTie, a, _)) = (TieT (prevTie, b, True), TieT (True, c, False))
         where (b,c) = toTied a

-- | 
-- /Not implemented/
-- Split all notes that cross a barlines into a pair of tied notes.
-- 
splitTies :: Tiable a => Score a -> Score a
splitTies = error "splitTies: Not implemented"

-- | 
-- Split all notes that cross a barlines into a pair of tied notes.
-- Note: only works for single-part scores (with no overlapping events).
-- 
splitTiesSingle :: Tiable a => Score a -> Score a
splitTiesSingle = partToSingleScore . splitTiesPart . singleScoreToPart

partToSingleScore :: Part (Maybe a) -> Score a
partToSingleScore  = Score . accumTime . getPart
    where
        accumTime = snd . List.mapAccumL g 0
            where
                g t (d, x) = (t .+^ d, (t, d, x))

singleScoreToPart :: Score a -> Part (Maybe a)
singleScoreToPart sc = Part . movePart . throwTime . getScore $ sc
    where
        throwTime = fmap g where g (t,d,x) = (d,x)
        d = onset sc .-. 0
        movePart = if (d == 0) then id else ([(d, Nothing)] ++)

-- | 
-- Split all notes that cross a barlines into a pair of tied notes.
-- 
splitTiesPart :: Tiable a => Part a -> Part a
splitTiesPart = Part . concat . snd . List.mapAccumL g 0 . getPart
    where
        g t (d, x) = (t + d, occs)        
            where
                (_, barTime) = properFraction t
                remBarTime   = 1 - barTime
                occs = splitDur remBarTime (d,x)

-- |
-- Split an event into a part the given duration, and parts shorter than or equal to one.
-- The returned list is always non-empty.
--
-- > sum $ fmap fst $ splitDur s (x,a) = x
--         
splitDur :: Tiable a => Duration -> (Duration, a) -> [(Duration, a)]
splitDur s x = case splitDur' s x of
    (a, Nothing) -> a : []
    (a, Just b)  -> a : splitDur 1 b

-- |
-- Extract the the first part of a given duration. If the note is shorter than the given duration,
-- return it and @Nothing@. Otherwise return the extracted part, and @Just@ the rest.
--
-- > splitDur s (d,a)
--         
splitDur' :: Tiable a => Duration -> (Duration, a) -> ((Duration, a), Maybe (Duration, a))
splitDur' s (d,a) | d <= s     =  ((d,a), Nothing)
                  | otherwise  =  ((s,b), Just (d-s, c)) where (b,c) = toTied a
                 


-- FIXME completely assumes bar dur 1 (see also main module)
