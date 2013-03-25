                              
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
    toTie :: a -> (a, a)

-- These are note really tiable..., but Tiable a => (Bool,a,Bool) would be
instance Tiable Double      where toTie x = (x,x)
instance Tiable Int         where toTie x = (x,x)
instance Tiable Integer     where toTie x = (x,x)
instance Tiable ()          where toTie x = (x,x)
instance Tiable (Ratio a)   where toTie x = (x,x)

instance Tiable a => Tiable (Maybe a) where
    toTie Nothing  = (Nothing, Nothing)
    toTie (Just x) = (Just x, Just x)

-- instance Tiable String   where 
--     toTie ")" = ("~", ")")
--     toTie _   = ("(", ")")

instance Tiable a => Tiable (Bool, a, Bool) where
    toTie (prevTie, x, _) = ((prevTie, x, True), (True, x, False))

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
splitTiesSingle = Score . accumTime . (getPart . splitTiesPart . Part) . throwTime . getScore
    where
        throwTime = fmap g where g (t,d,x) = (d,x)
        accumTime = snd . List.mapAccumL g 0
            where
                g t (d, x) = (t .+^ d, (t, d, x))

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
                  | otherwise  =  ((s,b), Just (d-s, c)) where (b,c) = toTie a
                 


-- FIXME completely assumes bar dur 1 (see also main module)
