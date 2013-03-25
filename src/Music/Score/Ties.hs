                              
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
        splitTies
  ) where

import Data.Ratio
import qualified Data.List as List

import Music.Score.Part
import Music.Score.Score




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

instance Tiable a => Tiable (Bool, a, Bool) where
    toTie (prevTie, x, _) = ((prevTie, x, True), (True, x, False))

-- | 
-- Split all notes that cross a barlines into a pair of tied notes.
-- 
splitTies :: Tiable a => Score a -> Score a
splitTies = undefined


-- -- | 
-- -- Split all notes that cross a barlines into a pair of tied notes.
-- -- 
-- splitTiesPart :: Tiable a => Part a -> Part a
-- splitTiesPart = Part . snd . List.mapAccumL g 0 . getPart
--     where
--         g barTime (d,x)
--             | barTime <  1 && barTime + d < 1
--             | barTime >= 1
--         







