                              
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


module Music.Score.Articulation (
        HasArticulation(..),
        ArticulationT(..),
  ) where

import Data.Ratio
import Data.Foldable
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace

import Music.Score.Part
import Music.Score.Score
import Music.Score.Duration
import Music.Score.Time

class HasArticulation a where
    setBeginSlur :: Bool -> a -> a
    setContSlur :: Bool -> a -> a
    setEndSlur :: Bool -> a -> a
    setAccLevel :: Int -> a -> a
    setStaccLevel :: Int -> a -> a
    
newtype ArticulationT a = ArticulationT { getArticulationT :: (Bool, Bool, Int, Int, a, Bool) }
    deriving (Eq, Show, Ord, Functor, Foldable)

