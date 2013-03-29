                              
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


module Music.Score.Ornaments (
        HasTremolo(..),
        TremoloT(..),
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

class HasTremolo a where
    setTrem :: Int -> a -> a

newtype TremoloT a = TremoloT { getTremoloT :: (Int, a) }
    deriving (Eq, Show, Ord, Functor{-, Foldable-})
