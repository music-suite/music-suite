
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
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
-------------------------------------------------------------------------------------

module Music.Time.Delayable (
        Delayable(..),
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace

import Music.Time.Time
import Music.Time.Duration


-- |
-- Delayable values. 
-- 
-- Similar to @(AffineSpace a, Diff a ~ Duration)@.
-- 
class Delayable a where

    -- |
    -- Delay a value.
    -- > Duration -> Score a -> Score a
    -- 
    delay :: Duration -> a -> a
