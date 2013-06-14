
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
import Music.Time.Pos


-- |
-- Delayable values. 
-- 
class Delayable s where

    -- |
    -- Delay a value.
    -- > Duration -> Score a -> Score a
    -- 
    delay :: Duration s -> s a -> s a
