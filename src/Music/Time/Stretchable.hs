
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

module Music.Time.Stretchable (
        Stretchable(..),
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace

import Music.Time.Time
import Music.Time.Duration
import Music.Time.Pos

-- -- |
-- -- Stretchable values.
-- -- 
-- type Stretchable a = (VectorSpace a, Scalar a ~ Duration)


-- |
-- Stretchable values. 
-- 
class Stretchable a where

    -- |
    -- Stretch (augment) a value by the given factor.
    -- 
    -- > Duration -> Score a -> Score a
    -- 
    stretch :: Dur a -> a -> a
