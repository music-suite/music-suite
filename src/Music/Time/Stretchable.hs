
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

import Music.Time.Pos

-- |
-- Stretchable values. 
-- 
class Stretchable s where

    -- |
    -- Stretch (augment) a value by the given factor.
    -- 
    -- > Duration -> Score a -> Score a
    -- 
    stretch :: Duration s -> s a -> s a
