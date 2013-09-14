
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
-- Provides reversible values.
--
-------------------------------------------------------------------------------------

module Music.Time.Reverse (
        -- * Reversible class
        Reversible(..),
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point

import Music.Time.Time

-- |
-- Reversible values.
--
-- For instances of 'Reversible' and 'HasOnset, the following laws should hold:
--
-- > onset a    = onset (rev a)
-- > duration a = duration (rev a)
--
class Reversible a where

    -- |
    -- Reverse a value.
    -- 
    -- Reverse is an involution:
    --
    -- > rev (rev a) = a
    -- 
    -- > Score a -> Score a
    -- 
    rev :: a -> a

instance Reversible a => Reversible [a] where
    rev = fmap rev


