
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
        compress,
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace

import Music.Time.Time

-- |
-- Stretchable values. 
-- 
class Stretchable a where

    -- |
    -- Stretch (augment) a value by the given factor.
    -- 
    -- > Duration -> Score a -> Score a
    -- 
    stretch :: Duration a -> a -> a


-- |
-- Compress (diminish) a score. Flipped version of '^/'.
--
-- > Duration -> Score a -> Score a
--
compress        :: (Stretchable a, Fractional d, d ~ Duration a) =>
                d -> a -> a

compress x      = stretch (recip x)

