
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
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

module Music.Time.Era (
        Era,
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace

import Music.Time.Time
import Music.Time.Duration
import Music.Time.Delayable
import Music.Time.Stretchable

-- |
-- An era is a time interval.
-- 
newtype Era = Era { getEra :: (Min Time, Max Time) }
    deriving (Eq, Ord, Show, Semigroup)

instance HasOnset Era where
    onset = getMin . fst . getEra

instance HasOffset Era where
    offset = getMax . snd . getEra

instance HasDuration Era where
    duration = durationDefault

instance Delayable Era where
    delay t (Era (Min a, Max b)) = Era (Min $ a .+^ t, Max $ b .+^ t)

-- instance Stretchable Era where

-- instance VectorSpace Era where
