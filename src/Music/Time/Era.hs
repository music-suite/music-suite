
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

module Music.Time.Era where

import Music.Time.Absolute
import Music.Time.Relative

-- |
-- An era is a time interval.
-- 
newtype Era = Era { getEra :: (Time, Time) }

    -- instance VectorSpace Era where
    -- instance Semigroup Era where -- smallest era that contains both eras
    -- instance HasOnset where
    -- instance HasOffset where
    -- instance HasDuration where
    -- instance Delayable where
    -- instance Stretchable where

