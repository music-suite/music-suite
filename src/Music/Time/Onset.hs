
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

module Music.Time.Onset (
        HasOnset(..),
        HasOffset(..),
        HasPreOnset(..),
        HasPostOnset(..),
        HasPostOffset(..),
        durationDefault,
        onsetDefault,
        offsetDefault,
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace

import Music.Time.Time
import Music.Time.Duration
import Music.Time.Delayable
import Music.Time.Stretchable

-- |
-- Class of types with a position in time.
--
-- Onset and offset are logical start and stop time, i.e. the preferred beginning and end
-- of the sound, not o the the time of the attack and damp actions on an instrument,
--
-- If a type has an instance for both 'HasOnset' and 'HasDuration', the following laws
-- should hold:
-- 
-- > duration a = offset a - onset a
-- > offset a >= onset a
--
-- implying
--
-- > duration a >= 0
--
class HasOnset a where
    -- | 
    -- Get the onset of the given value.
    --
    onset  :: a -> Time

class HasOffset a where
    -- | 
    -- Get the offset of the given value.
    --
    offset :: a -> Time
                              
class HasPreOnset a where
    preOnset :: a -> Time

class HasPostOnset a where
    postOnset :: a -> Time

class HasPostOffset a where
    postOffset :: a -> Time

-- | Given 'HasOnset' and 'HasOffset' instances, this function implements 'duration'.
durationDefault :: (HasOnset a, HasOffset a) => a -> Duration
durationDefault x = offset x .-. onset x

-- | Given 'HasDuration' and 'HasOffset' instances, this function implements 'onset'.
onsetDefault :: (HasDuration a, HasOffset a) => a -> Time
onsetDefault x = offset x .-^ duration x

-- | Given 'HasOnset' and 'HasOnset' instances, this function implements 'offset'.
offsetDefault :: (HasDuration a, HasOnset a) => a -> Time
offsetDefault x = onset x .+^ duration x
                                                 