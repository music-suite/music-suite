
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleContexts,
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
        HasDuration(..),
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

import Music.Time.Pos
import Music.Time.Time
import Music.Time.Duration
import Music.Time.Delayable
import Music.Time.Stretchable

class HasDuration s where
    duration :: s a -> Duration s

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
class HasOnset s where
    -- | 
    -- Get the onset of the given value.
    --
    onset  :: s a -> Time s

class HasOffset s where
    -- | 
    -- Get the offset of the given value.
    --
    offset :: s a -> Time s
                              
class HasPreOnset s where
    preOnset :: s a -> Time s

class HasPostOnset s where
    postOnset :: s a -> Time s

class HasPostOffset s where
    postOffset :: s a -> Time s

-- | Given 'HasOnset' and 'HasOffset' instances, this function implements 'duration'.
durationDefault :: (AffineSpace (Time s), HasOffset s, HasOnset s) => s a -> Duration s
durationDefault x = offset x .-. onset x

-- | Given 'HasDuration' and 'HasOffset' instances, this function implements 'onset'.
onsetDefault :: (AffineSpace (Time s), HasOffset s, HasDuration s) => s a -> Time s
onsetDefault x = offset x .-^ duration x

-- | Given 'HasOnset' and 'HasOnset' instances, this function implements 'offset'.
offsetDefault :: (AffineSpace (Time s), HasOnset s, HasDuration s) => s a -> Time s
offsetDefault x = onset x .+^ duration x
                                                 