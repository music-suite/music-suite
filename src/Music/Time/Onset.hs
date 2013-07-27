
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
        -- HasPreOnset(..),
        -- HasPostOnset(..),
        -- HasPostOffset(..),
        -- ** Defaults
        durationDefault,
        onsetDefault,
        offsetDefault,
        -- ** Wrappers
        -- AddOffset(..),
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace

import Music.Time.Time
import Music.Time.Delayable
import Music.Time.Stretchable

class HasDuration a where
    duration :: a -> Duration a

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
    onset  :: a -> Time a

class HasOffset a where
    -- | 
    -- Get the offset of the given value.
    --
    offset :: a -> Time a
                              
{-
class HasPreOnset s where
    preOnset :: s a -> Time s

class HasPostOnset s where
    postOnset :: s a -> Time s

class HasPostOffset s where
    postOffset :: s a -> Time s
-}

-- | Given 'HasOnset' and 'HasOffset' instances, this function implements 'duration'.
durationDefault :: (AdditiveGroup (Duration a), HasOffset a, HasOnset a) => a -> Duration a
durationDefault x = offset x .-. onset x

-- | Given 'HasDuration' and 'HasOffset' instances, this function implements 'onset'.
onsetDefault :: (AdditiveGroup (Duration a), HasOffset a, HasDuration a) => a -> Time a
onsetDefault x = offset x .-^ duration x

-- | Given 'HasOnset' and 'HasOnset' instances, this function implements 'offset'.
offsetDefault :: (AdditiveGroup (Duration a), HasOnset a, HasDuration a) => a -> Time a
offsetDefault x = onset x .+^ duration x
                                                 
{-
newtype AddOffset t s a = AddOffset (t, s a)

type instance Time (AddOffset t s) = t

instance (Delayable a, t ~ Time a) => Delayable (AddOffset t a) where
    delay d (AddOffset (t,a)) = AddOffset (t, delay d a)

instance (Stretchable a, t ~ Time a) => Stretchable (AddOffset t a) where
    stretch d (AddOffset (t,a)) = AddOffset (t, stretch d a)

instance (HasOnset a, t ~ Time a) => HasOnset (AddOffset t a) where
    onset (AddOffset (t,a)) = onset a

instance HasOffset (AddOffset t s) where
    offset (AddOffset (t,_)) = t

-}
