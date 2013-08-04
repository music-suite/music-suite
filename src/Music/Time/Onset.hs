
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
-- This misleadingly named module provide a way to query a value for its
--  'duration', 'onset' and 'offset'.
--
-------------------------------------------------------------------------------------

module Music.Time.Onset (
        -- * Duration class
        HasDuration(..),
        stretchTo,

        -- * Onset and offset class
        HasOnset(..),
        HasOffset(..),
        startAt,
        stopAt,
        
        -- HasPreOnset(..),
        -- HasPostOnset(..),
        -- HasPostOffset(..),


        -- * Utility
        -- ** Default implementations
        durationDefault,
        onsetDefault,
        offsetDefault,

        -- ** Wrappers
        AddOffset(..),
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace

import Music.Time.Time
import Music.Time.Delayable
import Music.Time.Stretchable

-- |
-- Class of types with a duration.
--
-- If a type has an instance for both 'HasOnset' and 'HasDuration', the following laws
-- should hold:
-- 
-- > duration a = offset a .-- onset a
--
class HasDuration a where
    duration :: a -> Duration a

-- |
-- Stretch a score to fit into the given duration.
--
-- > Duration -> Score a -> Score a
--
stretchTo       :: (Stretchable a, HasDuration a, Fractional d, d ~ Duration a) =>
                d -> a -> a


-- |
-- Class of types with a position in time.
--
-- Onset and offset are logical start and stop time, i.e. the preferred beginning and end
-- of the sound, not o the the time of the attack and damp actions on an instrument,
--
-- If a type has an instance for both 'HasOnset' and 'HasDuration', the following laws
-- should hold:
-- 
-- > duration a = offset a .-- onset a
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
                              
-- |
-- Move a score so that its onset is at the specific time.
--
-- > Time -> Score a -> Score a
--
startAt         :: (HasOnset a, Delayable a, AffineSpace t, t ~ Time a) =>
                t ->  a -> a

-- |
-- Move a score so that its offset is at the specific time.
--
-- > Time -> Score a -> Score a
--
stopAt          :: (HasOffset a, Delayable a, AffineSpace t, t ~ Time a) =>
                t -> a -> a

t `stretchTo` x = (t / duration x) `stretch` x
t `startAt` x   = (t .-. onset x) `delay` x
t `stopAt`  x   = (t .-. offset x) `delay` x
                                             

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
                                                 
newtype AddOffset t a = AddOffset (t, a)

type instance Duration (AddOffset t a) = Diff t

instance (Delayable a, Time a ~ t) => Delayable (AddOffset t a) where
    delay d (AddOffset (t, a)) = AddOffset (t, delay d a)

instance (Stretchable a, t ~ Time a) => Stretchable (AddOffset t a) where
    stretch d (AddOffset (t, a)) = AddOffset (t, stretch d a)

instance (HasOnset a, t ~ Time a) => HasOnset (AddOffset t a) where
    onset (AddOffset (t,a)) = onset a

-- instance HasOffset (AddOffset t s) where
--     offset (AddOffset (t, _)) = t

