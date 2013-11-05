
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
        withSameOnset,
        withSameOffset,
        
        -- HasPreOnset(..),
        -- HasPostOnset(..),
        -- HasPostOffset(..),


        -- * Utility
        -- ** Default implementations
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

-- |
-- Class of types with a duration.
--
-- If a type has an instance for both 'HasOnset' and 'HasDuration', the following laws
-- should hold:
-- 
-- > duration a = offset a .-. onset a
--
class HasDuration a where
    duration :: a -> Duration

-- |
-- Stretch a score to fit into the given duration.
--
-- > Duration -> Score a -> Score a
--
stretchTo       :: (Stretchable a, HasDuration a) =>
                Duration -> a -> a


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
    onset  :: a -> Time

class HasOffset a where
    -- | 
    -- Get the offset of the given value.
    --
    offset :: a -> Time
                              
-- |
-- Move a score so that its onset is at the specific time.
--
-- > Time -> Score a -> Score a
--
startAt         :: (HasOnset a, Delayable a) =>
                Time ->  a -> a

-- |
-- Move a score so that its offset is at the specific time.
--
-- > Time -> Score a -> Score a
--
stopAt          :: (HasOffset a, Delayable a) =>
                Time -> a -> a

t `stretchTo` x = (t / duration x) `stretch` x
t `startAt` x   = (t .-. onset x) `delay` x
t `stopAt`  x   = (t .-. offset x) `delay` x
                                             
-- |
-- Transform a score without affecting its onset.
--
-- > Time -> Score a -> Score a
--
withSameOnset      :: (Delayable a, HasOnset a, HasOnset b) =>
                    (b -> a) -> b -> a

-- |
-- Transform a score without affecting its offset.
--
-- > Time -> Score a -> Score a
--
withSameOffset      :: (Delayable a, HasOffset a, HasOffset b) =>

                    (b -> a) -> b -> a

withSameOnset f a  = startAt (onset a) $ f a
withSameOffset f a = stopAt (offset a) $ f a


{-
class HasPreOnset s where
    preOnset :: s a -> Time s

class HasPostOnset s where
    postOnset :: s a -> Time s

class HasPostOffset s where
    postOffset :: s a -> Time s
-}

-- | Given 'HasOnset' and 'HasOffset' instances, this function implements 'duration'.
durationDefault :: (AdditiveGroup (Duration), HasOffset a, HasOnset a) => a -> Duration
durationDefault x = offset x .-. onset x

-- | Given 'HasDuration' and 'HasOffset' instances, this function implements 'onset'.
onsetDefault :: (AdditiveGroup (Duration), HasOffset a, HasDuration a) => a -> Time
onsetDefault x = offset x .-^ duration x

-- | Given 'HasOnset' and 'HasOnset' instances, this function implements 'offset'.
offsetDefault :: (AdditiveGroup (Duration), HasOnset a, HasDuration a) => a -> Time
offsetDefault x = onset x .+^ duration x
                                                 
newtype AddOffset t a = AddOffset (t, a)

instance (Delayable a, Time ~ t) => Delayable (AddOffset t a) where
    delay d (AddOffset (t, a)) = AddOffset (t, delay d a)

instance (Stretchable a, t ~ Time) => Stretchable (AddOffset t a) where
    stretch d (AddOffset (t, a)) = AddOffset (t, stretch d a)

instance (HasOnset a, t ~ Time) => HasOnset (AddOffset t a) where
    onset (AddOffset (t,a)) = onset a

-- instance HasOffset (AddOffset t s) where
--     offset (AddOffset (t, _)) = t

