
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleContexts,
    FlexibleInstances,
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

        -- * Utility
        -- ** Default implementations
        durationDefault,
        onsetDefault,
        offsetDefault,
  ) where


import Data.Semigroup
import Data.VectorSpace hiding (Sum)
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Music.Time.Time
import Music.Time.Delayable
import Music.Time.Stretchable
import Music.Score.Util

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

instance HasDuration Duration where
    duration = id

instance HasDuration (Duration, a) where
    duration = fst

instance HasDuration (Time, Duration, a) where
    duration (t,d,x) = d

instance HasDuration a => HasDuration (Product a) where
    duration (Product x) = duration x

-- Works for monophonic containers but not in general
-- instance HasDuration a => HasDuration [a] where
    -- duration = getSum . F.foldMap (Sum . duration)

-- |
-- Stretch a score to fit into the given duration.
--
-- > Duration -> Score a -> Score a
--
stretchTo :: (Stretchable a, HasDuration a) => Duration -> a -> a

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

instance HasOnset Time where
    onset = id

instance HasOnset (Time, a) where
    onset = fst

instance HasOnset (Time, Duration, a) where
    onset (t,d,x) = t

instance HasOffset (Time, Duration, a) where
    offset (t,d,x) = t .+^ d

instance HasOnset a => HasOnset [a] where
    onset = list origin (minimum . fmap onset)

instance HasOffset a => HasOffset [a] where
    offset = list origin (maximum . fmap offset)

instance HasOnset a => HasOnset (Set a) where
    onset = list origin (onset . head) . Set.toAscList

instance HasOffset a => HasOffset (Set a) where
    offset = list origin (offset . last) . Set.toAscList

instance HasOnset k => HasOnset (Map k a) where
    onset = list origin (onset . head) . Map.keys

instance HasOffset k => HasOffset (Map k a) where
    offset = list origin (offset . last) . Map.keys

instance HasOnset a => HasOnset (Sum a) where
    onset (Sum x) = onset x

                              
-- |
-- Move a score so that its onset is at the specific time.
--
-- > Time -> Score a -> Score a
--
startAt :: (HasOnset a, Delayable a) => Time ->  a -> a

-- |
-- Move a score so that its offset is at the specific time.
--
-- > Time -> Score a -> Score a
--
stopAt :: (HasOffset a, Delayable a) => Time -> a -> a

t `stretchTo` x = (t / duration x) `stretch` x
t `startAt` x   = (t .-. onset x) `delay` x
t `stopAt`  x   = (t .-. offset x) `delay` x
                                             
-- |
-- Transform a score without affecting its onset.
--
-- > Time -> Score a -> Score a
--
withSameOnset :: (Delayable a, HasOnset a, HasOnset b) => (b -> a) -> b -> a

-- |
-- Transform a score without affecting its offset.
--
-- > Time -> Score a -> Score a
--
withSameOffset :: (Delayable a, HasOffset a, HasOffset b) => (b -> a) -> b -> a

withSameOnset f a  = startAt (onset a) $ f a
withSameOffset f a = stopAt (offset a) $ f a

-- | Given 'HasOnset' and 'HasOffset' instances, this function implements 'duration'.
durationDefault :: (HasOffset a, HasOnset a) => a -> Duration
durationDefault x = offset x .-. onset x

-- | Given 'HasDuration' and 'HasOffset' instances, this function implements 'onset'.
onsetDefault :: (HasOffset a, HasDuration a) => a -> Time
onsetDefault x = offset x .-^ duration x

-- | Given 'HasOnset' and 'HasOnset' instances, this function implements 'offset'.
offsetDefault :: (HasOnset a, HasDuration a) => a -> Time
offsetDefault x = onset x .+^ duration x
                                                 
