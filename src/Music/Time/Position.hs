
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
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

module Music.Time.Position (
      module Music.Time.Duration,

      -- * Music.Time.Position
      -- * The HasPosition class
      HasPosition(..),
      -- * Inspecting position
      era,
      position,

      -- * Specific positions
      onset,
      offset,
      preOnset,
      postOnset,
      postOffset,

      -- * Moving to absolute positions
      startAt,
      stopAt,
      placeAt,
      -- TODO
      _placeAt,
      _era,
  ) where


import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.VectorSpace       hiding (Sum)

import           Music.Time.Duration
import           Music.Time.Util

import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))

-- |
-- Class of values that have a position in time.
--
-- Many values such as notes, envelopes etc can in fact have many positions such as onset,
-- attack point, offset, decay point time etc. Rather than having separate methods for a
-- discrete set of cases, this class provides an interpolation from a /local/ position to
-- a /global/ position. While the local position goes from 0 to 1, the global position
-- goes from the 'onset' to the 'offset' of the value.
--
-- For instantaneous values, a suitable instance is:
--
-- @
-- '_position' x = 'const' t
-- @
--
-- For values with an onset and offset we can use 'alerp':
--
-- @
-- '_position' x = 'alerp' ('_onset' x) ('_offset' x)
-- @
--
class HasDuration a => HasPosition a where
  -- |
  -- Return the onset of the given value, or the value between the attack and decay phases.
  --
  _position :: a -> Duration -> Time
  _position x = alerp (_onset x) (_offset x)

  -- |
  -- Return the onset of the given value, or the value between the attack and decay phases.
  --
  _onset, _offset :: a -> Time
  _onset     = (`_position` 0)
  _offset    = (`_position` 1.0)

instance HasPosition Time where
  _position = const

instance HasPosition Span where
  -- Override as an optimization:
  _onset    (view range -> (t1, t2)) = t1
  _offset   (view range -> (t1, t2)) = t2
  _position (view range -> (t1, t2)) = alerp t1 t2

instance (HasPosition a, HasDuration a) => HasDuration [a] where
  _duration x = _offset x .-. _onset x

instance (HasPosition a, HasDuration a) => HasPosition [a] where
  _onset  = foldr min 0 . fmap _onset
  _offset = foldr max 0 . fmap _offset

_era :: HasPosition a => a -> Span
_era x = _onset x <-> _offset x
{-# INLINE _era #-}

-- |
-- Position of the given value.
--
position :: (HasPosition a, Transformable a) => Duration -> Lens' a Time
position d = lens (`_position` d) (flip $ placeAt d)
{-# INLINE position #-}

-- |
-- Onset of the given value.
--
onset :: (HasPosition a, Transformable a) => Lens' a Time
onset = position 0
{-# INLINE onset #-}

-- |
-- Onset of the given value.
--
offset :: (HasPosition a, Transformable a) => Lens' a Time
offset = position 1
{-# INLINE offset #-}

-- |
-- Pre-onset of the given value, or the value right before the attack phase.
--
preOnset :: (HasPosition a, Transformable a) => Lens' a Time
preOnset = position (-0.5)
{-# INLINE preOnset #-}

-- |
-- Post-onset of the given value, or the value between the decay and sustain phases.
--
postOnset :: (HasPosition a, Transformable a) => Lens' a Time
postOnset = position 0.5
{-# INLINE postOnset #-}

-- |
-- Post-offset of the given value, or the value right after the release phase.
--
postOffset :: (HasPosition a, Transformable a) => Lens' a Time
postOffset = position 1.5
{-# INLINE postOffset #-}



-- |
-- Move a value forward in time.
--
startAt :: (Transformable a, HasPosition a) => Time -> a -> a
startAt t x = (t .-. _onset x) `delay` x

-- |
-- Move a value forward in time.
--
stopAt  :: (Transformable a, HasPosition a) => Time -> a -> a
stopAt t x = (t .-. _offset x) `delay` x

-- |
-- Align a value to a given position.
--
-- @placeAt p t@ places the given thing so that its position p is at time t
--
-- @
-- 'placeAt' 0 = 'startAt'
-- 'placeAt' 1 = 'stopAt'
-- @
--
placeAt :: (Transformable a, HasPosition a) => Duration -> Time -> a -> a
placeAt p t x = (t .-. x `_position` p) `delay` x

-- |
-- Place a value over the given span.
--
-- @placeAt s t@ places the given thing so that @x^.place = s@
--
_placeAt :: (HasPosition a, Transformable a) => Span -> a -> a
_placeAt s x = transform (s ^-^ view era x) x

-- |
-- A lens to the position
--
era :: (HasPosition a, Transformable a) => Lens' a Span
era = lens _era (flip _placeAt)
{-# INLINE era #-}





{-

-- |
-- Whether this is a proper span, i.e. whether @'_onset' x '<' '_offset' x@.
--
isProper :: Span -> Bool
isProper (view range -> (t, u)) = t < u

-- |
-- Whether the given point falls inside the given span (inclusively).
--
-- Designed to be used infix, for example
--
-- @
-- 0.5 ``inside`` (1 '<->' 2)
-- @
--
inside :: Time -> Span -> Bool
inside x (view range -> (t, u)) = t <= x && x <= u

-- |
-- Whether the given
--
encloses :: Span -> Span -> Bool
a `encloses` b = _onset b `inside` a && _offset b `inside` a

-- |
-- Whether the given
--
overlaps :: Span -> Span -> Bool
a `overlaps` b = not (a `isBefore` b) && not (b `isBefore` a)

isBefore :: Span -> Span -> Bool
a `isBefore` b = (_onset a `max` _offset a) <= (_onset b `min` _offset b)

-}

{-
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

-}




