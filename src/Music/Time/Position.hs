
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

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

      -- * Internal
      -- TODO hide...
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


