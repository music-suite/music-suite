{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

module Music.Time.Duration
  ( module Music.Time.Transform,

    -- * The HasDuration class
    HasDuration (..),

    -- * Absolute duration
    duration,
    stretchToD,
  )
where

import Control.Lens hiding
  ( (<|),
    Indexable,
    Level,
    below,
    index,
    inside,
    parts,
    reversed,
    transform,
    (|>),
  )
import Control.Lens (Getter)
import Data.Functor.Contravariant (Op (..))
import Data.NumInstances ()
import Data.Semigroup hiding ()
import Data.VectorSpace hiding (Sum (..))
import Music.Time.Transform

-- |
-- Class of values that have a duration.
--
-- ==== Laws
--
-- [/transformable-duration/]
--
-- For types that are also 'Transformable':
--
--    @'_duration' ('transform' s x) = 'transform' s ('_duration' x)@
class HasDuration a where

  -- | Return the duration of a value.
  _duration :: a -> Duration

  {-# MINIMAL _duration #-}

instance HasDuration () where
  _duration _ = 1

instance HasDuration Char where
  _duration _ = 1

instance HasDuration Int where
  _duration _ = 1

instance HasDuration Double where
  _duration _ = 1

instance HasDuration Duration where
  _duration = id

instance HasDuration Span where
  _duration = snd . view onsetAndDuration

instance HasDuration a => HasDuration (Product a) where
  _duration (Product x) = _duration x

instance HasDuration a => HasDuration (Sum a) where
  _duration (Sum x) = _duration x

instance HasDuration a => HasDuration (Min a) where
  _duration (Min x) = _duration x

instance HasDuration a => HasDuration (Max a) where
  _duration (Max x) = _duration x

instance (HasDuration a, HasDuration b) => HasDuration (Either a b) where
  _duration (Left x) = _duration x
  _duration (Right x) = _duration x

-- |
-- Stretch a value to have the given duration.
stretchToD :: (Transformable a, HasDuration a) => Duration -> a -> a
stretchToD d x = (d ^/ _duration x) `stretch` x

-- |
-- Access the duration.
duration :: (Transformable a, HasDuration a) => Getter a Duration
duration = to _duration
{-# INLINE duration #-}
{-# DEPRECATED duration "Use _duration" #-}
