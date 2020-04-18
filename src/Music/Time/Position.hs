{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

-- |
-- Provides a way to query a value for its 'position'.
module Music.Time.Position
  ( module Music.Time.Duration,

    -- * The HasPosition class
    HasPosition (..),
    HasPosition1 (..),
    _position,
    _position1,

    -- * Position and Era
    position,
    era,
    setEra,

    -- ** Specific positions
    onset,
    midpoint,
    offset,
    preOnset,
    postOffset,

    -- * Moving
    startAt,
    stopAt,
    placeAt,

    -- * Transforming
    stretchTo,

    -- * Transforming relative a position
    stretchRelative,
    stretchRelativeOnset,
    stretchRelativeMidpoint,
  )
where

-- stretchRelativeMidpoint,
-- stretchRelativeOffset,
-- transformRelative,
-- transformRelativeOnset,
-- transformRelativeMidpoint,
-- transformRelativeOffset,

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
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.VectorSpace hiding (Sum)
import Music.Time.Duration
import Music.Time.Internal.Util

-- |
-- Class of values that have a position in time.
--
-- Many values such as notes, envelopes etc can in fact have many positions such as onset,
-- attack point, offset, decay point time etc. Rather than having separate methods for a
-- fixed set of cases, this class provides an interpolation from a /local/ position to
-- a /global/ position. While the local position goes from zero to one, the global position
-- goes from the 'onset' to the 'offset' of the value.
--
-- Laws:
--
-- For 'HasDuration' instances:
--
-- @
-- fmap _duration . _era = Just . _duration
-- @
--
-- For 'Transformable' instances:
--
-- @
-- _era (transform s x) = fmap (transform s) (_era x)
-- @
class HasPosition a where
  -- | Return the conventional bounds of a value (local time zero and one).
  _era :: a -> Maybe Span

instance HasPosition Span where
  _era = Just

class HasPosition a => HasPosition1 a where
  _era1 :: a -> Span

instance HasPosition1 Span where
  _era1 = id

-- | Map a local time in value to global time.
_position :: HasPosition a => a -> Duration -> Maybe Time
_position x d = do
  e <- _era x
  let (a, b) = e ^. onsetAndOffset
  pure $ alerp a b d

-- | Map a local time in value to global time.
_position1 :: HasPosition1 a => a -> Duration -> Time
_position1 x d =
  let e = _era1 x
      (a, b) = e ^. onsetAndOffset
   in alerp a b d

-- |
-- Position of the given value.
position :: (HasPosition1 a, Transformable a) => Duration -> Lens' a Time
position d = lens (`_position1` d) (flip $ placeAt d)
{-# INLINEABLE position #-}

-- |
-- Onset of the given value, corresponding to alignment @0@.
onset :: (HasPosition1 a, Transformable a) => Lens' a Time
onset = position 0
{-# INLINEABLE onset #-}

-- |
-- Offset of the given value, corresponding to alignment @1@.
offset :: (HasPosition1 a, Transformable a) => Lens' a Time
offset = position 1
{-# INLINEABLE offset #-}

-- |
-- Pre-onset of the given value, or the value right before the attack phase.
preOnset :: (HasPosition1 a, Transformable a) => Lens' a Time
preOnset = position (-0.5)
{-# INLINEABLE preOnset #-}

-- |
-- Midpoint of the given value, or the value between the decay and sustain phases.
midpoint :: (HasPosition1 a, Transformable a) => Lens' a Time
midpoint = position 0.5
{-# INLINEABLE midpoint #-}

-- |
-- Post-offset of the given value, or the value right after the release phase.
postOffset :: (HasPosition1 a, Transformable a) => Lens' a Time
postOffset = position 1.5
{-# INLINEABLE postOffset #-}

-- |
-- Move a value forward in time.
startAt :: (Transformable a, HasPosition a) => Time -> a -> a
startAt = placeAt 0

-- |
-- Move a value forward in time.
stopAt :: (Transformable a, HasPosition a) => Time -> a -> a
stopAt = placeAt 1

-- |
-- Align a value to a given position.
--
-- @placeAt p t@ places the given thing so that its position p is at time t
--
-- @
-- 'placeAt' 0 = 'startAt'
-- 'placeAt' 1 = 'stopAt'
-- @
placeAt :: (Transformable a, HasPosition a) => Alignment -> Time -> a -> a
placeAt p t x = case x `_position` p of
  Nothing -> x
  Just xp -> (t .-. xp) `delay` x

-- |
-- Stretch a value to have the given duration.
stretchTo :: (Transformable a, HasPosition a) => Duration -> a -> a
stretchTo d x = case _era x of
  Nothing -> x
  Just e -> (d ^/ _duration e) `stretch` x
{-# INLINE stretchTo #-}

{-
_onset, _offset :: (HasPosition a, Transformable a) => a -> Time
_onset = (`_position` 0)
_offset = (`_position` 1.0)

-}

-- |
-- Place a value over the given span.
--
-- @placeAt s t@ places the given thing so that @x^.place = s@
setEra :: (HasPosition a, Transformable a) => Span -> a -> a
setEra s x = case _era x of
  Nothing -> x
  Just e ->
    transform (s ^-^ e) x

era :: (HasPosition1 a, Transformable a) => Lens' a Span
era = lens _era1 $ flip setEra

-- |
-- Stretch a value relative to its local origin.
--
-- @
-- stretchRelativeOnset    = stretchRelative 0
-- stretchRelativeMidpoint = stretchRelative 0.5
-- stretchRelativeOffset   = stretchRelative 1
-- @
stretchRelative :: (HasPosition a, Transformable a) => Alignment -> Duration -> a -> a
stretchRelative p n x = case _era x of
  Nothing -> x
  Just e -> over (transformed $ undelaying (realToFrac $ e ^. position p)) (stretch n) x

-- |
-- Stretch a value relative to its onset.
--
-- >>> stretchRelativeOnset 2 (0 <-> 1)
-- 0 <-> 2
stretchRelativeOnset :: (HasPosition a, Transformable a) => Duration -> a -> a
stretchRelativeOnset = stretchRelative 0
-- |
-- Stretch a value relative to its midpoint.
--
-- >>> stretchRelativeMidpoint 2 (1 <-> 3)
-- 0 <-> 4
stretchRelativeMidpoint :: (HasPosition a, Transformable a) => Duration -> a -> a
stretchRelativeMidpoint = stretchRelative 0.5

{-
-- |
-- Stretch a value relative to its offset.
--
-- >>> stretchRelativeOffset 2 (1 <-> 2)
-- 0 <-> 2
stretchRelativeOffset :: (HasPosition a, Transformable a) => Duration -> a -> a
stretchRelativeOffset = stretchRelative 1

-- |
-- Transform a value relative to its local origin.
--
-- @
-- stretchRelativeOnset    = stretchRelative 0
-- stretchRelativeMidpoint = stretchRelative 0.5
-- stretchRelativeOffset   = stretchRelative 1
-- @
transformRelative :: (HasPosition a, Transformable a) => Alignment -> Span -> a -> a
transformRelative p n x = over (transformed $ undelaying (realToFrac $ x ^. position p)) (transform n) x

-- |
-- Transform a value relative to its local origin.
--
-- @
-- stretchRelativeOnset    = stretchRelative 0
-- stretchRelativeMidpoint = stretchRelative 0.5
-- stretchRelativeOffset   = stretchRelative 1
-- @
transformRelativeOnset :: (HasPosition a, Transformable a) => Span -> a -> a
transformRelativeOnset = transformRelative 0

-- |
-- Transform a value relative to its local origin.
--
-- @
-- stretchRelativeOnset    = stretchRelative 0
-- stretchRelativeMidpoint = stretchRelative 0.5
-- stretchRelativeOffset   = stretchRelative 1
-- @
transformRelativeMidpoint :: (HasPosition a, Transformable a) => Span -> a -> a
transformRelativeMidpoint = transformRelative 0.5

-- |
-- Transform a value relative to its local origin.
--
-- @
-- stretchRelativeOnset    = stretchRelative 0
-- stretchRelativeMidpoint = stretchRelative 0.5
-- stretchRelativeOffset   = stretchRelative 1
-- @
transformRelativeOffset :: (HasPosition a, Transformable a) => Span -> a -> a
transformRelativeOffset = transformRelative 1
-}
