
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
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
-------------------------------------------------------------------------------------

module Music.Time.Track (

        -- * Track type

        Track,

        -- * Construction
        track,
        placeds,

  ) where


import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Ratio
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.VectorSpace

import           Music.Time.Placed
import           Music.Time.Reverse
import           Music.Time.Split

import           Control.Applicative
import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))
import           Control.Monad
import           Control.Monad.Compose
import           Control.Monad.Plus
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as Foldable
import           Data.Traversable       (Traversable)
import qualified Data.Traversable       as T
import           Data.Typeable
import           Music.Time.Internal.Util

-- |
-- A 'Track' is a parallel composition of values.
--
-- @
-- type Track a = [Placed a]
-- @
--
newtype Track a = Track { getTrack :: TrackList (TrackEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)
-- {-# DEPRECATED Track "Use 'Chord'" #-}

-- A track is a list of events with explicit onset.
--
-- Track is a 'Monoid' under parallel composition. 'mempty' is the empty track
-- and 'mappend' interleaves values.
--
-- Track is a 'Monad'. 'return' creates a track containing a single value at time
-- zero, and '>>=' transforms the values of a track, allowing the addition and
-- removal of values relative to the time of the value. Perhaps more intuitively,
-- 'join' delays each inner track to start at the offset of an outer track, then
-- removes the intermediate structure.

-- Can use [] or Seq here
type TrackList = []

-- Can use any type as long as trackEv provides an Iso
type TrackEv a = Placed a

trackEv :: Iso (Placed a) (Placed b) (TrackEv a) (TrackEv b)
trackEv = id

instance Applicative Track where
  pure  = return
  (<*>) = ap

instance Alternative Track where
  (<|>) = (<>)
  empty = mempty

instance Monad Track where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

instance Wrapped (Track a) where
  type Unwrapped (Track a) = (TrackList (TrackEv a))
  _Wrapped' = iso getTrack Track

instance Rewrapped (Track a) (Track b)

instance Transformable a => Transformable (Track a) where
  transform s = over _Wrapped' (transform s)

-- instance Splittable a => Splittable (Track a) where
--   -- TODO

-- instance Reversible a => Reversible (Track a) where
--   -- TODO


-- |
-- Create a track from a list of notes.
--
-- Se also 'placeds'.
--
track :: Getter [Placed a] (Track a)
track = from unsafeTrack
{-# INLINE track #-}

placeds :: Lens (Track a) (Track b) [Placed a] [Placed b]
placeds = unsafeTrack
{-# INLINE placeds #-}

unsafeTrack :: Iso (Track a) (Track b) [Placed a] [Placed b]
unsafeTrack = _Wrapped
{-# INLINE unsafeTrack #-}

