{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fno-warn-unused-top-binds
  -fno-warn-redundant-constraints
  -fno-warn-unused-matches
  -fno-warn-unused-imports #-}
module Music.Time.Internal.Track
  ( -- * Track type
    Track,

    -- * Construction
    track,
    placeds,
  )
where

import Control.Applicative
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
import Control.Monad
import Control.Monad.Plus
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import qualified Data.Traversable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Typeable
import Data.VectorSpace
import Music.Time.Internal.Util
import Music.Time.Juxtapose
import Music.Time.Internal.Placed

-- |
-- A 'Track' is a parallel composition of values.
newtype Track a = Track {getTrack :: TrackList (TrackEv a)}
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

type TrackList = []

type TrackEv a = Placed a

trackEv :: Iso (Placed a) (Placed b) (TrackEv a) (TrackEv b)
trackEv = id

instance Applicative Track where

  pure = return

  (<*>) = ap

instance Alternative Track where

  (<|>) = (<>)

  empty = mempty

instance Monad Track where

  return = view _Unwrapped . return . return

  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs
    where
      mbind = (concat .) . fmap . (fmap join .) . Data.Traversable.traverse

instance Wrapped (Track a) where

  type Unwrapped (Track a) = (TrackList (TrackEv a))

  _Wrapped' = iso getTrack Track

instance Rewrapped (Track a) (Track b)

instance Transformable a => Transformable (Track a) where
  transform s = over _Wrapped' (transform s)

-- | Create a track from a list of notes.
track :: Getter [Placed a] (Track a)
track = from trackIgnoringMeta
{-# INLINE track #-}

placeds :: Lens (Track a) (Track b) [Placed a] [Placed b]
placeds = trackIgnoringMeta
{-# INLINE placeds #-}

trackIgnoringMeta :: Iso (Track a) (Track b) [Placed a] [Placed b]
trackIgnoringMeta = _Wrapped
{-# INLINE trackIgnoringMeta #-}
