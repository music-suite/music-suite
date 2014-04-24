
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

module Music.Time.Track (
    -- * Music.Time.Track
    Track,
    -- ** Substructure
    track,
    -- ** TODO
    
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

import           Music.Time.Position
import           Music.Time.Split
import           Music.Time.Reverse
import           Music.Time.Delayed

-----
import Control.Monad.Compose
import Music.Score.Util
import Data.Fixed
import           Data.Default
import           Data.Ratio

import           Control.Applicative
import           Control.Arrow                (first, second, (***), (&&&))
import qualified Control.Category
import           Control.Comonad
import           Control.Comonad.Env
import           Control.Lens                 hiding (Indexable, Level, above,
                                               below, index, inside, parts,
                                               reversed, transform, (|>), (<|))
import           Control.Monad
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Distributive
import           Data.Foldable                (Foldable)
import qualified Data.Foldable                as Foldable
import           Data.Functor.Rep
import qualified Data.List
import           Data.List.NonEmpty           (NonEmpty)
import           Data.Maybe
import           Data.NumInstances
import           Data.Semigroup               hiding ()
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Traversable             (Traversable)
import qualified Data.Traversable             as T
import           Data.Typeable
import           Data.VectorSpace hiding (Sum(..))
import           Music.Dynamics.Literal
import           Music.Pitch.Literal

import qualified Data.Ratio                   as Util_Ratio
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Ord as Ord
-----

-- |
-- A 'Track' is a parallel composition of values.
--
-- @
-- type Track a = [Delayed a]
-- @
--
newtype Track a = Track { getTrack :: TrackList (TrackEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

-- Can use [] or Seq here
type TrackList = []

-- Can use any type as long as trackEv provides an Iso
type TrackEv a = Delayed a

trackEv :: Iso (Delayed a) (Delayed b) (TrackEv a) (TrackEv b)
trackEv = id

instance Applicative Track where
  pure  = return
  (<*>) = ap

instance Monad Track where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (Track a) where
  type Unwrapped (Track a) = (TrackList (TrackEv a))
  _Wrapped' = iso getTrack Track

instance Rewrapped (Track a) (Track b)

instance Transformable (Track a) where
  transform s = over _Wrapped' (transform s)

instance HasDuration (Track a) where
  _duration = Foldable.sum . fmap _duration . view _Wrapped'

instance Splittable a => Splittable (Track a) where
  -- TODO

instance Reversible a => Reversible (Track a) where
  rev = over _Wrapped' (fmap rev) -- TODO OK?

-- TODO
-- instance HasMeta (Track a) where
  -- meta = error "Not implemented: meta" 

-- TODO
-- type instance Pitch (Track a) = Pitch a
-- type instance SetPitch g (Track a) = Track (SetPitch g a)
-- instance (HasPitches a b) => HasPitches (Track a) (Track b) where
--   pitches = _Wrapped . traverse . from trackEv . _Wrapped . whilstLT pitches


track :: Lens (Track a) (Track b) [Delayed a] [Delayed b]
track = _Wrapped 


