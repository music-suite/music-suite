
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

module Music.Time.Duration (
      -- * Music.Time.Duration
      -- * The HasDuration class
      HasDuration(..),
      -- * Stretching to absolute duration
      duration,
      stretchTo,
  ) where

import Music.Time.Types
import           Music.Time.Transform

-----
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
-- Class of values that have a duration.
--
-- Law Duration
--
-- @
-- '_duration' x = ('offset' x '.-.' 'onset' x)
-- @
--
class HasDuration a where
  _duration :: a -> Duration

instance HasDuration Time where
  _duration = 0

instance HasDuration Duration where
  _duration = id

instance HasDuration Span where
  _duration = snd . view delta

--
-- By convention, we treat pairs and triplets as having the form
-- (t,x), (d,x) and (t,d,x) where t has a position and d has a 
-- duration. This makes it convenient to represent simple event
-- lists as [(Time, Duration, a)] without needing any special
-- structure.
--

instance HasDuration a => HasDuration (a, b) where
  _duration (d,_) = _duration d

instance HasDuration b => HasDuration (a, b, c) where
  _duration (_,d,_) = _duration d

instance HasDuration a => HasDuration (Product a) where
  _duration (Product x) = _duration x

instance HasDuration a => HasDuration (Sum a) where
  _duration (Sum x) = _duration x

instance HasDuration a => HasDuration (Min a) where
  _duration (Min x) = _duration x

instance HasDuration a => HasDuration (Max a) where
  _duration (Max x) = _duration x


-- |
-- Access the duration.
--
duration :: (Transformable a, HasDuration a) => Lens' a Duration
duration = lens _duration (flip stretchTo)
{-# INLINE duration #-}

-- |
-- Stretch a value to have the given duration.
--
stretchTo :: (Transformable a, HasDuration a) => Duration -> a -> a
stretchTo d x = (d ^/ _duration x) `stretch` x

