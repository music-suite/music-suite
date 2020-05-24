{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.AffineSpace.Point
-- Copyright   :  (c) 2011 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- A type for /points/ (as distinct from vectors), with an appropriate
-- AffineSpace instance.
--
-----------------------------------------------------------------------------

module Data.AffineSpace.Point
       ( -- * Points
         Point(..), unPoint, origin, (*.), mirror,

         -- * Reflection through a point
         relative, relative2, relative3,
         reflectThrough,
       ) where

import           Data.AffineSpace
import           Data.VectorSpace

import           Data.Data        (Data)
import           Data.Typeable    (Typeable)

------------------------------------------------------------
--  Points  ------------------------------------------------
------------------------------------------------------------

-- | @Point@ is a newtype wrapper around vectors used to represent
--   points, so we don't get them mixed up. The distinction between
--   vectors and points is important: translations affect points, but
--   leave vectors unchanged.  Points are instances of the
--   'AffineSpace' class from "Data.AffineSpace".
newtype Point v = P v
  deriving (Eq, Ord, Read, Show, Data, Typeable, Functor)

-- | Convert a point @p@ into the vector from the origin to @p@.  This
--   should be considered a \"semantically unsafe\" operation; think
--   carefully about whether and why you need to use it.  The
--   recommended way to do this conversion would be to write @(p
--   '.-.' 'origin')@.
unPoint :: Point v -> v
unPoint (P v) = v

-- | The origin of the vector space @v@.
origin :: AdditiveGroup v => Point v
origin = P zeroV

instance AdditiveGroup v => AffineSpace (Point v) where
  type Diff (Point v) = v
  P v1 .-. P v2 = v1 ^-^ v2
  P v1 .+^ v2   = P (v1 ^+^ v2)

-- | Scale a point by a scalar.
(*.) :: VectorSpace v => Scalar v -> Point v -> Point v
s *. P v = P (s *^ v)

-- | Reflect a point through the 'origin'.
mirror :: AdditiveGroup v => Point v -> Point v
mirror = reflectThrough origin

-- | Apply a transformation relative to the given point.
relative :: AffineSpace p => p -> (Diff p -> Diff p) -> p -> p
relative p f = (p .+^) . f . (.-. p)

-- | Apply a transformation relative to the given point.
relative2 :: AffineSpace p => p -> (Diff p -> Diff p -> Diff p) -> p -> p -> p
relative2 p f x y = (p .+^) $ f (inj x) (inj y) where inj = (.-. p)

-- | Apply a transformation relative to the given point.
relative3 :: AffineSpace p => p -> (Diff p -> Diff p -> Diff p -> Diff p) -> p -> p -> p -> p
relative3 p f x y z = (p .+^) $ f (inj x) (inj y) (inj z) where inj = (.-. p)

-- | Mirror a point through a given point.
reflectThrough :: AffineSpace p => p -> p -> p
reflectThrough o = relative o negateV

