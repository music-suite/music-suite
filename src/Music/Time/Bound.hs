
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
-------------------------------------------------------------------------------------

module Music.Time.Bound (
    -- * Bound type
    Bound(..),
    
    -- * Construction
    bounds,
    bounding,
    -- trim,
    -- splice,
    -- bounded',
    -- bounded,
  ) where

import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Bifunctor
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Ratio
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.VectorSpace

import           Music.Time.Reverse
import           Music.Time.Split

import           Control.Applicative
import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))
import           Data.Typeable

-- |
-- 'Bound' restricts the start and stop time of a value, and prevents access to values
-- outside the bounds.
--
-- 'Bound' is especially useful to restrict the range of a 'Behavior'. If we have a
-- value with can only be reasonably defined for a particular time range, we can
-- represent it as 'Bound' 'Behavior'. This is isomorphic to a 'Note' 'Segment', and
-- 'bounded' whitnesses the isomorphism.
--
-- 'Bound' is not 'Foldable' or 'Traversable', as that would allow us to access values
-- outside the bounds. However, we can still access values of a 'Bound' 'Behavior' in a
-- safe manner using 'trim' or 'splice'.
--
newtype Bound a = Bound { getBound :: (Span, a) }
  deriving (Functor, Semigroup, Typeable, Eq, Show)

-- $semantics Bound
--
-- @
-- type Bound a = (Time, Time, a)
-- @
--

--
-- These are both unsafe, as they allow us to define 'unBound'
--
-- instance Foldable Bound where
--   foldr f z (Bound (_,x)) = f x z
--
-- instance Traversable Bound where
--   traverse f (Bound (s,x)) = (Bound . (s,)) <$> f x
--

--
-- TODO define Applicative/Monad
--
--
-- This is a Writer-style instance with interval arithmetic style union/empty as the Monoid
-- A possible problem with this is that there are multiple representations of the empty
-- set (namely [(t, t)^.from range | t <- {Time} ]).
--

instance Wrapped (Bound a) where
  type Unwrapped (Bound a) = (Span, a)
  _Wrapped' = iso getBound Bound

instance Rewrapped (Bound a) (Bound b)

instance Reversible a => Reversible (Bound a) where
  -- rev = over (_Wrapped . each) rev
  rev = over _Wrapped $ (bimap rev rev)

instance (HasPosition a, Splittable a) => Splittable (Bound a) where
  -- TODO

-- |
-- 'Bound' transform by transforming the bounded value as well as
-- the bounds.
--
instance Transformable a => Transformable (Bound a) where
  transform t = over _Wrapped (transform t `bimap` transform t)

instance (HasPosition a, HasDuration a) => HasDuration (Bound a) where
  _duration x = _offset x .-. _onset x

instance HasPosition a => HasPosition (Bound a) where
  -- TODO lawless
  -- _position (Bound (view range -> (t, u), x)) d = truncating t u (_position x d)
  _position (Bound (view range -> (t, u), x)) = alerp t u

truncating :: Ord a => a -> a -> a -> a
truncating t u x = (x `max` t) `min` u

-- |
-- Add bounds.
--
bounds :: Time -> Time -> a -> Bound a
bounds t u x = Bound (t <-> u, x)

-- |
-- Add bounds.
--
-- @
-- (s,x)^.note = (bounding s . transform s) x
-- @
--
bounding :: Span -> a -> Bound a
bounding (view range -> (t, u)) = bounds t u

