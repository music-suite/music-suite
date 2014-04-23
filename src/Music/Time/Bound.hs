
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

module Music.Time.Bound (
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

import           Music.Time.Types
import           Music.Time.Transform
import           Music.Time.Position
import           Music.Time.Duration
import           Music.Time.Split
import           Music.Time.Reverse

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
-- The semantics are given by
--
-- @
-- type Bound a = (Time, Time, a)
-- @
--
newtype Bound a = Bound { getBound :: (Span, a) }
  deriving (Functor, Semigroup, Typeable, Eq, Show)

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

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (Bound a) where
  type Unwrapped (Bound a) = (Span, a)
  _Wrapped' = iso getBound Bound

instance Rewrapped (Bound a) (Bound b)

instance Reversible a => Reversible (Bound a) where
  rev = over _Wrapped $ \(s,x) -> (rev s, rev x)

instance (HasPosition a, Splittable a) => Splittable (Bound a) where
  -- TODO

-- |
-- 'Bound' transform by transforming the bounded value as well as
-- the bounds.
--
instance Transformable a => Transformable (Bound a) where
  transform t = over _Wrapped (transform t *** transform t)

instance (HasPosition a, HasDuration a) => HasDuration (Bound a) where
  _duration x = _offset x .-. _onset x

instance HasPosition a => HasPosition (Bound a) where
  -- TODO lawless
  -- _position (Bound (view range -> (t, u), x)) d = truncating t u (_position x d)
  _position (Bound (view range -> (t, u), x)) d = alerp t u d

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

-- |
-- View a 'Note' 'Segment' as a 'Bound' 'Behavior' and vice versa.
--
-- This can be used to safely turn a behavior into a segment and vice
-- versa. Often 'focusing' is more convenient to use.
--
bounded' :: Iso'
  (Note (Segment a))
  (Bound (Behavior a))
bounded' = bounded

-- |
-- View a 'Note' 'Segment' as a 'Bound' 'Behavior' and vice versa.
--
-- This can be used to safely turn a behavior into a segment and vice
-- versa. Often 'focusing' is more convenient to use.
--
bounded :: Iso
  (Note (Segment a))
  (Note (Segment b))
  (Bound (Behavior a))
  (Bound (Behavior b))
bounded = iso ns2bb bb2ns 
  where
    bb2ns (Bound (s, x)) = view note (s, b2s $ transform (negateV s) $ x)
    ns2bb (view (from note) -> (s, x)) = Bound (s,       transform s           $ s2b $ x)
    s2b = under tabulated (. realToFrac)
    b2s = under tabulated (. realToFrac)

--
-- Note that the isomorhism only works because of 'Bound' being abstract.
-- A function @unBound :: Bound a -> a@ could break the isomorphism
-- as follows:
--
-- > (unBound . view (from bounded . bounded) . bounds 0 1) b ! 2
-- *** Exception: Outside 0-1
--

-- |
-- Extract a bounded behavior, replacing all values outside the bound with 'mempty'.
--
-- @
-- 'trim'   = 'splice' 'mempty'
-- 'trim' x = 'trimBefore' '_onset' x . 'trimAfter' '_offset' x
-- @
--
trim :: Monoid b => Bound (Behavior b) -> Behavior b
trim = trimG
  where
    trimG :: (Monoid b, Representable f, Rep f ~ Time) => Bound (f b) -> f b
    trimG (Bound (s, x)) = tabulate (trimOutside s) `apRep` x

trimOutside :: Monoid a => Span -> Time -> a -> a
trimOutside s t x = if t `inside` s then x else mempty

-- |
-- Inserts a bounded behavior on top of another behavior.
--
-- @
-- 'trim' = 'splice' 'mempty'
-- @
--
-- (Named after the analogous tape-editing technique.)
--
splice :: Behavior a -> Bound (Behavior a) -> Behavior a
splice constant insert = fmap fromLast $ fmap toLast constant <> trim (fmap (fmap toLast) insert)
  where
    toLast   = Option . Just . Last
    fromLast = getLast . fromJust . getOption
    -- fromJust is safe here, as toLast is used to create the Maybe wrapper

