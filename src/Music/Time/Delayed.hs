
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

module Music.Time.Delayed (
    -- * Music.Time.Delayed
    Delayed,
    delayed,
    getDelayed,
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

import           Music.Time.Split
import           Music.Time.Reverse

-----
import Data.PairMonad
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
-- 'Delayed' represents a value with an offset in time.
--
-- A delayed value has a known 'position', but no 'duration'.
--
-- Placing a value inside 'Delayed' does not make it invariant under 'stretch', as the
-- offset of a delayed value may be stretched with respect to the origin. However, in
-- contrast to a note the /duration/ is not stretched.
--
-- The semantics are given by
--
-- @
-- type Delayed a = (Time, a)
-- @
--
newtype Delayed a = Delayed   { _getDelayed :: (Time, a) }
  deriving (Eq, {-Ord, -}{-Show, -}
            Applicative, Monad, {-Comonad, -}
            Functor,  Foldable, Traversable)

deriving instance Typeable1 Delayed
deriving instance Show a => Show (Delayed a)

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (Delayed a) where
  type Unwrapped (Delayed a) = (Time, a)
  _Wrapped' = iso _getDelayed Delayed

instance Rewrapped (Delayed a) (Delayed b)

instance Transformable (Delayed a) where
  transform t = over _Wrapped $ first (transform t)

instance HasDuration (Delayed a) where
  _duration x = _offset x .-. _onset x

instance HasPosition (Delayed a) where
  x `_position` p = ask (view _Wrapped x) `_position` p

instance Reversible (Delayed a) where
  rev = revDefault

instance Splittable a => Splittable (Delayed a) where
  -- FIXME

-- |
-- View a delayed value as a pair of a the original value and a delay time.
--
delayed :: Iso (Time, a) (Time, b) (Delayed a) (Delayed b)
delayed = _Unwrapped


-- |
-- View a delayed value as a pair of the original value and the transformation (and vice versa).
--
getDelayed :: (Transformable a, Transformable b) 
  => Lens 
      (Delayed a) (Delayed b) 
      a b
getDelayed = lens runDelayed (flip $ _delayed . const)
  where
    _delayed f (Delayed (t,x)) = 
      Delayed (t, f `whilstDelay` t $ x)
{-# INLINE getDelayed #-}


runDelayed :: Transformable a => Delayed a -> a
runDelayed = uncurry delayTime . view _Wrapped
