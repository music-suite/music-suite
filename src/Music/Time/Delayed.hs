
{-# LANGUAGE StandaloneDeriving #-}
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

module Music.Time.Delayed (
      -- * Delayed type
      Delayed,

      -- * Construction
      delayed,

      -- ** Inspecting delayed values
      delayedValue,
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

import           Music.Time.Reverse
import           Music.Time.Split

import           Control.Applicative
import           Control.Arrow          (first, second, (&&&), (***))
import           Control.Comonad
import           Control.Comonad.Env
import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as Foldable
import           Data.PairMonad
import           Data.Typeable

import           Music.Dynamics.Literal
import           Music.Pitch.Literal


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
newtype Delayed a = Delayed   { _delayedValue :: (Time, a) }
  deriving (Eq, {-Ord, -}{-Show, -}
            Applicative, Monad, {-Comonad, -}
            Functor,  Foldable, Traversable, Typeable)

deriving instance Show a => Show (Delayed a)

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (Delayed a) where
  type Unwrapped (Delayed a) = (Time, a)
  _Wrapped' = iso _delayedValue Delayed

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
delayedValue :: (Transformable a, Transformable b)
  => Lens
      (Delayed a) (Delayed b)
      a b
delayedValue = lens runDelayed (flip $ _delayed . const)
  where
    _delayed f (Delayed (t,x)) = Delayed (t, f `whilst` delaying (t .-. 0) $ x)
{-# INLINE delayedValue #-}


runDelayed :: Transformable a => Delayed a -> a
runDelayed = uncurry delayTime . view _Wrapped


deriving instance IsPitch a => IsPitch (Delayed a)	 
deriving instance IsInterval a => IsInterval (Delayed a)	 
deriving instance IsDynamics a => IsDynamics (Delayed a)

