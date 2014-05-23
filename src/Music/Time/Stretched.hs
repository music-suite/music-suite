
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

module Music.Time.Stretched (
    -- * Stretched values
    Stretched,

    -- * Construction
    stretched,
    stretchedValue,
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

-- |
-- A 'Stretched' value has a known 'duration', but no 'position'.
--
-- Placing a value inside 'Stretched' makes it invariante under 'delay'.
--
-- The semantics are given by
--
-- @
-- type Stretched = (Duration, a)
-- @
--
newtype Stretched a = Stretched { _stretchedValue :: (Duration, a) }
  deriving (Eq, {-Ord, -}{-Show, -}
            Applicative, Monad, {-Comonad, -}
            Functor,  Foldable, Traversable)

-- >>> stretch 2 $ (5,1)^.stretched
-- (10,1)^.stretched
--
-- >>> delay 2 $ (5,1)^.stretched
-- (5,1)^.stretched
--

deriving instance Typeable1 Stretched

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (Stretched a) where
  type Unwrapped (Stretched a) = (Duration, a)
  _Wrapped' = iso _stretchedValue Stretched

instance Rewrapped (Stretched a) (Stretched b)

instance Transformable (Stretched a) where
  transform t = over _Wrapped $ first (transform t)

instance HasDuration (Stretched a) where
  _duration = _duration . ask . view _Wrapped

instance Reversible (Stretched a) where
  rev = stretch (-1)

instance Splittable a => Splittable (Stretched a) where
  beginning d = over _Wrapped $ \(s, v) -> (beginning d s, beginning d v)
  ending    d = over _Wrapped $ \(s, v) -> (ending    d s, ending    d v)

deriving instance Show a => Show (Stretched a)

-- |
-- View a stretched value as a pair of the original value and a stretch factor.
--
stretched :: Iso (Duration, a) (Duration, b) (Stretched a) (Stretched b)
stretched = _Unwrapped


-- |
-- View a stretched value as a pair of the original value and the transformation (and vice versa).
--
stretchedValue :: (Transformable a, Transformable b) => Lens (Stretched a) (Stretched b) a b
stretchedValue = lens runStretched (flip $ _stretched . const)
  where
    _stretched f (Stretched (d,x)) = Stretched (d, f `whilst` stretching d $ x)
{-# INLINE stretchedValue #-}

runStretched :: Transformable a => Stretched a -> a
runStretched = uncurry stretch . view _Wrapped
