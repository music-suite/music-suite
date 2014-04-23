
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

module Music.Time.Stretched (
    -- * Music.Time.Stretched
    Stretched,
    stretched,
    getStretched,
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
newtype Stretched a = Stretched { _getStretched :: (Duration, a) }
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
  _Wrapped' = iso _getStretched Stretched

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
getStretched :: (Transformable a, Transformable b) => Lens (Stretched a) (Stretched b) a b
getStretched = lens runStretched (flip $ _stretched . const)
  where
    _stretched f (Stretched (d,x)) = 
      Stretched (d, f `whilstStretch` d $ x)
{-# INLINE getStretched #-}

runStretched :: Transformable a => Stretched a -> a
runStretched = uncurry stretch . view _Wrapped
