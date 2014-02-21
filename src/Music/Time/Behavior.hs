
{-# LANGUAGE 
    ScopedTypeVariables, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds, 
    StandaloneDeriving,
    GADTs, 
    ViewPatterns,
    TypeFamilies, 
    MultiParamTypeClasses, 
    FlexibleInstances #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Behaviors, or time-varying values.
--
-- TODO integrate better in the library
--
-------------------------------------------------------------------------------------

module Music.Time.Behavior (
        Behavior,
        constant,
        behavior,
        varying,
        varyingIn,
        -- time,
        switchB,
        trimBeforeB,
  ) where

import Prelude hiding (trimAfter)

import Control.Lens
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Plus       
import Control.Monad.Compose
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.NumInstances()
import Data.Typeable
import Data.Semigroup
import Data.Profunctor
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Music.Time.Time
import Music.Time.Delayable
import Music.Time.Stretchable
import Music.Time.Span
import Music.Time.Time
import Music.Time.Reactive

-- Inner TFun focuses on [0..1]
newtype Behavior a = Behavior { getBehavior :: Reactive (Time -> a) }
    deriving (Functor, Semigroup, Monoid)

instance Delayable (Behavior a) where
    delay n (Behavior x) = Behavior (fmap (delay n) $ delay n x)

instance Stretchable (Behavior a) where
    stretch n (Behavior x) = Behavior (fmap (stretch n) $ stretch n x)

instance Wrapped (Reactive (Time -> a)) (Reactive (Time -> a)) (Behavior a) (Behavior a) where
    wrapped = iso Behavior getBehavior

instance Applicative Behavior where
    pure    = (^. wrapped) . pure . pure
    ((^. unwrapped) -> f) <*> ((^. unwrapped) -> x) = (^. wrapped) $ liftA2 (<*>) f x

instance HasBehavior Behavior where
    (?) = behAt

deriving instance Num a => Num (Behavior a)
deriving instance Fractional a => Fractional (Behavior a)
deriving instance Floating a => Floating (Behavior a)
    
-- instance HasPitch (Behavior a) where
    -- type Pitch (Behavior a) = Behavior a
    -- type SetPitch g (Behavior a) = g
    -- getPitches = return
    -- mapPitch = id

-- | A constant (non-varying) behavior.
--   
--   Identical to @behavior . const@ but creates more efficient behaviors.
constant :: a -> Behavior a
constant = (^. wrapped) . pure . pure

-- | Create a behavior from function of (absolute) time.
--   
--   You should pass a function defined for the whole range, including negative time.
behavior :: (Time -> a) -> Behavior a
behavior = (^. wrapped) . pure

-- | Create a behaviour from a function of (relative) duration focused on 'sunit'.
--   
--   You should pass a function defined for the whole range, including negative durations.
varying :: (Duration -> a) -> Behavior a
varying = varyingIn sunit

-- | Create a behaviour from a function of (relative) duration focused on the given span.
--   
--   You should pass a function defined for the whole range, including negative durations.
varyingIn :: Span -> (Duration -> a) -> Behavior a
varyingIn s f = behavior $ sapp (sinvert s) (lmap (.-. start) f)

-- | @b ?? t@ returns the value of the behavior at time @t@.
--  Semantic function.
behAt :: Behavior a -> Time -> a
b `behAt` t = ((^. unwrapped) b ? t) t

time :: Behavior Time
time = behavior id

trimBeforeB :: Monoid a => Time -> Behavior a -> Behavior a
trimBeforeB t = unwrapping Behavior %~ trimBefore t

switchB :: Time -> Behavior a -> Behavior a -> Behavior a
switchB t ((^. unwrapped) -> x) ((^. unwrapped) -> y) = (^. wrapped) $ switch t x y




