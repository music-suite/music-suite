
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

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
        -- constant,
        -- behavior,
        -- varying,
        -- varyingIn,
        -- time,
        -- switchB,
        -- trimBeforeB,
  ) where

import           Prelude                hiding (trimAfter)

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Compose
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as F
import qualified Data.List              as List
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.NumInstances      ()
import           Data.Profunctor
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Traversable       (Traversable)
import qualified Data.Traversable       as T
import           Data.Typeable
import           Data.VectorSpace

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Time.Delayable
import           Music.Time.Reactive
import           Music.Time.Span
import           Music.Time.Stretchable
import           Music.Time.Time
import           Music.Time.Time

-- Inner TFun focuses on [0..1]
newtype Behavior a = Behavior { getBehavior :: Reactive (Time -> a) }
    deriving (Functor, Semigroup, Monoid)

instance Delayable (Behavior a) where
    delay n (Behavior x) = Behavior (fmap (delay n) $ delay n x)

instance Stretchable (Behavior a) where
    stretch n (Behavior x) = Behavior (fmap (stretch n) $ stretch n x)

instance Wrapped (Behavior a) where
    type Unwrapped (Behavior a) = (Reactive (Time -> a))
    _Wrapped' = iso getBehavior Behavior

instance Applicative Behavior where
    pure    = (^. _Unwrapped') . pure . pure
    ((^. _Wrapped') -> f) <*> ((^. _Wrapped') -> x) = (^. _Unwrapped') $ liftA2 (<*>) f x

instance HasBehavior Behavior where
    (?) = behAt

deriving instance Num a => Num (Behavior a)
deriving instance Fractional a => Fractional (Behavior a)
deriving instance Floating a => Floating (Behavior a)

instance IsPitch a => IsPitch (Behavior a) where
    fromPitch = pure . fromPitch
instance IsInterval a => IsInterval (Behavior a) where
    fromInterval = pure . fromInterval
instance IsDynamics a => IsDynamics (Behavior a) where
    fromDynamics = pure . fromDynamics
instance AdditiveGroup a => AdditiveGroup (Behavior a) where
    zeroV = pure zeroV
    negateV = fmap negateV
    (^+^) = liftA2 (^+^)
instance AffineSpace a => AffineSpace (Behavior a) where
    type Diff (Behavior a) = Behavior (Diff a)
    (.+^) = liftA2 (.+^)
    (.-.) = liftA2 (.-.)




-- | A constant (non-varying) behavior.
--
--   Identical to @behavior . const@ but creates more efficient behaviors.
constant :: a -> Behavior a
constant = (^. _Unwrapped') . pure . pure

-- | Create a behavior from function of (absolute) time.
--
--   You should pass a function defined for the whole range, including negative time.
behavior :: (Time -> a) -> Behavior a
behavior = (^. _Unwrapped') . pure

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
b `behAt` t = ((^. _Wrapped') b ? t) t

time :: Behavior Time
time = behavior id
-- 
-- trimBeforeB :: Monoid a => Time -> Behavior a -> Behavior a
-- trimBeforeB t = _Wrapping' Behavior %~ trimBefore t
-- 
-- switchB :: Time -> Behavior a -> Behavior a -> Behavior a
-- switchB t ((^. _Wrapped') -> x) ((^. _Wrapped') -> y) = (^. _Unwrapped') $ switch t x y
-- 
-- 
-- 
--                  
