
{-# LANGUAGE 
    ScopedTypeVariables, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds, 
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
        varying,
        time,
        switchB,
        sinceB,
  ) where

import Prelude hiding (until)

import Control.Newtype                
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Plus       
import Control.Monad.Compose
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Typeable
import Data.Semigroup
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Music.Time
import Music.Time.Reactive
import Music.Score.Note
import Music.Score.Track
import Music.Score.Pitch
import Music.Score.Util
import Music.Pitch.Literal
import Music.Dynamics.Literal   

-- Inner TFun is always defined on [0..1]
newtype Behavior a = Behavior { getBehavior :: Reactive (Time -> a) }
    deriving (Functor, Semigroup, Monoid)

instance Delayable (Behavior a) where
    delay n (Behavior x) = Behavior (delay n x)

instance Stretchable (Behavior a) where
    stretch n (Behavior x) = Behavior (stretch n x)

instance Newtype (Behavior a) (Reactive (Time -> a)) where
    pack = Behavior
    unpack = getBehavior

instance Applicative Behavior where
    pure    = pack . pure . pure
    (unpack -> f) <*> (unpack -> x) = pack $ liftA2 (<*>) f x

constant :: a -> Behavior a
constant = pack . pure . pure

varying :: (Time -> a) -> Behavior a
varying = pack . pure

time :: Behavior Time
time = varying id

sinceB :: Monoid a => Time -> Behavior a -> Behavior a
sinceB t = over Behavior (since t)

switchB :: Time -> Behavior a -> Behavior a -> Behavior a
switchB t (unpack -> x) (unpack -> y) = pack $ switch t x y




