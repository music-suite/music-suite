
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
import           Data.Bifunctor
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Ratio
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.VectorSpace
import           Data.String
import           Data.Functor.Couple

import           Control.Applicative
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
import           Music.Time.Reverse
import           Music.Time.Split


-- |
-- A 'Stretched' value has a known 'duration', but no 'position'.
--
-- Placing a value inside 'Stretched' makes it invariant under 'delay', however the inner
-- value can still be delayed using @'fmap' 'delay'@.
--
newtype Stretched a = Stretched { _stretchedValue :: Couple Duration a }
  deriving (Applicative, Monad, {-Comonad, -}
            Functor,  Foldable, Traversable)

-- $semantics Stretched
--
-- @
-- type Stretched = (Duration, a)
-- @
--

-- >>> stretch 2 $ (5,1)^.stretched
-- (10,1)^.stretched
--
-- >>> delay 2 $ (5,1)^.stretched
-- (5,1)^.stretched
--

deriving instance Eq  a => Eq  (Stretched a)
deriving instance Num a => Num (Stretched a)
deriving instance Fractional a => Fractional (Stretched a)
deriving instance Floating a => Floating (Stretched a)
deriving instance Ord a => Ord (Stretched a)
deriving instance Real a => Real (Stretched a)
deriving instance RealFrac a => RealFrac (Stretched a)

deriving instance Typeable1 Stretched

instance Wrapped (Stretched a) where
  type Unwrapped (Stretched a) = (Duration, a)
  _Wrapped' = iso (getCouple . _stretchedValue) (Stretched . Couple)

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

instance (Show a, Transformable a) => Show (Stretched a) where
  show x = show (x^.from stretched) ++ "^.stretched"

-- Lifted instances

instance IsString a => IsString (Stretched a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Stretched a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Stretched a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Stretched a) where
  fromDynamics = pure . fromDynamics

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
    _stretched f (Stretched (Couple (d, x))) = Stretched (Couple (d, f `whilst` stretching d $ x))
    runStretched = uncurry stretch . view _Wrapped
{-# INLINE stretchedValue #-}

