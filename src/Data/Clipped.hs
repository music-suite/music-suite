
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

module Data.Clipped (
    -- * Data.Clipped
    Clipped,
    unsafeToClipped,
    fromClipped,
    clipped,
    unclipped,    
  ) where

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

-- | A value in the unit interval /(0,1)/.
newtype Clipped a = UnsafeClip { unsafeGetClipped :: a }
  deriving (Eq, Ord, Show)

instance Num a => Bounded (Clipped a) where
  minBound = UnsafeClip 0
  maxBound = UnsafeClip 1

instance (Num a, Ord a) => Num (Clipped a) where
  a + b = unsafeToClipped (fromClipped a + fromClipped b)
  a - b = unsafeToClipped (fromClipped a - fromClipped b)
  a * b = unsafeToClipped (fromClipped a * fromClipped b)
  abs   = id
  signum 0 = 0
  signum _ = 1
  negate = error "negate: No instance for Clipped"
  fromInteger = unsafeToClipped . fromInteger

instance (Num a, Ord a, Fractional a) => Fractional (Clipped a) where
  a / b = unsafeToClipped (fromClipped a / fromClipped b)
  recip 1 = 1
  recip _ = error "Can not take reciprocal of a clipped value other than 1"
  fromRational = unsafeToClipped . fromRational

unsafeToClipped   = fromMaybe (error "Outside 0-1") . (^? clipped)
fromClipped = (^. unclipped)

clipped :: (Num a, Ord a) => Prism' a (Clipped a)
clipped = prism unsafeGetClipped $
  \x -> if 0 <= x && x <= 1
      then Right (UnsafeClip x)
      else Left x

unclipped :: (Num a, Ord a) => Getter (Clipped a) a
unclipped = re clipped

zipClippedWith
  :: (Num a, Ord a,
      Num b, Ord b,
      Num c, Ord c)
  => (a -> b -> c)
  -> Clipped a -> Clipped b -> Maybe (Clipped c)
zipClippedWith f a b = ((a^.unclipped) `f` (b^.unclipped))^? clipped

addLim = zipClippedWith (+)
