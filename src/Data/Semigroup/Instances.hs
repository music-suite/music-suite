{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semigroup.Instances
  (
  )
where

import Control.Applicative
import Data.AffineSpace
import Data.Orphans ()
import Data.Semigroup
import Data.VectorSpace hiding (Sum)
import Music.Dynamics.Literal
import Music.Pitch.Literal

-- TODO move these to semigroups and music-pitch-literal

deriving instance Real a => Real (Sum a)

deriving instance Fractional a => Fractional (Sum a)

deriving instance AdditiveGroup a => AdditiveGroup (Sum a)

instance VectorSpace a => VectorSpace (Sum a) where

  type Scalar (Sum a) = Scalar a

  s *^ Sum v = Sum (s *^ v)

instance AffineSpace a => AffineSpace (Sum a) where

  type Diff (Sum a) = Sum (Diff a)

  Sum p .-. Sum q = Sum (p .-. q)

  Sum p .+^ Sum v = Sum (p .+^ v)

deriving instance Real a => Real (Product a)

deriving instance Fractional a => Fractional (Product a)

deriving instance AdditiveGroup a => AdditiveGroup (Product a)

instance VectorSpace a => VectorSpace (Product a) where

  type Scalar (Product a) = Scalar a

  x *^ Product y = Product (x *^ y)

instance AffineSpace a => AffineSpace (Product a) where

  type Diff (Product a) = Product (Diff a)

  Product p .-. Product q = Product (p .-. q)

  Product p .+^ Product v = Product (p .+^ v)

deriving instance IsDynamics a => IsDynamics (Sum a)

deriving instance IsDynamics a => IsDynamics (Product a)

deriving instance IsPitch a => IsPitch (Sum a)

deriving instance IsPitch a => IsPitch (Product a)
{-
deriving instance Floating a => Floating (Product a)
instance Num a => Num (Product a) where
  fromInteger = pure . fromInteger
  abs    = fmap abs
  signum = fmap signum
  (+)    = liftA2 (+)
  (-)    = liftA2 (-)
  (*)    = liftA2 (*)
instance Fractional a => Fractional (Product a) where
  fromRational = pure . fromRational
  (/) = liftA2 (/)
instance Real a => Real (Product a) where
  toRational (Product x) = toRational x
-}
