
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Semigroup.Instances () where

import Data.Semigroup
import Data.VectorSpace hiding (Sum)
import Data.AffineSpace
import Control.Applicative
import Music.Pitch.Literal
import Music.Dynamics.Literal

-- TODO move these to semigroups and music-pitch-literal

deriving instance Num a => Num (Sum a)
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


deriving instance Num a => Num (Product a)
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
instance Applicative Product where
  pure = Product
  Product f <*> Product x = Product (f x)
deriving instance Applicative Sum where
  pure = Sum
  Sum f <*> Sum x = Sum (f x)


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
