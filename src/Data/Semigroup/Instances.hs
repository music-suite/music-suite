
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Semigroup.Instances () where

import Data.Semigroup
import Data.VectorSpace hiding (Sum)
import Data.AffineSpace
import Music.Pitch.Literal
import Music.Dynamics.Literal
-- TODO move these to semigroups (PR!)

deriving instance Num a => Num (Sum a)
deriving instance Real a => Real (Sum a)
deriving instance AdditiveGroup a => AdditiveGroup (Sum a)

instance VectorSpace a => VectorSpace (Sum a) where
  type Scalar (Sum a) = Scalar a
  s *^ Sum v = Sum (s *^ v)

instance AffineSpace a => AffineSpace (Sum a) where
  type Diff (Sum a) = Sum (Diff a)
  Sum p .-. Sum q = Sum (p .-. q)
  Sum p .+^ Sum v = Sum (p .+^ v)


instance IsDynamics a => IsDynamics (Sum a) where
  fromDynamics = Sum . fromDynamics

instance IsPitch a => IsPitch (Sum a) where
  fromPitch = Sum . fromPitch

                                