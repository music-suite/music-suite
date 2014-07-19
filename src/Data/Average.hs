
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Average (
    Average(..),
    average,
    maybeAverage
  ) where

import Prelude hiding ((**))

-- import Test.QuickCheck(Arbitrary(..))

import Data.Typeable
import Data.Maybe
import Data.Semigroup
import Data.AdditiveGroup
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad
import Control.Applicative

-- |
-- A monoid for 'Average' values.
--
-- This is actually just the free monoid with an extra function 'average' for
-- extracing the (arithmetic) mean. This function is used to implement 'Real',
-- so you can use 'Average' whenever a ('Monoid', 'Real') is required.
--
-- >>> toRational $ mconcat [1,2::Average Rational]
-- 3 % 2
-- >>> toRational $ mconcat [1,2::Sum Rational]
-- 3 % 1
-- >>> toRational $ mconcat [1,2::Product Rational]
-- 2 % 1
--
newtype Average a = Average { getAverage :: [a] }
  deriving (Show, {-Enum, Bounded,-} Semigroup, Monoid, 
    Typeable, Functor, Applicative)

instance (Fractional a, Eq a) => Eq (Average a) where
  a == b = average a == average b

instance (Fractional a, Ord a) => Ord (Average a) where
  a `compare` b = average a `compare` average b
  
-- What should (+) and (*) do for Average values?
-- 
-- The important thing is to preserve scalar addition and multiplication (for example
-- scaling all components of) an average value by some constant factor, so we can just as
-- well use the standard list instance. What about averages with more components? I *think*
-- 'average' is a linear map, so they would work as expected:
-- 
-- >>> average (2<>2<>3)+average (3<>3)
-- 16 % 3
-- >>> average $ (2<>2<>3)+(3<>3)
-- 16 % 3
-- >>> average (mconcat [5,6,9])*average (mconcat[-1,0])
-- (-10) % 3
-- >>> average $ (mconcat [5,6,9])*(mconcat[-1,0])
-- (-10) % 3
-- 

instance Num a => Num (Average a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs    = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  
instance (Fractional a, Num a) => Fractional (Average a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance (Real a, Fractional a) => Real (Average a) where
  toRational = toRational . average

instance Floating a => Floating (Average a) where
  pi = pure pi
  exp = fmap exp
  sqrt = fmap sqrt
  log = fmap log
  sin = fmap sin
  tan = fmap tan
  cos = fmap cos
  asin = fmap asin
  atan = fmap atan
  acos = fmap acos
  sinh = fmap sinh
  tanh = fmap tanh
  cosh = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh
  
instance AdditiveGroup a => AdditiveGroup (Average a) where
  zeroV = pure zeroV
  (^+^) = liftA2 (^+^)
  negateV = fmap negateV

instance VectorSpace a => VectorSpace (Average a) where
  type Scalar (Average a) = Scalar a
  s *^ v = liftA2 (*^) (pure s) v

instance AffineSpace a => AffineSpace (Average a) where
  type Diff (Average a) = Average (Diff a)
  p1 .-. p2 = liftA2 (.-.) p1 p2
  p .+^ v   = liftA2 (.+^) p v

{-
instance Arbitrary a => Arbitrary (Average a) where
  arbitrary = fmap Average arbitrary
-}

-- | Return the average of all monoidal components. If given 'mempty', return zero.
average :: Fractional a => Average a -> a
average = fromMaybe 0 . maybeAverage

-- | Return the average of all monoidal components. If given 'mempty', return 'Nothing'.
maybeAverage :: Fractional a => Average a -> Maybe a
maybeAverage (Average []) = Nothing
maybeAverage (Average xs) = Just $ sum xs / fromIntegral (length xs)


