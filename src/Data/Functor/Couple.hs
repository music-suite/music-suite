

{-# LANGUAGE CPP,
             GeneralizedNewtypeDeriving,
             DeriveDataTypeable,
             DeriveFunctor,
             DeriveTraversable,
             DeriveFoldable,
             MultiParamTypeClasses,
             TypeFamilies
             #-}
-- |
-- Defines two variants of @(,)@ with lifted instances for the standard type classes.
--
-- The 'Functor', 'Applicative' and 'Comonad' instances are the standard instances. The
-- 'Monad' instances are not in base (but should argubly be there). All of these instances
-- are equivalent to 'Writer' in transformers.
--
-- 'Applicative' is used to lift 'Monoid' and the standard numeric classes.
--
-- The only difference between 'Twain' and 'Couple' is the handling of 'Eq' and 'Ord':
-- 'Twain' compares only the second value, while 'Couple' compares both. Thus 'Couple' needs
-- an extra @Ord b@ constraint for all classes.
--
module Data.Functor.Couple (Twain(..), Couple(..)) where

import Data.Bifunctor
import Data.Functor.Product
import Data.Functor.Identity
import Data.Foldable
import Data.Traversable
import Data.Functor.Adjunction (unzipR)
import Data.Semigroup
import Data.Typeable
import Control.Applicative
import Control.Comonad
import Data.PairMonad ()
import Control.Lens (Wrapped(..), Rewrapped(..), iso)

-- |
-- A variant of pair/writer with lifted instances for the numeric classes, using 'Applicative'.
--
newtype Twain b a = Twain { getTwain :: (b, a) }
  deriving (Show, Functor, Foldable, Typeable, Applicative, Monad, Comonad, Semigroup, Monoid)

instance Wrapped (Twain b a) where
  type Unwrapped (Twain b a) = (b, a)
  _Wrapped' = iso getTwain Twain

instance Rewrapped (Twain c a) (Twain c b)

instance (Monoid b, Num a) => Num (Twain b a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Monoid b, Fractional a) => Fractional (Twain b a) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance (Monoid b, Floating a) => Floating (Twain b a) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acos

instance (Monoid b, Enum a) => Enum (Twain b a) where
  toEnum = pure . toEnum
  fromEnum = fromEnum . extract

instance (Monoid b, Bounded a) => Bounded (Twain b a) where
  minBound = pure minBound
  maxBound = pure maxBound

-- 
-- Eq, Ord and their subclasses
-- 
-- If comparison takes both values into account, we must add and (Ord b)
-- constraint to all of the following instances. Instead, follow the
-- spirit of the Num et al instances to compare just the second argument.
--

instance Eq a => Eq (Twain b a) where
  Twain (b,a) == Twain (b',a')  =  a == a'

instance Ord a => Ord (Twain b a) where
  Twain (b,a) < Twain (b',a') = a < a'

instance (Monoid b, Real a, Enum a, Integral a) => Integral (Twain b a) where
  quot = liftA2 quot
  rem  = liftA2 rem
  quotRem = fmap (fmap unzipR) (liftA2 quotRem)
  toInteger = toInteger . extract  

instance (Monoid b, Real a) => Real (Twain b a) where
  toRational = toRational . extract

instance (Monoid b, RealFrac a) => RealFrac (Twain b a) where
  properFraction = first extract . unzipR . fmap properFraction


-- |
-- A variant of pair/writer with lifted instances for the numeric classes, using 'Applicative'.
--
newtype Couple b a = Couple { getCouple :: (b, a) }
  deriving (Show, Functor, Foldable, Typeable, Applicative, Monad, Comonad, Semigroup, Monoid)

instance Wrapped (Couple b a) where
  type Unwrapped (Couple b a) = (b, a)
  _Wrapped' = iso getCouple Couple

instance Rewrapped (Couple c a) (Couple c b)

instance (Monoid b, Num a) => Num (Couple b a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Monoid b, Fractional a) => Fractional (Couple b a) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance (Monoid b, Floating a) => Floating (Couple b a) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acos

instance (Monoid b, Enum a) => Enum (Couple b a) where
  toEnum = pure . toEnum
  fromEnum = fromEnum . extract

instance (Monoid b, Bounded a) => Bounded (Couple b a) where
  minBound = pure minBound
  maxBound = pure maxBound

instance (Eq b, Eq a) => Eq (Couple b a) where
  Couple ((b,a)) == Couple (b',a')  =  (b,a) == (b',a')

instance (Ord b, Ord a) => Ord (Couple b a) where
  Couple (b,a) < Couple (b',a') = (b,a) < (b',a')

instance (Monoid b, Ord b, Real a, Enum a, Integral a) => Integral (Couple b a) where
  quot = liftA2 quot
  rem  = liftA2 rem
  quotRem = fmap (fmap unzipR) (liftA2 quotRem)
  toInteger = toInteger . extract  

instance (Monoid b, Ord b, Real a) => Real (Couple b a) where
  toRational = toRational . extract

instance (Monoid b, Ord b, RealFrac a) => RealFrac (Couple b a) where
  properFraction = first extract . unzipR . fmap properFraction

