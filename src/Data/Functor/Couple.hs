

{-# LANGUAGE CPP,
             GeneralizedNewtypeDeriving,
             DeriveDataTypeable,
             DeriveFunctor,
             DeriveTraversable,
             DeriveFoldable,
             MultiParamTypeClasses,
             TypeFamilies
             #-}
module Data.Functor.Couple where

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
import Control.Arrow (first)
import Control.Lens (Wrapped(..), Rewrapped(..), iso)

-- |
-- A variant of pair/writer with lifted instances for the numeric classes, using 'Applicative'.
--
newtype Couple b a = Couple { getCouple :: (b, a) }
  deriving (Eq, Show, Functor, Foldable, Typeable, Applicative, Monad, Comonad)

-- | Unsafe: Do not use 'Wrapped' instances
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

-- The following are more suspect, as they require Ord

instance (Monoid b, Ord b, Ord a) => Ord (Couple b a) where
  Couple (a,b) < Couple (a',b') = (b,a) < (b',a')

instance (Monoid b, Ord b, Real a, Enum a, Integral a) => Integral (Couple b a) where
  quot = liftA2 quot
  rem  = liftA2 rem
  quotRem = fmap (fmap unzipR) (liftA2 quotRem)
  toInteger = toInteger . extract  

instance (Monoid b, Ord b, Real a) => Real (Couple b a) where
  toRational = toRational . extract

instance (Monoid b, Ord b, RealFrac a) => RealFrac (Couple b a) where
  properFraction = first extract . unzipR . fmap properFraction

