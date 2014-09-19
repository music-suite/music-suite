
module Data.Semigroup.Option.Instances where

import           Control.Applicative
import           Control.Lens            (toListOf)
import           Data.Default
import           Data.Functor.Adjunction (unzipR)
import qualified Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Traversable        (traverse)
import           Data.Typeable
import           Text.Numeral.Roman      (toRoman)

-- TODO move

instance Num a => Num (Option a) where
  (+)       = liftA2 (+)
  (-)       = liftA2 (-)
  (*)       = liftA2 (*)
  abs       = fmap abs
  signum    = fmap signum
  fromInteger = pure . fromInteger
instance Integral a => Integral (Option a) where
  quotRem x y = unzipR $ liftA2 quotRem x y
  toInteger = toInteger . get where get = (head.toListOf traverse)
instance Real a => Real (Option a) where
  toRational = toRational . get where get = (head.toListOf traverse)
instance Enum a => Enum (Option a) where
  fromEnum = fromEnum . get where get = (head.toListOf traverse)
  toEnum = pure . toEnum

-- TODO move

instance Num a => Num (First a) where
  (+)       = liftA2 (+)
  (-)       = liftA2 (-)
  (*)       = liftA2 (*)
  abs       = fmap abs
  signum    = fmap signum
  fromInteger = pure . fromInteger
instance Integral a => Integral (First a) where
  quotRem x y = unzipR $ liftA2 quotRem x y
  toInteger = toInteger . get where get = (head.toListOf traverse)
instance Real a => Real (First a) where
  toRational = toRational . get where get = (head.toListOf traverse)
-- instance Enum a => Enum (First a) where
  -- toEnum = toEnum . get where get = (head.toListOf traverse)
  -- fromEnum = pure . fromEnum
