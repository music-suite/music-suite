module Data.Semigroup.Applicative where

import Control.Applicative
import Data.Semigroup

-- TODO move to semigroups package
instance Functor First where
  fmap f (First x) = First (f x)

instance Applicative First where

  pure x = First x

  First f <*> First x = First (f x)

instance Functor Last where
  fmap f (Last x) = Last (f x)

instance Applicative Last where

  pure x = Last x

  Last f <*> Last x = Last (f x)
