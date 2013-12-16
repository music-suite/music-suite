
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Substitute where

import Data.Set(Set)
import Data.Map(Map)
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Data.Ratio (Ratio)
import Control.Monad (join)

type family a /~ b

type instance () /~ g           = ()
type instance Double /~ g       = ()
type instance Float /~ g        = ()
type instance Int /~ g          = ()
type instance Integer /~ g      = ()
type instance Ratio a /~ g      = Ratio (a /~ g)

type instance [a] /~ g          = [a /~ g]
type instance (Set a) /~ g      = Set (a /~ g)
type instance (Map k a) /~ g    = Map k (a /~ g)
type instance (b -> a) /~ g     = b -> (a /~ g)
type instance (b, a) /~ g       = (b, a /~ g)
type instance (Maybe a) /~ g    = Maybe (a /~ g)
type instance (Either e a) /~ g = Either e (a /~ g)
