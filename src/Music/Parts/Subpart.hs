{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

-- | A type to represent (recursive) subdivisions of a part.
module Music.Parts.Subpart
  ( Subpart (..),
    containsSubpart,
    properlyContainsSubpart,
    isSubpartOf,
    isProperSubpartOf,
    BoundIncr (..),
    HasSubpart (..),
  )
where

import Control.Applicative
import Control.Lens (Lens', Rewrapped, Wrapped (..), iso, toListOf)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson
import Data.Default
import qualified Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Semigroup
import Data.Semigroup.Option.Instances
import Data.Set (Set)
import Data.Traversable (traverse)
import Data.Typeable
import Music.Parts.Division
import Text.Numeral.Roman (toRoman)

{-
    - Note the meaning of maxSubpart for Common.Subpart (~ [Integer]):
        maxSubpart = pure . maximum . fmap head
    - In other words, the part component must allow the operations
        maxSubpart :: Set a -> Subpart a
        incrSubpart :: Subpart a -> Subpart a
        subpart :: Lens' a (Subpart a)
-}
-- TODO laws
-- TODO rename? (SubpartOf -> Subpart, Subpart -> DivisionList/Voice)
class BoundIncr (SubpartOf a) => HasSubpart a where

  type SubpartOf a

  subpart :: Lens' a (SubpartOf a)

-- TODO name
-- TODO laws?
class BoundIncr a where

  maximum' :: NonEmpty a -> a

  increment' :: a -> a

instance BoundIncr Integer where

  maximum' = maximum

  increment' = succ

instance BoundIncr Subpart where

  maximum' = maximum . fmap firstComp

  increment' (Subpart xs) = Subpart (fmap succ xs)

firstComp :: Subpart -> Subpart
firstComp (Subpart xs) = Subpart (pure $ NonEmpty.head xs)

-- |
-- A subpart is a potentially infinite sequence of divisions, each typically
-- designated using a new index type, i.e. @I.1.2@.
--
-- The empty subpart (also known as 'mempty') represents all the players of the group,
-- or in the context of 'Part', all players of the given instrument.
newtype Subpart = Subpart (NonEmpty Division)
  deriving (Eq, Ord, Semigroup)

instance Default Subpart where
  def = Subpart (pure 1)

{-
instance Wrapped Subpart where

  type Unwrapped Subpart = [Division]

  _Wrapped' = iso getSubpart Subpart
    where
      getSubpart (Subpart x) = x

instance Rewrapped Subpart Subpart
-}

instance Num Subpart where

  fromInteger n = Subpart (pure $ fromInteger n)

  (+) = error "Num Subpart: Not implemented"

  (*) = error "Num Subpart: Not implemented"

  (-) = error "Num Subpart: Not implemented"

  abs = error "Num Subpart: Not implemented"

  signum = error "Num Subpart: Not implemented"

instance ToJSON Subpart where
  toJSON (Subpart xs) = toJSON xs

instance FromJSON Subpart where
  parseJSON = (fmap . fmap) Subpart $ parseJSON

{-
TODO hide internals

Expose

divisions :: Traversal' Subpart Division

or similar
-}

instance Show Subpart where
  show (Subpart ps) = Data.List.intercalate "." $ mapFR showDivisionR showDivision $ ps
    where
      mapFR f g (x NonEmpty.:| xs) = f x : fmap g xs

containsSubpart :: Subpart -> Subpart -> Bool
containsSubpart = flip isSubpartOf

properlyContainsSubpart :: Subpart -> Subpart -> Bool
properlyContainsSubpart = flip isProperSubpartOf

isSubpartOf :: Subpart -> Subpart -> Bool
Subpart x `isSubpartOf` Subpart y = NonEmpty.toList y `NonEmpty.isPrefixOf` x

isProperSubpartOf :: Subpart -> Subpart -> Bool
x `isProperSubpartOf` y = x `isSubpartOf` y && x /= y
