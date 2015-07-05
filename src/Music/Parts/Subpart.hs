
-- | A type to represent (recursive) subdivisions of a part.
module Music.Parts.Subpart (
          Subpart(..),
          containsSubpart,
          properlyContainsSubpart,
          isSubpartOf,
          isProperSubpartOf,
  ) where

import           Control.Applicative
import           Control.Lens                    (toListOf, Wrapped(..), Rewrapped(..))
import           Data.Default
import           Data.Functor.Adjunction         (unzipR)
import qualified Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Option.Instances
import           Data.Traversable                (traverse)
import           Data.Typeable
import           Text.Numeral.Roman              (toRoman)

import Music.Parts.Division

-- |
-- A subpart is a potentially infinite sequence of divisions, each typically
-- designated using a new index type, i.e. @I.1.2@.
--
-- The empty subpart (also known as 'mempty') represents all the players of the group,
-- or in the context of 'Part', all players of the given instrument.
--
newtype Subpart = Subpart [Division]
    deriving (Eq, Ord, Default, Semigroup, Monoid)

instance Wrapped Subpart where
  type Unwrapped Subpart = [Division]
  _Wrapped' = iso getSubpart Subpart
    where
      getSubpart (Subpart x) = x

instance Rewrapped Subpart Subpart   

{-
TODO hide internals

Expose

_divisions :: Traversal' Subpart Division

or similar 
-}

instance Show Subpart where
  show (Subpart ps) = Data.List.intercalate "." $ mapFR showDivisionR showDivision $ ps
    where
      mapFR f g []     = []
      mapFR f g (x:xs) = f x : fmap g xs

containsSubpart :: Subpart -> Subpart -> Bool
containsSubpart = flip isSubpartOf

properlyContainsSubpart :: Subpart -> Subpart -> Bool
properlyContainsSubpart = flip isProperSubpartOf

isSubpartOf :: Subpart -> Subpart -> Bool
Subpart x `isSubpartOf` Subpart y = y `Data.List.isPrefixOf` x

isProperSubpartOf :: Subpart -> Subpart -> Bool
x `isProperSubpartOf` y = x `isSubpartOf` y && x /= y





