
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2015
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Parts.Subpart (
          Subpart(..),
          containsSubpart,
  ) where

import           Control.Applicative
import           Control.Lens                    (toListOf)
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

instance Show Subpart where
    show (Subpart ps) = Data.List.intercalate "." $ mapFR showDivisionR showDivision $ ps
        where
            mapFR f g []     = []
            mapFR f g (x:xs) = f x : fmap g xs

containsSubpart :: Subpart -> Subpart -> Bool
Subpart x `containsSubpart` Subpart y = y `Data.List.isPrefixOf` x

