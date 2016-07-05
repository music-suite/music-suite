
-- | Provides basic voice types. Useful for vocal music but also for abstract
--   voice leading problems etc.
module Music.Parts.Voices (
  ) where

import           Control.Applicative
import           Control.Lens            (toListOf)
import           Data.Default
import qualified Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Traversable        (traverse)
import           Data.Typeable
import           Text.Numeral.Roman      (toRoman)
import           Data.Semigroup.Option.Instances

-- Related to "bounded" and "sized" integers (i.e. Pitch.Equal)
newtype SATB       = SATB Int
newtype SSAATTBB   = SSAATTBB Int
newtype SMezATBarB = SMezATBarB Int
