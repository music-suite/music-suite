
-- | Provides basic voice types. Useful for vocal music but also for abstract
--   voice leading problems etc.
module Music.Parts.Voices (
  ) where

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
import           Data.Semigroup.Option.Instances

-- TODO newtype...
-- Related to "bounded" and "sized" integers (i.e. Pitch.Equal)
type SATB       = Int
type SSAATTBB   = Int
type SMezATBarB = Int

-- newtype BasicPart = BasicPart { getBasicPart :: Option (First Integer) }
--     deriving (Eq, Ord, Num, Integral, Real, Enum, Typeable, Semigroup, Monoid)
-- 
-- instance Default BasicPart where
--   def = mempty
-- 
-- instance Show BasicPart where
--     show _ = ""
--                       
