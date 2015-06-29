
-- | Basic part representation.
module Music.Parts.Basic (
        BasicPart
  ) where

import           Control.Applicative
import           Control.Lens            (toListOf)
import           Data.Default
import           Data.Functor.Adjunction (unzipR)
import           Data.Semigroup
import           Data.Semigroup.Option.Instances
import           Data.Traversable        (traverse)
import           Data.Typeable

newtype BasicPart = BasicPart { getBasicPart :: Option (First Integer) }
    deriving (Eq, Ord, Num, Integral, Real, Enum, Typeable, Semigroup, Monoid)

instance Default BasicPart where
  def = mempty

instance Show BasicPart where
    show _ = ""

