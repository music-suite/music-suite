
-- | Representation of solo vs. tutti.
module Music.Parts.Solo (
          Solo(..),
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

data Solo
    = Solo
    | Tutti
    deriving (Eq, Show, Ord, Enum)

instance Default Solo where
    def = Tutti

