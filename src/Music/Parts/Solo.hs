
-- | Representation of solo vs. tutti.
module Music.Parts.Solo (
          Solo(..),
  ) where

import           Control.Applicative
import           Data.Aeson                      (ToJSON (..), FromJSON(..))
import qualified Data.Aeson
import           Control.Lens                    (toListOf)
import           Data.Default
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

instance ToJSON Solo where
  toJSON Solo  = toJSON ("solo"::String)
  toJSON Tutti = toJSON ("tutti"::String)
instance FromJSON Solo where
  parseJSON (Data.Aeson.String "solo") = return Solo
  parseJSON (Data.Aeson.String "tutti") = return Tutti
  parseJSON _ = empty
