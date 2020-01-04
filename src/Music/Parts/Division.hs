-- | A type to represent (flat) subdivisions of a part.
module Music.Parts.Division
  ( Division (..),
    getDivision,
    divisions,
    showDivision,
    showDivisionR,
  )
where

import Control.Applicative
import Control.Lens (toListOf)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson
import Data.Default
import Data.Semigroup
import Data.Semigroup.Option.Instances
import Data.Traversable (traverse)
import Data.Typeable
import Text.Numeral.Roman (toRoman)

-- |
-- A subdivision of a part, e.g. in "Violin II", the division is "II".
--
-- See also 'SubPart'.
newtype Division = Division {getDivision :: Integer}
  deriving (Eq, Ord, Enum, Num)

instance Show Division where
  show (Division m) = "division " ++ show m

instance Default Division where
  def = Division 1

instance ToJSON Division where
  toJSON (getDivision -> x) = Data.Aeson.object [("part-div", toJSON x)]

instance FromJSON Division where
  parseJSON (Data.Aeson.Object v) = do
    x <- v Data.Aeson..: "part-div"
    return $ Division x
  parseJSON _ = empty

-- | Show division in roman numerals.
showDivisionR :: Division -> String
showDivisionR = toRoman . getDivision

-- | Show division in ordinary numerals.
showDivision :: Division -> String
showDivision = show . getDivision

-- | Get all possible divisions for a given divisor in ascending order.
divisions :: Int -> [Division]
divisions n = take n $ [Division x | x <- [0 ..]]
