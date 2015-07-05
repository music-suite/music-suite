
-- | A type to represent (flat) subdivisions of a part.
module Music.Parts.Division (
        Division,
        division,
        getDivision,
        divisions,
        showDivision,
        showDivisionR,
  ) where

import           Control.Applicative
import           Control.Lens            (toListOf)
import           Data.Aeson                      (ToJSON (..), FromJSON(..))
import qualified Data.Aeson
import           Data.Default
import           Data.Functor.Adjunction (unzipR)
import           Data.Semigroup
import           Data.Semigroup.Option.Instances
import           Data.Traversable                (traverse)
import           Data.Typeable
import           Text.Numeral.Roman              (toRoman)



{-
    For each part we want to know:
        - Classification:
            - Type: (i.e. woodwind)
            - Family: (i.e. saxophone)
            - Range: (i.e. tenor)
        - Range (i.e. [c_:e'])
        - Transposition:
            sounding = written .+^ transp
        - Suggested clefs
-}

-- |
-- A division represents a subset of a finite group of performers.
--
-- For example a group may be divided into three equal divisions,
-- designated @(0, 3)@, @(1, 3)@ and @(2, 3)@ respectively.
--
newtype Division = Division { getDivision :: (Int, Int) }
    deriving (Eq, Ord)

instance Show Division where
  show (Division (n,m)) = "division " ++ show n ++ show m

instance Default Division where
    def = Division (0,1)

instance ToJSON Division where
  toJSON (getDivision -> (x,y)) = Data.Aeson.object [("num-parts",toJSON y),("part-div",toJSON (x + 1))]
instance FromJSON Division where
  parseJSON (Data.Aeson.Object v) = do
    x <- v Data.Aeson..: "part-div"
    y <- v Data.Aeson..: "num-parts"
    return $ division (x - 1) y
  parseJSON _ = empty

-- | Show division in roman numerals.
showDivisionR :: Division -> String
showDivisionR = toRoman . succ . fst . getDivision

-- | Show division in ordinary numerals.
showDivision :: Division -> String
showDivision  = show . succ . fst . getDivision

-- | Create a division out of a ratio. Dual of getDivision.
division :: Int -> Int -> Division
division n m
  | n < 0 || m <= 0  = error "Invalid division"
  | n <  m           = Division (n, m)
  | otherwise        = error "Invalid division"

-- | Get all possible divisions for a given divisor in ascending order.
divisions :: Int -> [Division]
divisions n = [Division (x,n) | x <- [0..n-1]]


