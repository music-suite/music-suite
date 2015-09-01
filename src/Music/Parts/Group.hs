
module Music.Parts.Group where

import           Control.Applicative
import           Control.Lens                    (toListOf, Lens, Lens', (^.))
import           Data.Aeson                      (ToJSON (..), FromJSON(..))
import qualified Data.Aeson
import           Data.Default
-- import           Data.Monoid
-- import           Control.Lens (set)
import           Data.Functor.Adjunction         (unzipR)
import qualified Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Option.Instances
import           Data.Traversable                (traverse)
import           Data.Typeable
import           Text.Numeral.Roman              (toRoman)

import           Music.Parts.Basic
import           Music.Parts.Division
import           Music.Parts.Solo
import           Music.Parts.Instrument
import           Music.Parts.Part
import           Music.Parts.Subpart

