-- | Common intervals.
module Music.Pitch.Common.Interval
  ( -- * Intervals
    Interval,

    -- ** Creating intervals
    interval,
    interval',
    _number,
    _quality,
    _steps,
    _alteration,

    -- ** Synonyms
    perfect,
    major,
    minor,
    augmented,
    diminished,
    doublyAugmented,
    doublyDiminished,

    -- ** Inspecting intervals
    isNegative,
    isPositive,
    isNonNegative,
    isPerfectUnison,
    isStep,
    isLeap,

    -- ** Simple and compound intervals
    isSimple,
    isCompound,
    separate,
    simple,
    octaves,

    -- *** Inversion
    invert,

    -- * Basis values
    IntervalBasis (..),

    -- ** Converting basis
    convertBasis,
    convertBasisFloat,
    intervalDiv,
  )
where

import Control.Applicative
import Control.Lens hiding (simple)
import Control.Monad
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson
import Data.AffineSpace.Point (relative)
import Data.Basis
import Data.Either
import qualified Data.List as List
import Data.Maybe
import Data.Semigroup
import Data.Typeable
import Data.VectorSpace
import Music.Pitch.Absolute
import Music.Pitch.Augmentable
import Music.Pitch.Common.Number
import Music.Pitch.Common.Quality
import Music.Pitch.Common.Semitones
import Music.Pitch.Common.Types
import Music.Pitch.Common.Internal
import Music.Pitch.Literal
import Numeric.Positive

