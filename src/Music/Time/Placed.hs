module Music.Time.Placed
  ( -- * Placed type
    Placed,

    -- * Construction
    placed,
    placee,
  )
where

import Control.Applicative
import Control.Comonad
import Control.Lens hiding
  ( (<|),
    Indexable,
    Level,
    above,
    below,
    index,
    inside,
    parts,
    reversed,
    transform,
    (|>),
  )
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Bifunctor
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Functor.Couple
import Data.String
import Data.Typeable
import Data.VectorSpace
import Music.Dynamics.Literal
import Music.Pitch.Literal
import Music.Time.Internal.Util (dependingOn)
import Music.Time.Juxtapose

-- |
-- 'Placed' represents a value with an offset in time.
newtype Placed a = Placed {getPlaced :: Time `Couple` a}
  deriving
    ( Eq,
      Ord,
      Typeable,
      Foldable,
      Traversable,
      Functor,
      Applicative,
      Monad,
      Comonad
    )

instance (Show a, Transformable a) => Show (Placed a) where
  show x = show (x ^. from placed) ++ "^.placed"

instance Wrapped (Placed a) where

  type Unwrapped (Placed a) = (Time, a)

  _Wrapped' = iso (getCouple . getPlaced) (Placed . Couple)

instance Rewrapped (Placed a) (Placed b)

instance Transformable a => Transformable (Placed a) where
  transform t =
    over (from placed . _1) (transform t)
      . over (from placed . _2) (stretch $ stretchComponent t)

instance IsString a => IsString (Placed a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Placed a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Placed a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Placed a) where
  fromDynamics = pure . fromDynamics

placed :: Iso (Time, a) (Time, b) (Placed a) (Placed b)
placed = _Unwrapped

placee :: (Transformable a, Transformable b) => Lens (Placed a) (Placed b) a b
placee = from placed `dependingOn` (transformed . delayingTime)
  where
    delayingTime = (>-> 1)
