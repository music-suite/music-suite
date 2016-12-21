
module Music.Time.Split (

        module Music.Time.Position,

        -- * The Splittable class
        Splittable(..),

        -- * Miscellaneous
        chunks,

  ) where

import           Control.Lens             hiding (Indexable, Level, above,
                                           below, index, inside, parts,
                                           reversed, transform, (<|), (|>))
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Semigroup           hiding ()
import           Data.Sequence            (Seq)
import qualified Data.Sequence            as Seq
import           Data.VectorSpace         hiding (Sum (..))

import           Music.Time.Internal.Util
import           Music.Time.Position

-- |
-- Class of values that can be split.
--
-- Instances should satisfy:
--
-- @
-- ('beginning' t x)^.'duration' + ('ending' t x)^.'duration' = x^.'duration'
-- ('beginning' t x)^.'duration' = t `min` x^.'duration'                    iff t >= 0
-- ('ending' t x)^.'duration'    = x^.'duration' - (t `min` x^.'duration')    iff t >= 0
-- @
--
-- (Note that any of these three laws can be derived from the other two, so it is
-- sufficient to prove two!).
--
class HasDuration a => Splittable a where

  -- | Split a value at the given duration and return both parts.
  split      :: Duration -> a -> (a, a)
  split   d x = (beginning d x, ending d x)

  -- | Split a value at the given duration and return only the first part.
  beginning  :: Duration -> a -> a

  -- | Split a value at the given duration and return only the second part.
  ending     :: Duration -> a -> a
  beginning d = fst . split d
  ending    d = snd . split d

  {-# MINIMAL (split) | (beginning, ending) #-}

instance Splittable () where
  split _ x = (x, x)
instance Splittable Char where
  split _ x = (x,x)
instance Splittable Int where
  split _ x = (x,x)
instance Splittable Double where
  split _ x = (x,x)

instance Splittable Duration where
  -- Directly from the laws
  -- Guard against t < 0
  split t x = (t' `min` x, x ^-^ (t' `min` x))
    where t' = t `max` 0

chunks :: (Transformable a, Splittable a) => Duration -> a -> [a]
chunks d xs = chunks' d xs
  where
    chunks' d (split d -> (x, xs)) = [x] ++ chunks d xs
