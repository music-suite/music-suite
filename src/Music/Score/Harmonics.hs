{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

-- | Provides a representation of harmonics.
module Music.Score.Harmonics
  ( -- * Harmonics
    HasHarmonic (..),
    Harmonics (..),
    HarmonicT (..),
    runHarmonicT,
    harmonic,
    artificial,
  )
where

import Control.Applicative
import Control.Comonad
import Control.Lens hiding (transform)
import Data.Foldable
import Data.Foldable
import Data.Functor.Couple
import Data.Ratio
import Data.Semigroup
import Data.Typeable
import Data.Word
import Music.Dynamics.Literal
import Music.Pitch.Alterable
import Music.Pitch.Augmentable
import Music.Pitch.Literal
import Music.Score.Part
import Music.Score.Phrases
import Music.Time.Score
import Numeric.Natural

class HasHarmonic a where

  setNatural :: Bool -> a -> a

  -- 0 for none, positive for natural, negative for artificial
  -- (isNatural, overtone series index where 0 is fundamental)
  setHarmonic :: Int -> a -> a

  default setNatural :: forall f b. (a ~ f b, Functor f, HasHarmonic b) => Bool -> a -> a
  setNatural s = fmap (setNatural s)

  default setHarmonic :: forall f b. (a ~ f b, Functor f, HasHarmonic b) => Int -> a -> a
  setHarmonic s = fmap (setHarmonic s)

newtype HarmonicT a = HarmonicT {getHarmonicT :: Couple (Any, Sum Int) a}
  deriving
    ( Eq,
      Show,
      Ord,
      Functor,
      Foldable,
      Traversable,
      Typeable,
      Applicative,
      Monad,
      Comonad
    )

data Harmonics
  = NaturalHarmonic Natural
  | ArtificialHarmonic Natural
  deriving (Eq, Ord, Show)

runHarmonicT :: HarmonicT a -> (Harmonics, a)
runHarmonicT (HarmonicT (Couple (hs, x))) = (,x) $ case hs of
  (Any False, Sum n) -> ArtificialHarmonic (fromIntegral n)
  (Any True, Sum n) -> NaturalHarmonic (fromIntegral n)

instance HasHarmonic a => HasHarmonic (b, a)

instance HasHarmonic a => HasHarmonic (Maybe a)

instance HasHarmonic a => HasHarmonic (Couple b a)

instance HasHarmonic a => HasHarmonic [a]

instance HasHarmonic a => HasHarmonic (Score a)

instance Wrapped (HarmonicT a) where

  type Unwrapped (HarmonicT a) = Couple (Any, Sum Int) a

  _Wrapped' = iso getHarmonicT HarmonicT

instance Rewrapped (HarmonicT a) (HarmonicT b)

instance HasHarmonic (HarmonicT a) where

  setNatural b = over (_Wrapped' . _Wrapped') $ \((_, n), x) -> ((Any b, n), x)

  setHarmonic n = over (_Wrapped' . _Wrapped') $ \((nat, _), x) -> ((nat, Sum n), x)

-- Lifted instances
deriving instance Num a => Num (HarmonicT a)

deriving instance Fractional a => Fractional (HarmonicT a)

deriving instance Floating a => Floating (HarmonicT a)

deriving instance Enum a => Enum (HarmonicT a)

deriving instance Bounded a => Bounded (HarmonicT a)

deriving instance (Num a, Ord a, Real a) => Real (HarmonicT a)

deriving instance (Real a, Enum a, Integral a) => Integral (HarmonicT a)

-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
harmonic :: HasHarmonic a => Int -> a -> a
harmonic n = setNatural True . setHarmonic n

-- TODO verify this can actually be played

-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
artificial :: HasHarmonic a => a -> a
artificial = setNatural False . setHarmonic 3
