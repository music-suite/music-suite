{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports #-}

module Music.Score.Tremolo
  ( -- * Tremolo
    HasTremolo (..),
    tremolo,

    -- ** Tremolo note transformer
    TremoloT (..),
    runTremoloT,
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
import Music.Dynamics.Literal
import Music.Pitch.Alterable
import Music.Pitch.Alterable
import Music.Pitch.Augmentable
import Music.Pitch.Augmentable
import Music.Pitch.Literal
import Music.Pitch.Literal
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Harmonics
import Music.Score.Meta
import Music.Score.Part
import Music.Score.Part
import Music.Score.Phrases
import Music.Score.Pitch
import Music.Score.Slide
import Music.Score.Text
import Music.Score.Ties
import Music.Time

class HasTremolo a where

  setTrem :: Int -> a -> a

  default setTrem :: forall f b. (a ~ f b, Functor f, HasTremolo b) => Int -> a -> a
  setTrem s = fmap (setTrem s)

instance HasTremolo a => HasTremolo (Maybe a)

instance HasTremolo a => HasTremolo (b, a)

instance HasTremolo a => HasTremolo (Couple b a)

instance HasTremolo a => HasTremolo [a]

instance HasTremolo a => HasTremolo (Score a)

-- |
-- Set the number of tremolo divisions for all notes in the score.
tremolo :: HasTremolo a => Int -> a -> a
tremolo = setTrem

newtype TremoloT a = TremoloT {getTremoloT :: Couple (Max Word) a}
  deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad, Comonad)

--
-- We use Word instead of Int to get (mempty = Max 0), as (Max.mempty = Max minBound)
-- Preferably we would use Natural but unfortunately this is not an instance of Bounded
--

instance Wrapped (TremoloT a) where

  type Unwrapped (TremoloT a) = Couple (Max Word) a

  _Wrapped' = iso getTremoloT TremoloT

instance Rewrapped (TremoloT a) (TremoloT b)

instance HasTremolo (TremoloT a) where
  setTrem n = set (_Wrapped . _Wrapped . _1) (Max $ fromIntegral n)

deriving instance Num a => Num (TremoloT a)

deriving instance Fractional a => Fractional (TremoloT a)

deriving instance Floating a => Floating (TremoloT a)

deriving instance Enum a => Enum (TremoloT a)

deriving instance Bounded a => Bounded (TremoloT a)

deriving instance (Num a, Ord a, Real a) => Real (TremoloT a)

deriving instance (Real a, Enum a, Integral a) => Integral (TremoloT a)

deriving instance IsPitch a => IsPitch (TremoloT a)

deriving instance IsDynamics a => IsDynamics (TremoloT a)

deriving instance Semigroup a => Semigroup (TremoloT a)

deriving instance Tiable a => Tiable (TremoloT a)

deriving instance HasHarmonic a => HasHarmonic (TremoloT a)

deriving instance HasSlide a => HasSlide (TremoloT a)

deriving instance HasText a => HasText (TremoloT a)

deriving instance Transformable a => Transformable (TremoloT a)

deriving instance Alterable a => Alterable (TremoloT a)

deriving instance Augmentable a => Augmentable (TremoloT a)

type instance Pitch (TremoloT a) = Pitch a

type instance SetPitch g (TremoloT a) = TremoloT (SetPitch g a)

type instance Dynamic (TremoloT a) = Dynamic a

type instance SetDynamic g (TremoloT a) = TremoloT (SetDynamic g a)

type instance Articulation (TremoloT a) = Articulation a

type instance SetArticulation g (TremoloT a) = TremoloT (SetArticulation g a)

instance (HasPitches a b) => HasPitches (TremoloT a) (TremoloT b) where
  pitches = _Wrapped . pitches

instance (HasPitch a b) => HasPitch (TremoloT a) (TremoloT b) where
  pitch = _Wrapped . pitch

instance (HasDynamics a b) => HasDynamics (TremoloT a) (TremoloT b) where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b) => HasDynamic (TremoloT a) (TremoloT b) where
  dynamic = _Wrapped . dynamic

instance (HasArticulations a b) => HasArticulations (TremoloT a) (TremoloT b) where
  articulations = _Wrapped . articulations

instance (HasArticulation a b) => HasArticulation (TremoloT a) (TremoloT b) where
  articulation = _Wrapped . articulation

-- |
-- Get the number of tremolo divisions.
runTremoloT :: TremoloT a -> (Int, a)
runTremoloT (TremoloT (Couple (Max n, a))) = (fromIntegral n, a)
