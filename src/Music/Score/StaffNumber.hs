{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports #-}

module Music.Score.StaffNumber
  ( -- * StaffNumber
    HasStaffNumber (..),
    staffNumber,

    -- ** StaffNumber note transformer
    StaffNumberT (..),
    runStaffNumberT,
  )
where

import Control.Applicative
import Control.Comonad
import Control.Lens hiding (transform)
import Data.Foldable
import Data.Foldable
import Data.Functor.Couple
import Data.Monoid
import Data.Ratio
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
import Numeric.Natural

class HasStaffNumber a where

  setStaffNumber :: Natural -> a -> a

  default setStaffNumber :: forall f b. (a ~ f b, Functor f, HasStaffNumber b) => Natural -> a -> a
  setStaffNumber s = fmap (setStaffNumber s)

instance HasStaffNumber a => HasStaffNumber (b, a)

instance HasStaffNumber a => HasStaffNumber (Couple b a)

instance HasStaffNumber a => HasStaffNumber [a]

instance HasStaffNumber a => HasStaffNumber (Score a)

staffNumber :: HasStaffNumber a => Natural -> a -> a
staffNumber = setStaffNumber

newtype StaffNumberT a = StaffNumberT {getStaffNumberT :: Couple (First Natural) a}
  deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad, Comonad)

--
-- We use Word instead of Int to get (mempty = Max 0), as (Max.mempty = Max minBound)
-- Preferably we would use Natural but unfortunately this is not an instance of Bounded
--

instance Wrapped (StaffNumberT a) where

  type Unwrapped (StaffNumberT a) = Couple (First Natural) a

  _Wrapped' = iso getStaffNumberT StaffNumberT

instance Rewrapped (StaffNumberT a) (StaffNumberT b)

instance HasStaffNumber (StaffNumberT a) where
  setStaffNumber n = set (_Wrapped . _Wrapped . _1) (First $ Just n)

-- Lifted instances
deriving instance Num a => Num (StaffNumberT a)

deriving instance Fractional a => Fractional (StaffNumberT a)

deriving instance Floating a => Floating (StaffNumberT a)

deriving instance Enum a => Enum (StaffNumberT a)

deriving instance Bounded a => Bounded (StaffNumberT a)

deriving instance (Num a, Ord a, Real a) => Real (StaffNumberT a)

deriving instance (Real a, Enum a, Integral a) => Integral (StaffNumberT a)

deriving instance IsPitch a => IsPitch (StaffNumberT a)

deriving instance IsDynamics a => IsDynamics (StaffNumberT a)

deriving instance Semigroup a => Semigroup (StaffNumberT a)

deriving instance Tiable a => Tiable (StaffNumberT a)

deriving instance HasHarmonic a => HasHarmonic (StaffNumberT a)

deriving instance HasSlide a => HasSlide (StaffNumberT a)

deriving instance HasText a => HasText (StaffNumberT a)

deriving instance Transformable a => Transformable (StaffNumberT a)

deriving instance Alterable a => Alterable (StaffNumberT a)

deriving instance Augmentable a => Augmentable (StaffNumberT a)

type instance Pitch (StaffNumberT a) = Pitch a

type instance SetPitch g (StaffNumberT a) = StaffNumberT (SetPitch g a)

type instance Dynamic (StaffNumberT a) = Dynamic a

type instance SetDynamic g (StaffNumberT a) = StaffNumberT (SetDynamic g a)

type instance Articulation (StaffNumberT a) = Articulation a

type instance SetArticulation g (StaffNumberT a) = StaffNumberT (SetArticulation g a)

instance (HasPitches a b) => HasPitches (StaffNumberT a) (StaffNumberT b) where
  pitches = _Wrapped . pitches

instance (HasPitch a b) => HasPitch (StaffNumberT a) (StaffNumberT b) where
  pitch = _Wrapped . pitch

instance (HasDynamics a b) => HasDynamics (StaffNumberT a) (StaffNumberT b) where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b) => HasDynamic (StaffNumberT a) (StaffNumberT b) where
  dynamic = _Wrapped . dynamic

instance (HasArticulations a b) => HasArticulations (StaffNumberT a) (StaffNumberT b) where
  articulations = _Wrapped . articulations

instance (HasArticulation a b) => HasArticulation (StaffNumberT a) (StaffNumberT b) where
  articulation = _Wrapped . articulation

runStaffNumberT :: StaffNumberT a -> (Natural, a)
runStaffNumberT (StaffNumberT (Couple (First n, a))) = (maybe 0 id n, a)
{-
TODO
  - Add Tecnique as a polymorphic transformer
  - See if the approach for dynamics can be copied exactly for contextual notation
    (see asp2ToAsp3!)
  - Will this be partwise?

-}
