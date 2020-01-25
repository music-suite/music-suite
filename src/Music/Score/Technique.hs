{-# OPTIONS_GHC
  -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports
  #-}
module Music.Score.Technique
  ( -- * Technique
    HasTechnique (..),
    technique,

    -- ** Technique note transformer
    TechniqueT (..),
    runTechniqueT,
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
import Music.Score.Tremolo () -- TODO because of orphans in Tremolo
import Music.Time


type SomeTechnique = () -- TODO

class HasTechnique a where
  setTechnique :: SomeTechnique -> a -> a

instance HasTechnique a => HasTechnique (b, a) where
  setTechnique n = fmap (setTechnique n)

instance HasTechnique a => HasTechnique (Couple b a) where
  setTechnique n = fmap (setTechnique n)

instance HasTechnique a => HasTechnique [a] where
  setTechnique n = fmap (setTechnique n)

instance HasTechnique a => HasTechnique (Score a) where
  setTechnique n = fmap (setTechnique n)

technique :: HasTechnique a => SomeTechnique -> a -> a
technique = setTechnique


newtype TechniqueT a = TechniqueT {getTechniqueT :: Couple SomeTechnique a}
  deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad, Comonad)

--
-- We use Word instead of Int to get (mempty = Max 0), as (Max.mempty = Max minBound)
-- Preferably we would use Natural but unfortunately this is not an instance of Bounded
--

instance Wrapped (TechniqueT a) where

  type Unwrapped (TechniqueT a) = Couple SomeTechnique a

  _Wrapped' = iso getTechniqueT TechniqueT

instance Rewrapped (TechniqueT a) (TechniqueT b)

instance HasTechnique (TechniqueT a) where
  setTechnique n = set (_Wrapped . _Wrapped . _1) n

-- Lifted instances
deriving instance Num a => Num (TechniqueT a)

deriving instance Fractional a => Fractional (TechniqueT a)

deriving instance Floating a => Floating (TechniqueT a)

deriving instance Enum a => Enum (TechniqueT a)

deriving instance Bounded a => Bounded (TechniqueT a)

deriving instance (Num a, Ord a, Real a) => Real (TechniqueT a)

deriving instance (Real a, Enum a, Integral a) => Integral (TechniqueT a)

deriving instance IsPitch a => IsPitch (TechniqueT a)

deriving instance IsDynamics a => IsDynamics (TechniqueT a)

deriving instance Semigroup a => Semigroup (TechniqueT a)

deriving instance Tiable a => Tiable (TechniqueT a)

deriving instance HasHarmonic a => HasHarmonic (TechniqueT a)

deriving instance HasSlide a => HasSlide (TechniqueT a)

deriving instance HasText a => HasText (TechniqueT a)

deriving instance Transformable a => Transformable (TechniqueT a)

deriving instance Reversible a => Reversible (TechniqueT a)

deriving instance Alterable a => Alterable (TechniqueT a)

deriving instance Augmentable a => Augmentable (TechniqueT a)

type instance Pitch (TechniqueT a) = Pitch a

type instance SetPitch g (TechniqueT a) = TechniqueT (SetPitch g a)

type instance Dynamic (TechniqueT a) = Dynamic a

type instance SetDynamic g (TechniqueT a) = TechniqueT (SetDynamic g a)

type instance Articulation (TechniqueT a) = Articulation a

type instance SetArticulation g (TechniqueT a) = TechniqueT (SetArticulation g a)

instance (HasPitches a b) => HasPitches (TechniqueT a) (TechniqueT b) where
  pitches = _Wrapped . pitches

instance (HasPitch a b) => HasPitch (TechniqueT a) (TechniqueT b) where
  pitch = _Wrapped . pitch

instance (HasDynamics a b) => HasDynamics (TechniqueT a) (TechniqueT b) where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b) => HasDynamic (TechniqueT a) (TechniqueT b) where
  dynamic = _Wrapped . dynamic

instance (HasArticulations a b) => HasArticulations (TechniqueT a) (TechniqueT b) where
  articulations = _Wrapped . articulations

instance (HasArticulation a b) => HasArticulation (TechniqueT a) (TechniqueT b) where
  articulation = _Wrapped . articulation

runTechniqueT :: TechniqueT a -> (SomeTechnique, a)
runTechniqueT (TechniqueT (Couple (n, a))) = (n, a)

{-
TODO needs polymorphic transformer for this to work...

addArtCon ::
  ( HasPhrases s t a b,
    HasTechnique a,
    HasTechnique b,
    Articulation a ~ d,
    Articulation b ~ Ctxt d
  ) =>
  s ->
  t
addArtCon = over (phrases . varticulation) withContext

varticulation ::
  (HasArticulation s s, HasArticulation s t) =>
  Lens (Voice s) (Voice t) (Voice (Articulation s)) (Voice (Articulation t))
varticulation = lens (fmap $ viewTechnique) (flip $ zipVoiceWithNoScale (setTechnique ))

-}
