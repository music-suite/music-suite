{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints
  -fno-warn-orphans #-}

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides miscellaneous instances.
module Music.Score.Internal.Instances
  (
  )
where

import Control.Applicative
import Control.Comonad
import Control.Lens hiding (transform)
import Control.Monad
import Data.AffineSpace
import Data.Foldable
import Data.Functor.Context
import Data.Functor.Couple
import qualified Data.List as List
import Data.Maybe
import Data.Monoid.Average
import Data.Ratio
import Data.Semigroup
import Data.Semigroup.Instances
import Data.Typeable
import Data.VectorSpace hiding (Sum)
import Music.Dynamics.Literal
import qualified Music.Parts
import qualified Music.Pitch
import Music.Pitch.Alterable
import Music.Pitch.Augmentable
import qualified Music.Pitch.Common as Common
import Music.Pitch.Literal
import Music.Score.Articulation
import Music.Score.Color
import Music.Score.Dynamics
import Music.Score.Harmonics
import Music.Score.Meta
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Slide
import Music.Score.StaffNumber
import Music.Score.Technique
import Music.Score.Text
import Music.Score.Ties
import Music.Score.Tremolo
import Music.Time

-- -------------------------------------------------------------------------------------
--
-- instance Semigroup a => Semigroup (DynamicT a) where
--     DynamicT (d1, x1) <> DynamicT (d2, x2) = DynamicT (d1 <> d2, x1 <> x2)
instance Semigroup a => Semigroup (SlideT a) where
  (<>) = liftA2 (<>)

instance Semigroup a => Semigroup (TieT a) where
  TieT (t1, x1) <> TieT (t2, x2) = TieT (t1 <> t2, x1 <> x2)

-- This instance is suspect: in general chord notes are not required to share ties,
-- so this instance may be removed (provided that TieT is moved inside Chord for
-- all Preludes). See #134
instance Semigroup a => Semigroup (HarmonicT a) where
  (<>) = liftA2 (<>)

instance Semigroup a => Semigroup (TextT a) where
  (<>) = liftA2 (<>)

instance Semigroup a => Semigroup (PartT n a) where
  PartT (v1, x1) <> PartT (_v2, x2) = PartT (v1, x1 <> x2)

-- -------------------------------------------------------------------------------------

--
-- Aspect instaces (Pitch, Dynamics and Articulation) for PartT needs to go here,
-- as the other aspects depends on partwise traversals etc
--
type instance Pitch (TechniqueT p a) = Pitch a

type instance SetPitch b (TechniqueT p a) = TechniqueT p (SetPitch b a)

instance HasPitch a b => HasPitch (TechniqueT p a) (TechniqueT p b) where
  pitch = _Wrapped . pitch

instance HasPitches a b => HasPitches (TechniqueT p a) (TechniqueT p b) where
  pitches = _Wrapped . pitches

type instance Dynamic (TechniqueT p a) = Dynamic a

type instance SetDynamic b (TechniqueT p a) = TechniqueT p (SetDynamic b a)

instance HasDynamic a b => HasDynamic (TechniqueT p a) (TechniqueT p b) where
  dynamic = _Wrapped . dynamic

instance HasDynamics a b => HasDynamics (TechniqueT p a) (TechniqueT p b) where
  dynamics = _Wrapped . dynamics

type instance Articulation (TechniqueT p a) = Articulation a

type instance SetArticulation b (TechniqueT p a) = TechniqueT p (SetArticulation b a)

instance HasArticulation a b => HasArticulation (TechniqueT p a) (TechniqueT p b) where
  articulation = _Wrapped . articulation

instance HasArticulations a b => HasArticulations (TechniqueT p a) (TechniqueT p b) where
  articulations = _Wrapped . articulations

type instance Pitch (PartT p a) = Pitch a

type instance SetPitch b (PartT p a) = PartT p (SetPitch b a)

instance HasPitch a b => HasPitch (PartT p a) (PartT p b) where
  pitch = _Wrapped . _2 . pitch

instance HasPitches a b => HasPitches (PartT p a) (PartT p b) where
  pitches = _Wrapped . _2 . pitches

type instance Pitch (DynamicT p a) = Pitch a

type instance SetPitch b (DynamicT p a) = DynamicT p (SetPitch b a)

instance HasPitch a b => HasPitch (DynamicT p a) (DynamicT p b) where
  pitch = _Wrapped . _2 . pitch

instance HasPitches a b => HasPitches (DynamicT p a) (DynamicT p b) where
  pitches = _Wrapped . _2 . pitches

type instance Pitch (ArticulationT p a) = Pitch a

type instance SetPitch b (ArticulationT p a) = ArticulationT p (SetPitch b a)

instance HasPitch a b => HasPitch (ArticulationT p a) (ArticulationT p b) where
  pitch = _Wrapped . _2 . pitch

instance HasPitches a b => HasPitches (ArticulationT p a) (ArticulationT p b) where
  pitches = _Wrapped . _2 . pitches

type instance Dynamic (PartT p a) = Dynamic a

type instance SetDynamic b (PartT p a) = PartT p (SetDynamic b a)

instance HasDynamic a b => HasDynamic (PartT p a) (PartT p b) where
  dynamic = _Wrapped . _2 . dynamic

instance HasDynamics a b => HasDynamics (PartT p a) (PartT p b) where
  dynamics = _Wrapped . _2 . dynamics

type instance Dynamic (ArticulationT p a) = Dynamic a

type instance SetDynamic b (ArticulationT p a) = ArticulationT p (SetDynamic b a)

instance HasDynamic a b => HasDynamic (ArticulationT p a) (ArticulationT p b) where
  dynamic = _Wrapped . _2 . dynamic

instance HasDynamics a b => HasDynamics (ArticulationT p a) (ArticulationT p b) where
  dynamics = _Wrapped . _2 . dynamics

type instance Articulation (PartT p a) = Articulation a

type instance SetArticulation b (PartT p a) = PartT p (SetArticulation b a)

instance HasArticulation a b => HasArticulation (PartT p a) (PartT p b) where
  articulation = _Wrapped . _2 . articulation

instance HasArticulations a b => HasArticulations (PartT p a) (PartT p b) where
  articulations = _Wrapped . _2 . articulations

-- TODO move up?
type instance Pitch (ColorT a) = Pitch a

type instance SetPitch g (ColorT a) = ColorT (SetPitch g a)

instance (HasPitches a b) => HasPitches (ColorT a) (ColorT b) where
  pitches = _Wrapped . pitches

instance (HasPitch a b) => HasPitch (ColorT a) (ColorT b) where
  pitch = _Wrapped . pitch

type instance Dynamic (ColorT a) = Dynamic a

type instance SetDynamic g (ColorT a) = ColorT (SetDynamic g a)

instance (HasDynamics a b) => HasDynamics (ColorT a) (ColorT b) where
  dynamics = _Wrapped . dynamics

instance (HasDynamic a b) => HasDynamic (ColorT a) (ColorT b) where
  dynamic = _Wrapped . dynamic

type instance Articulation (ColorT a) = Articulation a

type instance SetArticulation g (ColorT a) = ColorT (SetArticulation g a)

instance (HasArticulations a b) => HasArticulations (ColorT a) (ColorT b) where
  articulations = _Wrapped . articulations

instance (HasArticulation a b) => HasArticulation (ColorT a) (ColorT b) where
  articulation = _Wrapped . articulation

-- -------------------------------------------------------------------------------------
deriving instance HasColor a => HasColor (TremoloT a)

deriving instance HasColor a => HasColor (StaffNumberT a)

deriving instance HasTremolo a => HasTremolo (PartT n a)

deriving instance HasTremolo a => HasTremolo (StaffNumberT a)

deriving instance HasHarmonic a => HasHarmonic (PartT n a)

deriving instance HasSlide a => HasSlide (PartT n a)

deriving instance HasText a => HasText (PartT n a)

deriving instance HasTremolo a => HasTremolo (TechniqueT n a)

deriving instance HasHarmonic a => HasHarmonic (TechniqueT n a)

deriving instance HasSlide a => HasSlide (TechniqueT n a)

deriving instance HasText a => HasText (TechniqueT n a)

deriving instance HasTremolo a => HasTremolo (TieT a)

deriving instance HasHarmonic a => HasHarmonic (TieT a)

deriving instance HasSlide a => HasSlide (TieT a)

deriving instance HasText a => HasText (TieT a)

-- TextT

instance Tiable a => Tiable (TextT a) where
  toTied (TextT (Couple (n, a))) = (TextT (Couple (n, b)), TextT (Couple (mempty, c))) where (b, c) = toTied a

deriving instance HasTremolo a => HasTremolo (TextT a)

deriving instance HasHarmonic a => HasHarmonic (TextT a)

deriving instance HasSlide a => HasSlide (TextT a)

-- HarmonicT

instance Tiable a => Tiable (HarmonicT a) where
  -- toTied = unzipR . fmap toTied
  toTied (HarmonicT (Couple (n, a))) = (HarmonicT (Couple (n, b)), HarmonicT (Couple (mempty, c))) where (b, c) = toTied a

deriving instance HasTremolo a => HasTremolo (HarmonicT a)

deriving instance HasSlide a => HasSlide (HarmonicT a)

deriving instance HasText a => HasText (HarmonicT a)

-- SlideT

instance Tiable a => Tiable (SlideT a) where
  toTied = unzipR . fmap toTied

deriving instance HasTremolo a => HasTremolo (SlideT a)

deriving instance HasHarmonic a => HasHarmonic (SlideT a)

deriving instance HasText a => HasText (SlideT a)

deriving instance IsPitch a => IsPitch (TextT a)

deriving instance IsDynamics a => IsDynamics (TextT a)

deriving instance IsPitch a => IsPitch (HarmonicT a)

deriving instance IsDynamics a => IsDynamics (HarmonicT a)

deriving instance IsPitch a => IsPitch (SlideT a)

deriving instance IsDynamics a => IsDynamics (SlideT a)

deriving instance IsPitch a => IsPitch (ColorT a)

deriving instance IsDynamics a => IsDynamics (ColorT a)

deriving instance Transformable a => Transformable (ColorT a)

deriving instance Alterable a => Alterable (ColorT a)

deriving instance Augmentable a => Augmentable (ColorT a)

deriving instance HasTremolo a => HasTremolo (ColorT a)

deriving instance HasHarmonic a => HasHarmonic (ColorT a)

deriving instance HasSlide a => HasSlide (ColorT a)

deriving instance HasText a => HasText (ColorT a)

deriving instance Transformable a => Transformable (SlideT a)

deriving instance Transformable a => Transformable (HarmonicT a)

deriving instance Transformable a => Transformable (TextT a)

deriving instance Alterable a => Alterable (SlideT a)

deriving instance Alterable a => Alterable (HarmonicT a)

deriving instance Alterable a => Alterable (TextT a)

deriving instance Augmentable a => Augmentable (SlideT a)

deriving instance Augmentable a => Augmentable (HarmonicT a)

deriving instance Augmentable a => Augmentable (TextT a)

-------------------------------------------------------------------------------------
-- Literal instances
-------------------------------------------------------------------------------------

instance Alterable a => Alterable (Score a) where

  sharpen = fmap sharpen

  flatten = fmap flatten

deriving instance Alterable a => Alterable (TieT a)

deriving instance Alterable a => Alterable (PartT n a)

deriving instance Alterable a => Alterable (TechniqueT n a)

deriving instance Alterable a => Alterable (DynamicT n a)

deriving instance Alterable a => Alterable (ArticulationT n a)

instance Augmentable a => Augmentable (Score a) where

  augment = fmap augment

  diminish = fmap diminish

deriving instance Augmentable a => Augmentable (TieT a)

deriving instance Augmentable a => Augmentable (PartT n a)

deriving instance Augmentable a => Augmentable (TechniqueT n a)

deriving instance Augmentable a => Augmentable (DynamicT n a)

deriving instance Augmentable a => Augmentable (ArticulationT n a)

-- -------------------------------------------------------------------------------------
-- -- Num, Integral, Enum and Bounded
-- -------------------------------------------------------------------------------------
--
-- PartT

instance (Enum v, Eq v, Num a) => Num (PartT v a) where

  PartT (v, a) + PartT (_, b) = PartT (v, a + b)

  PartT (v, a) * PartT (_, b) = PartT (v, a * b)

  PartT (v, a) - PartT (_, b) = PartT (v, a - b)

  abs (PartT (v, a)) = PartT (v, abs a)

  signum (PartT (v, a)) = PartT (v, signum a)

  fromInteger a = PartT (toEnum 0, fromInteger a)

instance (Monoid v, Enum a) => Enum (PartT v a) where

  toEnum a = PartT (mempty, toEnum a)

  fromEnum (PartT (_v, a)) = fromEnum a

instance (Monoid v, Bounded a) => Bounded (PartT v a) where

  minBound = PartT (mempty, minBound)

  maxBound = PartT (mempty, maxBound)

-- instance (Enum v, Ord v, Num a, Ord a, Real a) => Real (PartT v a) where
--   toRational (PartT (v, a)) = toRational a

-- instance (Monoid v, Ord v, Real a, Enum a, Integral a) => Integral (PartT v a) where
--
--   PartT (v, a) `quotRem` PartT (_, b) = (PartT (v, q), PartT (v, r)) where (q, r) = a `quotRem` b
--
--   toInteger (PartT (v, a)) = toInteger a

--
-- TODO suspect instances
-- We should remove both these after replacing [] by Chord in Preludes
--
-- instance Enum a => Enum [a] where
--     toEnum a       = [toEnum a]
--     fromEnum ([a]) = fromEnum a

-- instance Bounded a => Bounded [a] where
--
--   minBound = [minBound]
--
--   maxBound = [maxBound]

-- TODO use wrapper type and replace withContext
type instance Dynamic (a, b, c) = (a, b, c)

type instance SetDynamic g (a, b, c) = g

-- instance (Transformable a, Transformable b, Transformable c) => Transformable (a,b,c) where
--   transform s (a,b,c) = (transform s a,transform s b,transform s c)

-- TODO place for this?
-- For use with single-note scores etc
instance Tiable a => Tiable (Score a) where

  beginTie = fmap beginTie

  endTie = fmap endTie

instance Transformable a => Transformable (Ctxt a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (Average a) where
  transform s = fmap (transform s)

instance IsPitch a => IsPitch (Average a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Average a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Average a) where
  fromDynamics = pure . fromDynamics

deriving instance Typeable Music.Parts.Part

instance Tiable Music.Pitch.Pitch where

  beginTie = id

  endTie = id

instance Transformable Music.Parts.Part where
  transform _ = id

type instance Part Music.Parts.Part = Music.Parts.Part

type instance SetPart a Music.Parts.Part = a

instance (Transformable a, a ~ Part a) => HasPart Music.Parts.Part a where
  part = ($)

instance (Transformable a, a ~ Part a) => HasParts Music.Parts.Part a where
  parts = ($)

instance HasDuration Common.Pitch where
  _duration = const 1

instance HasDuration a => HasDuration (PartT p a) where
  _duration = _duration . extract

instance HasDuration a => HasDuration (ColorT a) where
  _duration = _duration . extract

instance HasDuration a => HasDuration (TextT a) where
  _duration = _duration . extract

instance HasDuration a => HasDuration (StaffNumberT a) where
  _duration = _duration . extract

instance HasDuration a => HasDuration (TremoloT a) where
  _duration = _duration . extract

instance HasDuration a => HasDuration (HarmonicT a) where
  _duration = _duration . extract

instance HasDuration a => HasDuration (SlideT a) where
  _duration = _duration . extract

instance HasDuration a => HasDuration (ArticulationT b a) where
  _duration = _duration . extract

instance HasDuration a => HasDuration (DynamicT b a) where
  _duration = _duration . extract

instance HasDuration a => HasDuration (TechniqueT b a) where
  _duration = _duration . extract

instance HasDuration a => HasDuration (TieT a) where
  _duration = _duration . extract

instance Splittable Common.Pitch where
  split _ x = (x, x)

instance Splittable a => Splittable (PartT p a) where
  split t = unzipR . fmap (split t)

instance Splittable a => Splittable (TechniqueT p a) where
  split t = unzipR . fmap (split t)

instance Splittable a => Splittable (ColorT a) where
  split t = unzipR . fmap (split t)

instance Splittable a => Splittable (TextT a) where
  split t = unzipR . fmap (split t)

instance Splittable a => Splittable (StaffNumberT a) where
  split t = unzipR . fmap (split t)

instance Splittable a => Splittable (TremoloT a) where
  split t = unzipR . fmap (split t)

instance Splittable a => Splittable (HarmonicT a) where
  split t = unzipR . fmap (split t)

instance Splittable a => Splittable (SlideT a) where
  split t = unzipR . fmap (split t)

instance Splittable a => Splittable (ArticulationT b a) where
  split t = unzipR . fmap (split t)

instance Splittable a => Splittable (DynamicT b a) where
  split t = unzipR . fmap (split t)

instance Splittable a => Splittable (TieT a) where
  split t = unzipR . fmap (split t)

unzipR :: Functor f => f (a, b) -> (f a, f b)
unzipR x = (fmap fst x, fmap snd x)
