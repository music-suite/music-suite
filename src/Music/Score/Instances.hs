
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

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
-- Instances for note transformers. This module should be imported as part
-- of "Music.Score".
--
-------------------------------------------------------------------------------------

module Music.Score.Instances () where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens             hiding (part, transform)
import           Control.Monad
import           Data.AffineSpace
import           Data.Default
import           Data.Foldable
import           Data.Functor.Adjunction  (unzipR)
import           Data.Functor.Couple
import qualified Data.List                as List
import           Data.Maybe
import           Data.Ratio
import           Data.Semigroup
import           Data.Typeable
import           Data.VectorSpace

import           Music.Dynamics.Literal
import           Music.Pitch.Alterable
import           Music.Pitch.Augmentable
import           Music.Pitch.Literal
import           Music.Score.Articulation
import           Music.Score.Color
import           Music.Score.Dynamics
import           Music.Score.Harmonics
import           Music.Score.Meta
import           Music.Score.Part
import           Music.Score.Pitch
import           Music.Score.Slide
import           Music.Score.Text
import           Music.Score.Ties
import           Music.Score.Tremolo
import           Music.Time

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
instance Semigroup a => Semigroup (TremoloT a) where
    (<>) = liftA2 (<>)
instance Semigroup a => Semigroup (PartT n a) where
    PartT (v1,x1) <> PartT (v2,x2) = PartT (v1, x1 <> x2)


-- -------------------------------------------------------------------------------------

--
-- Aspect instaces (Pitch, Dynamics and Articulation) for PartT needs to go here,
-- as the other aspects depends on partwise traversals etc
--

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
type instance Pitch (ColorT a)        = Pitch a
type instance SetPitch g (ColorT a)   = ColorT (SetPitch g a)
instance (HasPitches a b) => HasPitches (ColorT a) (ColorT b) where
  pitches = _Wrapped . pitches
instance (HasPitch a b) => HasPitch (ColorT a) (ColorT b) where
  pitch = _Wrapped . pitch

type instance Dynamic (ColorT a)        = Dynamic a
type instance SetDynamic g (ColorT a)   = ColorT (SetDynamic g a)
instance (HasDynamics a b) => HasDynamics (ColorT a) (ColorT b) where
  dynamics = _Wrapped . dynamics
instance (HasDynamic a b) => HasDynamic (ColorT a) (ColorT b) where
  dynamic = _Wrapped . dynamic

type instance Articulation (ColorT a)        = Articulation a
type instance SetArticulation g (ColorT a)   = ColorT (SetArticulation g a)
instance (HasArticulations a b) => HasArticulations (ColorT a) (ColorT b) where
  articulations = _Wrapped . articulations
instance (HasArticulation a b) => HasArticulation (ColorT a) (ColorT b) where
  articulation = _Wrapped . articulation


-- -------------------------------------------------------------------------------------

deriving instance HasTremolo a => HasTremolo (PartT n a)
deriving instance HasHarmonic a => HasHarmonic (PartT n a)
deriving instance HasSlide a => HasSlide (PartT n a)
deriving instance HasText a => HasText (PartT n a)


deriving instance HasTremolo a => HasTremolo (TieT a)
deriving instance HasHarmonic a => HasHarmonic (TieT a)
deriving instance HasSlide a => HasSlide (TieT a)
deriving instance HasText a => HasText (TieT a)

instance Tiable a => Tiable (TremoloT a) where
    toTied = unzipR . fmap toTied

type instance Part (TremoloT a) = Part a
deriving instance HasHarmonic a => HasHarmonic (TremoloT a)
deriving instance HasSlide a => HasSlide (TremoloT a)
deriving instance HasText a => HasText (TremoloT a)


-- TextT

instance Tiable a => Tiable (TextT a) where
  toTied (TextT (Couple (n,a))) = (TextT (Couple (n,b)), TextT (Couple (mempty,c))) where (b,c) = toTied a
deriving instance HasTremolo a => HasTremolo (TextT a)
deriving instance HasHarmonic a => HasHarmonic (TextT a)
deriving instance HasSlide a => HasSlide (TextT a)


-- HarmonicT

instance Tiable a => Tiable (HarmonicT a) where
  -- toTied = unzipR . fmap toTied
  toTied (HarmonicT (Couple (n,a))) = (HarmonicT (Couple (n,b)), HarmonicT (Couple (mempty,c))) where (b,c) = toTied a
deriving instance HasTremolo a => HasTremolo (HarmonicT a)
deriving instance HasSlide a => HasSlide (HarmonicT a)
deriving instance HasText a => HasText (HarmonicT a)


-- SlideT


instance Tiable a => Tiable (SlideT a) where
  toTied = unzipR . fmap toTied
deriving instance HasTremolo a => HasTremolo (SlideT a)
deriving instance HasHarmonic a => HasHarmonic (SlideT a)
deriving instance HasText a => HasText (SlideT a)



deriving instance (Monoid b, IsPitch a) => IsPitch (Couple b a)
deriving instance (Monoid b, IsDynamics a) => IsDynamics (Couple b a)
deriving instance (Monoid b, Transformable a) => Transformable (Couple b a)
deriving instance (Monoid b, Reversible a) => Reversible (Couple b a)
deriving instance (Monoid b, Alterable a) => Alterable (Couple b a)
deriving instance (Monoid b, Augmentable a) => Augmentable (Couple b a)

deriving instance IsPitch a => IsPitch (TremoloT a)
deriving instance IsDynamics a => IsDynamics (TremoloT a)
deriving instance IsPitch a => IsPitch (TextT a)
deriving instance IsDynamics a => IsDynamics (TextT a)
deriving instance IsPitch a => IsPitch (HarmonicT a)
deriving instance IsDynamics a => IsDynamics (HarmonicT a)
deriving instance IsPitch a => IsPitch (SlideT a)
deriving instance IsDynamics a => IsDynamics (SlideT a)

deriving instance IsPitch a => IsPitch (ColorT a)
deriving instance IsDynamics a => IsDynamics (ColorT a)
deriving instance Transformable a => Transformable (ColorT a)
deriving instance Reversible a => Reversible (ColorT a)
deriving instance Alterable a => Alterable (ColorT a)
deriving instance Augmentable a => Augmentable (ColorT a)
deriving instance HasTremolo a => HasTremolo (ColorT a)
deriving instance HasHarmonic a => HasHarmonic (ColorT a)
deriving instance HasSlide a => HasSlide (ColorT a)
deriving instance HasText a => HasText (ColorT a)

deriving instance Transformable a => Transformable (SlideT a)
deriving instance Transformable a => Transformable (HarmonicT a)
deriving instance Transformable a => Transformable (TextT a)
deriving instance Transformable a => Transformable (TremoloT a)

deriving instance Reversible a => Reversible (SlideT a)
deriving instance Reversible a => Reversible (HarmonicT a)
deriving instance Reversible a => Reversible (TextT a)
deriving instance Reversible a => Reversible (TremoloT a)

deriving instance Alterable a => Alterable (SlideT a)
deriving instance Alterable a => Alterable (HarmonicT a)
deriving instance Alterable a => Alterable (TextT a)
deriving instance Alterable a => Alterable (TremoloT a)

deriving instance Augmentable a => Augmentable (SlideT a)
deriving instance Augmentable a => Augmentable (HarmonicT a)
deriving instance Augmentable a => Augmentable (TextT a)
deriving instance Augmentable a => Augmentable (TremoloT a)



-------------------------------------------------------------------------------------
-- Literal instances
-------------------------------------------------------------------------------------


instance Alterable a => Alterable (Score a) where
    sharpen = fmap sharpen
    flatten = fmap flatten
deriving instance Alterable a => Alterable (TieT a)
deriving instance Alterable a => Alterable (PartT n a)
deriving instance Alterable a => Alterable (DynamicT n a)
deriving instance Alterable a => Alterable (ArticulationT n a)

instance Augmentable a => Augmentable (Score a) where
    augment = fmap augment
    diminish = fmap diminish
deriving instance Augmentable a => Augmentable (TieT a)
deriving instance Augmentable a => Augmentable (PartT n a)
deriving instance Augmentable a => Augmentable (DynamicT n a)
deriving instance Augmentable a => Augmentable (ArticulationT n a)


-- -------------------------------------------------------------------------------------
-- -- Num, Integral, Enum and Bounded
-- -------------------------------------------------------------------------------------
--
-- PartT

instance (Enum v, Eq v, Num a) => Num (PartT v a) where
    PartT (v,a) + PartT (_,b) = PartT (v,a+b)
    PartT (v,a) * PartT (_,b) = PartT (v,a*b)
    PartT (v,a) - PartT (_,b) = PartT (v,a-b)
    abs (PartT (v,a))          = PartT (v,abs a)
    signum (PartT (v,a))       = PartT (v,signum a)
    fromInteger a               = PartT (toEnum 0,fromInteger a)

instance (Enum v, Enum a) => Enum (PartT v a) where
    toEnum a = PartT (toEnum 0, toEnum a) -- TODO use def, mempty or minBound?
    fromEnum (PartT (v,a)) = fromEnum a

instance (Enum v, Bounded a) => Bounded (PartT v a) where
    minBound = PartT (toEnum 0, minBound)
    maxBound = PartT (toEnum 0, maxBound)

instance (Enum v, Ord v, Num a, Ord a, Real a) => Real (PartT v a) where
    toRational (PartT (v,a)) = toRational a

instance (Enum v, Ord v, Real a, Enum a, Integral a) => Integral (PartT v a) where
    PartT (v,a) `quotRem` PartT (_,b) = (PartT (v,q), PartT (v,r)) where (q,r) = a `quotRem` b
    toInteger (PartT (v,a)) = toInteger a

--
-- TODO suspect instances
-- We should remove both these after replacing [] by Chord in Preludes
--
instance Enum a => Enum [a] where
    toEnum a       = [toEnum a]
    fromEnum ([a]) = fromEnum a

instance Bounded a => Bounded [a] where
    minBound = [minBound]
    maxBound = [maxBound]



-- TODO move
-- TODO derive more of these?
instance Applicative Product where
  pure = Product
  Product f <*> Product x = Product (f x)
deriving instance Floating a => Floating (Product a)
instance Num a => Num (Product a) where
  fromInteger = pure . fromInteger
  abs    = fmap abs
  signum = fmap signum
  (+)    = liftA2 (+)
  (-)    = liftA2 (-)
  (*)    = liftA2 (*)
instance Fractional a => Fractional (Product a) where
  fromRational = pure . fromRational
  (/) = liftA2 (/)
instance Real a => Real (Product a) where
  toRational (Product x) = toRational x



-- TODO use wrapper type and replace withContext
type instance Dynamic (a,b,c) = (a,b,c)
type instance SetDynamic g (a,b,c) = g

instance Transformable a => Transformable (Maybe a) where
  transform s = fmap (transform s)
instance (Transformable a, Transformable b, Transformable c) => Transformable (a,b,c) where
  transform s (a,b,c) = (transform s a,transform s b,transform s c)

deriving instance IsDynamics a => IsDynamics (Product a)
deriving instance AdditiveGroup a => AdditiveGroup (Product a)
instance VectorSpace a => VectorSpace (Product a) where
  type Scalar (Product a) = Scalar a
  x *^ Product y = Product (x *^ y)
instance AffineSpace a => AffineSpace (Product a) where
  type Diff (Product a) = Product (Diff a)
  Product p .-. Product q = Product (p .-. q)
  Product p .+^ Product v = Product (p .+^ v)


