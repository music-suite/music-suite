
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
-- Copyright   : (c) Hans Hoglund 2012
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
import           Control.Monad
import           Control.Lens hiding (transform, part)
import           Data.AffineSpace
import           Data.Functor.Adjunction (unzipR)
import           Data.Default
import           Data.Foldable
import qualified Data.List                as List
import           Data.Maybe
import           Data.Ratio
import           Data.Semigroup
import           Data.Typeable
import           Data.VectorSpace

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Pitch.Augmentable
import           Music.Pitch.Alterable
import           Music.Time
import           Music.Score.Meta
import           Music.Score.Meta2
import           Music.Score.Articulation
import           Music.Score.Dynamics
import           Music.Score.Part
import           Music.Score.Pitch
import           Music.Score.Ornaments
import           Music.Score.Rhythm
import           Music.Score.Ties
import           Music.Score.Util

-------------------------------------------------------------------------------------

-- TODO move to pitch-literal
instance IsPitch a => IsPitch [a] where
    fromPitch = return . fromPitch
instance IsDynamics a => IsDynamics [a] where
    fromDynamics = return . fromDynamics

-- instance IsPitch a => IsPitch (DynamicT a) where
--     fromPitch = pure . fromPitch
-- instance IsDynamics a => IsDynamics (DynamicT a) where
--     fromDynamics = return . fromDynamics
-- 
-- instance IsPitch a => IsPitch (ArticulationT a) where
--     fromPitch = pure . fromPitch
-- instance IsDynamics a => IsDynamics (ArticulationT a) where
--     fromDynamics = return . fromDynamics
-- 
-- -------------------------------------------------------------------------------------
-- 
-- instance Transformable a => Transformable (DynamicT a) where
--     transform s = fmap (transform s)
-- instance Transformable a => Transformable (ArticulationT a) where
--     transform s = fmap (transform s)


-- -------------------------------------------------------------------------------------
-- 
-- instance Reversible a => Reversible (DynamicT a) where
--     rev = fmap rev
-- instance Reversible a => Reversible (ArticulationT a) where
--     rev = fmap rev

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
-- 
-- 
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

-- -------------------------------------------------------------------------------------




deriving instance HasTremolo a => HasTremolo (PartT n a)
deriving instance HasHarmonic a => HasHarmonic (PartT n a)
deriving instance HasSlide a => HasSlide (PartT n a)
deriving instance HasText a => HasText (PartT n a)


deriving instance HasTremolo a => HasTremolo (TieT a)
deriving instance HasHarmonic a => HasHarmonic (TieT a)
deriving instance HasSlide a => HasSlide (TieT a)
deriving instance HasText a => HasText (TieT a)

-- 
-- 
-- -- DynamicT
-- 
-- -- end cresc/dim, level, begin cresc/dim
-- -- newtype DynamicT a = DynamicT { getDynamicT :: (Bool, Bool, Maybe Double, a, Bool, Bool) }
-- 
-- instance Tiable a => Tiable (DynamicT a) where
--     toTied (DynamicT (l, a)) = (DynamicT (l, b), DynamicT (mempty, c)) where (b,c) = toTied a
-- 
-- deriving instance HasTremolo a => HasTremolo (DynamicT a)
-- deriving instance HasHarmonic a => HasHarmonic (DynamicT a)
-- deriving instance HasSlide a => HasSlide (DynamicT a)
-- deriving instance HasText a => HasText (DynamicT a)
-- 
-- 
-- -- ArticulationT
-- 
-- -- end slur, cont slur, acc level, stacc level, begin slur
-- -- newtype ArticulationT a = ArticulationT { getArticulationT :: (Bool, Bool, Int, Int, a, Bool) }
-- 
-- 
-- instance Tiable a => Tiable (ArticulationT a) where
--     toTied (ArticulationT (v,a)) = (ArticulationT (v,b), ArticulationT (v,c)) where (b,c) = toTied a
-- 
-- deriving instance HasTremolo a => HasTremolo (ArticulationT a)
-- deriving instance HasHarmonic a => HasHarmonic (ArticulationT a)
-- deriving instance HasSlide a => HasSlide (ArticulationT a)
-- deriving instance HasText a => HasText (ArticulationT a)
-- 
-- -- TremoloT
-- 
instance Tiable a => Tiable (TremoloT a) where
    toTied (TremoloT (n,a))                         = (TremoloT (n,b), TremoloT (n,c)) where (b,c) = toTied a
type instance Part (TremoloT a)                          = Part a
deriving instance HasHarmonic a => HasHarmonic (TremoloT a)
deriving instance HasSlide a => HasSlide (TremoloT a)
deriving instance HasText a => HasText (TremoloT a)
-- 
-- 
-- -- TextT
-- 
instance Tiable a => Tiable (TextT a) where
    toTied (TextT (n,a))                            = (TextT (n,b), TextT (mempty,c)) where (b,c) = toTied a
deriving instance HasTremolo a => HasTremolo (TextT a)
deriving instance HasHarmonic a => HasHarmonic (TextT a)
deriving instance HasSlide a => HasSlide (TextT a)
-- 
-- 
-- -- HarmonicT
-- 
instance Tiable a => Tiable (HarmonicT a) where
    toTied (HarmonicT (n,a))                        = (HarmonicT (n,b), HarmonicT (n,c)) where (b,c) = toTied a
deriving instance HasTremolo a => HasTremolo (HarmonicT a)
deriving instance HasSlide a => HasSlide (HarmonicT a)
deriving instance HasText a => HasText (HarmonicT a)
-- 
-- 
-- -- SlideT
-- 
-- 
instance Tiable a => Tiable (SlideT a) where
    toTied (SlideT (v,x)) = (SlideT (v,a), SlideT (v,b)) where (a,b) = toTied x
deriving instance HasTremolo a => HasTremolo (SlideT a)
deriving instance HasHarmonic a => HasHarmonic (SlideT a)
deriving instance HasText a => HasText (SlideT a)
-- 
-- 
-- 
-- -------------------------------------------------------------------------------------
-- -- Literal instances
-- -------------------------------------------------------------------------------------
-- 
instance Alterable a => Alterable (Score a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (SlideT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (TieT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (HarmonicT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

-- instance Alterable a => Alterable (ArticulationT a) where
--     sharpen = fmap sharpen
--     flatten = fmap flatten
-- 
instance Alterable a => Alterable (TextT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (TremoloT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (PartT n a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Augmentable a => Augmentable (Score a) where
    augment = fmap augment
    diminish = fmap diminish

-- instance Augmentable a => Augmentable (DynamicT a) where
--     augment = fmap augment
--     diminish = fmap diminish
-- 
instance Augmentable a => Augmentable (SlideT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (TieT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (HarmonicT a) where
    augment = fmap augment
    diminish = fmap diminish

-- instance Augmentable a => Augmentable (ArticulationT a) where
--     augment = fmap augment
--     diminish = fmap diminish

instance Augmentable a => Augmentable (TextT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (TremoloT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (PartT n a) where
    augment = fmap augment
    diminish = fmap diminish
-- 
-- 
-- 
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

-- 
-- 
-- -- DynamicT
-- 
-- instance Num a => Num (DynamicT a) where
--     (+) = liftA2 (+)
--     (*) = liftA2 (*)
--     (-) = liftA2 (-)
--     abs = fmap abs
--     signum = fmap signum
--     fromInteger = pure . fromInteger
-- 
-- instance Fractional a => Fractional (DynamicT a) where
--     recip        = fmap recip
--     fromRational = pure . fromRational
-- 
-- instance Floating a => Floating (DynamicT a) where
--     pi    = pure pi
--     sqrt  = fmap sqrt
--     exp   = fmap exp
--     log   = fmap log
--     sin   = fmap sin
--     cos   = fmap cos
--     asin  = fmap asin
--     atan  = fmap atan
--     acos  = fmap acos
--     sinh  = fmap sinh
--     cosh  = fmap cosh
--     asinh = fmap asinh
--     atanh = fmap atanh
--     acosh = fmap acos
-- 
-- instance Enum a => Enum (DynamicT a) where
--     toEnum = pure . toEnum
--     fromEnum = fromEnum . get1
-- 
-- instance Bounded a => Bounded (DynamicT a) where
--     minBound = pure minBound
--     maxBound = pure maxBound
-- 
-- instance (Num a, Ord a, Real a) => Real (DynamicT a) where
--     toRational = toRational . get1
-- 
-- instance (Real a, Enum a, Integral a) => Integral (DynamicT a) where
--     quot = liftA2 quot
--     rem = liftA2 rem
--     toInteger = toInteger . get1
-- 
-- 
-- -- ArticulationT
-- 
-- instance Num a => Num (ArticulationT a) where
--     (+) = liftA2 (+)
--     (*) = liftA2 (*)
--     (-) = liftA2 (-)
--     abs = fmap abs
--     signum = fmap signum
--     fromInteger = pure . fromInteger
-- 
-- instance Fractional a => Fractional (ArticulationT a) where
--     recip        = fmap recip
--     fromRational = pure . fromRational
-- 
-- instance Floating a => Floating (ArticulationT a) where
--     pi    = pure pi
--     sqrt  = fmap sqrt
--     exp   = fmap exp
--     log   = fmap log
--     sin   = fmap sin
--     cos   = fmap cos
--     asin  = fmap asin
--     atan  = fmap atan
--     acos  = fmap acos
--     sinh  = fmap sinh
--     cosh  = fmap cosh
--     asinh = fmap asinh
--     atanh = fmap atanh
--     acosh = fmap acos
-- 
-- instance Enum a => Enum (ArticulationT a) where
--     toEnum = pure . toEnum
--     fromEnum = fromEnum . get1
-- 
-- instance Bounded a => Bounded (ArticulationT a) where
--     minBound = pure minBound
--     maxBound = pure maxBound
-- 
-- instance (Num a, Ord a, Real a) => Real (ArticulationT a) where
--     toRational = toRational . get1
-- 
-- instance (Real a, Enum a, Integral a) => Integral (ArticulationT a) where
--     quot = liftA2 quot
--     rem = liftA2 rem
--     toInteger = toInteger . get1
-- 

-- TODO replace with (^?!), extract or similar
get1 = head . toList

fmaps :: Functor f => (a -> (b, c)) -> f a -> (f b, f c)
fmaps f x = ((fst . f) <$> x, (snd . f) <$> x)

liftsA2 :: Applicative f => (a -> b -> (c, d)) -> f a -> f b -> (f a, f b)
liftsA2 f x y = (fst <$> ((,) <$> x <*> y), snd <$> ((,) <$> x <*> y))
