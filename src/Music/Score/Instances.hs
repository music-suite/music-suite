
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
import           Data.AffineSpace
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
import           Music.Score.Articulation
import           Music.Score.Chord
import           Music.Score.Combinators
import           Music.Score.Dynamics
import           Music.Score.Ornaments
import           Music.Score.Part
import           Music.Score.Pitch
import           Music.Score.Rhythm
import           Music.Score.Score
import           Music.Score.Ties
import           Music.Score.Track
import           Music.Score.Util
import           Music.Score.Voice
import           Music.Time

-------------------------------------------------------------------------------------

{-
    Rewrite pairs:

    CANNOT DO
        ChordT
    TODO
        PartT

    DONE
        DynamicT
        TieT
        ArticulationT
        TremoloT
        HarmonicT
        TextT
        SlideT


-}


instance (IsPitch a, Enum n) => IsPitch (PartT n a) where
    fromPitch l                                     = PartT (toEnum 0, fromPitch l)
instance (IsDynamics a, Enum n) => IsDynamics (PartT n a) where
    fromDynamics l                                  = PartT (toEnum 0, fromDynamics l)

instance IsPitch a => IsPitch (ChordT a) where
    fromPitch = return . fromPitch
instance IsDynamics a => IsDynamics (ChordT a) where
    fromDynamics = return . fromDynamics

instance IsPitch a => IsPitch (TieT a) where
    fromPitch = pure . fromPitch
instance IsDynamics a => IsDynamics (TieT a) where
    fromDynamics = return . fromDynamics

instance IsPitch a => IsPitch (DynamicT a) where
    fromPitch = pure . fromPitch
instance IsDynamics a => IsDynamics (DynamicT a) where
    fromDynamics = return . fromDynamics

instance IsPitch a => IsPitch (TremoloT a) where
    fromPitch = pure . fromPitch
instance IsDynamics a => IsDynamics (TremoloT a) where
    fromDynamics = pure . fromDynamics

instance IsPitch a => IsPitch (TextT a) where
    fromPitch = pure . fromPitch
instance IsDynamics a => IsDynamics (TextT a) where
    fromDynamics = pure . fromDynamics

instance IsPitch a => IsPitch (HarmonicT a) where
    fromPitch = pure . fromPitch
instance IsDynamics a => IsDynamics (HarmonicT a) where
    fromDynamics = pure . fromDynamics

instance IsPitch a => IsPitch (SlideT a) where
    fromPitch = pure . fromPitch
instance IsDynamics a => IsDynamics (SlideT a) where
    fromDynamics = pure . fromDynamics

-------------------------------------------------------------------------------------

instance Reversible (ChordT a) where
    rev = id
instance Reversible a => Reversible (DynamicT a) where
    rev = fmap rev
instance Reversible a => Reversible (SlideT a) where
    rev = fmap rev
instance Reversible a => Reversible (TieT a) where
    rev = fmap rev
instance Reversible a => Reversible (HarmonicT a) where
    rev = fmap rev
instance Reversible a => Reversible (ArticulationT a) where
    rev = fmap rev
instance Reversible a => Reversible (TextT a) where
    rev = fmap rev
instance Reversible a => Reversible (TremoloT a) where
    rev = fmap rev
instance Reversible a => Reversible (PartT p a) where
    rev = fmap rev


-------------------------------------------------------------------------------------

instance Semigroup a => Semigroup (DynamicT a) where
    DynamicT (d1, x1) <> DynamicT (d2, x2) = DynamicT (d1 <> d2, x1 <> x2)
instance Semigroup a => Semigroup (SlideT a) where
    (<>) = liftA2 (<>)
instance Semigroup a => Semigroup (TieT a) where
    TieT (t1, x1) <> TieT (t2, x2) = TieT (t1 <> t2, x1 <> x2)
    -- This instance is suspect: in general chord notes are not required to share ties,
    -- so this instance may be removed (provided that TieT is moved inside ChordT for
    -- all Preludes). See #134
instance Semigroup a => Semigroup (HarmonicT a) where
    (<>) = liftA2 (<>)
instance Semigroup a => Semigroup (TextT a) where
    (<>) = liftA2 (<>)
instance Semigroup a => Semigroup (TremoloT a) where
    (<>) = liftA2 (<>)
instance Semigroup a => Semigroup (PartT n a) where
    PartT (v1,x1) <> PartT (v2,x2) = PartT (v1, x1 <> x2)


-------------------------------------------------------------------------------------

-- Maybe

-- TODO this instance may be problematic with mapPhrase
instance HasArticulation a => HasArticulation (Maybe a) where
    setEndSlur    n = fmap (setEndSlur n)
    setContSlur   n = fmap (setContSlur n)
    setBeginSlur  n = fmap (setBeginSlur n)
    setAccLevel   n = fmap (setAccLevel n)
    setStaccLevel n = fmap (setStaccLevel n)
type instance Part (Maybe a)                             = Part a
instance HasPart a => HasPart (Maybe a) where
    getPart Nothing                                 = error "Nothing: no part"
    getPart (Just a)                                = getPart a
    modifyPart f = fmap (modifyPart f)
type instance Pitch (Maybe a) = Pitch a
instance HasSetPitch a b => HasSetPitch (Maybe a) (Maybe b) where
    type SetPitch g (Maybe a) = Maybe (SetPitch g a)
    __mapPitch f (Nothing)                          = Nothing
    __mapPitch f (Just a)                           = Just (__mapPitch f a)


-- PartT

instance HasChord a => HasChord (PartT n a) where
    type ChordNote (PartT n a)                           = PartT n (ChordNote a)
    getChord (PartT (v,x))                          = fmap (\x -> PartT (v,x)) (getChord x)

type instance Pitch (PartT n a) = Pitch a
instance HasGetPitch a => HasGetPitch (PartT n a) where
    __getPitch = __getPitch . extract
instance HasSetPitch a b => HasSetPitch (PartT n a) (PartT n b) where
    type SetPitch g (PartT n a) = PartT n (SetPitch g a)
    __mapPitch f = fmap (__mapPitch f)

instance Tiable a => Tiable (PartT n a) where
    toTied (PartT (v,a)) = (PartT (v,b), PartT (v,c)) where (b,c) = toTied a
deriving instance HasDynamic a => HasDynamic (PartT n a)
deriving instance HasArticulation a => HasArticulation (PartT n a)
deriving instance HasTremolo a => HasTremolo (PartT n a)
deriving instance HasHarmonic a => HasHarmonic (PartT n a)
deriving instance HasSlide a => HasSlide (PartT n a)
deriving instance HasText a => HasText (PartT n a)


-- ChordT

instance Tiable a => Tiable (ChordT a) where
    toTied (ChordT as)                              = (ChordT bs, ChordT cs) where (bs,cs) = (unzip . fmap toTied) as
-- There is no (HasPart ChordT) instance, so PartT must be outside ChordT in the stack
-- This restriction assures all chord notes are in the same part

type instance Pitch (ChordT a) = Pitch a

instance HasSetPitch a b => HasSetPitch (ChordT a) (ChordT b) where
    type SetPitch g (ChordT a) = ChordT (SetPitch g a)
    __mapPitch f = fmap (__mapPitch f)

instance HasDynamic a => HasDynamic (ChordT a) where
    setEndDim     n = fmap (setEndDim n)
    setLevel      n = fmap (setLevel n)
instance HasArticulation a => HasArticulation (ChordT a) where
    setEndSlur    n = fmap (setEndSlur n)
    setContSlur   n = fmap (setContSlur n)
    setBeginSlur  n = fmap (setBeginSlur n)
    setAccLevel   n = fmap (setAccLevel n)
    setStaccLevel n = fmap (setStaccLevel n)
instance HasTremolo a => HasTremolo (ChordT a) where
    setTrem       n = fmap (setTrem n)
instance HasHarmonic a => HasHarmonic (ChordT a) where
    setNatural    n = fmap (setNatural n)
    setHarmonic   n = fmap (setHarmonic n)
instance HasSlide a => HasSlide (ChordT a) where
    setBeginGliss n = fmap (setBeginGliss n)
    setBeginSlide n = fmap (setBeginSlide n)
    setEndGliss   n = fmap (setEndGliss n)
    setEndSlide   n = fmap (setEndSlide n)
instance HasText a => HasText (ChordT a) where
    addText       s (ChordT as) = ChordT (mapF (addText s) as)


-- TieT

type instance Part (TieT a) = Part a
instance HasPart a => HasPart (TieT a) where
    getPart (TieT (_,x)) = getPart x
    modifyPart f = fmap (modifyPart f)
instance HasChord a => HasChord (TieT a) where
    type ChordNote (TieT a) = TieT (ChordNote a)
    getChord (TieT (t,x))   = fmap (\x -> TieT (t,x)) (getChord x)

type instance Pitch (TieT a) = Pitch a
instance HasGetPitch a => HasGetPitch (TieT a) where
    __getPitch = __getPitch . get1
instance HasSetPitch a b => HasSetPitch (TieT a) (TieT b) where
    type SetPitch g (TieT a) = TieT (SetPitch g a)
    __mapPitch f = fmap (__mapPitch f)


deriving instance HasDynamic a => HasDynamic (TieT a)
deriving instance HasArticulation a => HasArticulation (TieT a)
deriving instance HasTremolo a => HasTremolo (TieT a)
deriving instance HasHarmonic a => HasHarmonic (TieT a)
deriving instance HasSlide a => HasSlide (TieT a)
deriving instance HasText a => HasText (TieT a)


-- DynamicT

-- end cresc/dim, level, begin cresc/dim
-- newtype DynamicT a = DynamicT { getDynamicT :: (Bool, Bool, Maybe Double, a, Bool, Bool) }

instance Tiable a => Tiable (DynamicT a) where
    toTied (DynamicT (l, a)) = (DynamicT (l, b), DynamicT (mempty, c)) where (b,c) = toTied a

type instance Part (DynamicT a) = Part a
instance HasPart a => HasPart (DynamicT a) where
    getPart (DynamicT (_,x)) = getPart x
    modifyPart f = fmap (modifyPart f)
instance HasChord a => HasChord (DynamicT a) where
    type ChordNote (DynamicT a) = DynamicT (ChordNote a)
    getChord (DynamicT (d,as)) = fmap (\x -> DynamicT (d,x)) (getChord as)

type instance Pitch (DynamicT a) = Pitch a
instance HasGetPitch a => HasGetPitch (DynamicT a) where
    __getPitch (DynamicT (_,x)) = __getPitch x
instance HasSetPitch a b => HasSetPitch (DynamicT a) (DynamicT b) where
    type SetPitch g (DynamicT a) = DynamicT (SetPitch g a)
    __mapPitch f = fmap (__mapPitch f)


deriving instance HasArticulation a => HasArticulation (DynamicT a)
deriving instance HasTremolo a => HasTremolo (DynamicT a)
deriving instance HasHarmonic a => HasHarmonic (DynamicT a)
deriving instance HasSlide a => HasSlide (DynamicT a)
deriving instance HasText a => HasText (DynamicT a)


-- ArticulationT

-- end slur, cont slur, acc level, stacc level, begin slur
-- newtype ArticulationT a = ArticulationT { getArticulationT :: (Bool, Bool, Int, Int, a, Bool) }


instance Tiable a => Tiable (ArticulationT a) where
    toTied (ArticulationT (v,a)) = (ArticulationT (v,b), ArticulationT (v,c)) where (b,c) = toTied a

type instance Part (ArticulationT a)                         = Part a
instance HasPart a => HasPart (ArticulationT a) where
    getPart = getPart . get1
    modifyPart   f                                      = fmap (modifyPart f)
instance HasChord a => HasChord (ArticulationT a) where
    type ChordNote (ArticulationT a)                         = ArticulationT (ChordNote a)
    getChord (ArticulationT (v,x))                          = fmap (\x -> ArticulationT (v,x)) (getChord x)

type instance Pitch (ArticulationT a) = Pitch a
instance HasGetPitch a => HasGetPitch (ArticulationT a) where
    __getPitch (ArticulationT (_,a)) = __getPitch a
instance HasSetPitch a b => HasSetPitch (ArticulationT a) (ArticulationT b) where
    type SetPitch g (ArticulationT a) = ArticulationT (SetPitch g a)
    __mapPitch f (ArticulationT (v,x)) = (ArticulationT (v,__mapPitch f x))

deriving instance HasDynamic a => HasDynamic (ArticulationT a)
deriving instance HasTremolo a => HasTremolo (ArticulationT a)
deriving instance HasHarmonic a => HasHarmonic (ArticulationT a)
deriving instance HasSlide a => HasSlide (ArticulationT a)
deriving instance HasText a => HasText (ArticulationT a)

-- TremoloT

instance Tiable a => Tiable (TremoloT a) where
    toTied (TremoloT (n,a))                         = (TremoloT (n,b), TremoloT (n,c)) where (b,c) = toTied a
type instance Part (TremoloT a)                          = Part a
instance HasPart a => HasPart (TremoloT a) where
    getPart (TremoloT (_,a))                        = getPart a
    modifyPart f (TremoloT (n,x))                   = TremoloT (n, modifyPart f x)
instance HasChord a => HasChord (TremoloT a) where
    type ChordNote (TremoloT a)                          = TremoloT (ChordNote a)
    getChord (TremoloT (n,x))                       = fmap (\x -> TremoloT (n,x)) (getChord x)

type instance Pitch (TremoloT a) = Pitch a
instance HasGetPitch a => HasGetPitch (TremoloT a) where
    __getPitch = __getPitch . get1
instance HasSetPitch a b => HasSetPitch (TremoloT a) (TremoloT b) where
    type SetPitch g (TremoloT a) = TremoloT (SetPitch g a)
    __mapPitch f = fmap (__mapPitch f)

deriving instance HasDynamic a => HasDynamic (TremoloT a)
deriving instance HasArticulation a => HasArticulation (TremoloT a)
deriving instance HasHarmonic a => HasHarmonic (TremoloT a)
deriving instance HasSlide a => HasSlide (TremoloT a)
deriving instance HasText a => HasText (TremoloT a)


-- TextT

instance Tiable a => Tiable (TextT a) where
    toTied (TextT (n,a))                            = (TextT (n,b), TextT (mempty,c)) where (b,c) = toTied a
type instance Part (TextT a)                             = Part a
instance HasPart a => HasPart (TextT a) where
    getPart (TextT (_,a))                           = getPart a
    modifyPart f = fmap (modifyPart f)
instance HasChord a => HasChord (TextT a) where
    type ChordNote (TextT a)                             = TextT (ChordNote a)
    getChord (TextT (n,x))                          = fmap (\x -> TextT (n,x)) (getChord x)

type instance Pitch (TextT a) = Pitch a
instance HasGetPitch a => HasGetPitch (TextT a) where
    __getPitch = __getPitch . get1
instance HasSetPitch a b => HasSetPitch (TextT a) (TextT b) where
    type SetPitch g (TextT a) = TextT (SetPitch g a)
    __mapPitch f = fmap (__mapPitch f)

deriving instance HasDynamic a => HasDynamic (TextT a)
deriving instance HasArticulation a => HasArticulation (TextT a)
deriving instance HasTremolo a => HasTremolo (TextT a)
deriving instance HasHarmonic a => HasHarmonic (TextT a)
deriving instance HasSlide a => HasSlide (TextT a)


-- HarmonicT

instance Tiable a => Tiable (HarmonicT a) where
    toTied (HarmonicT (n,a))                        = (HarmonicT (n,b), HarmonicT (n,c)) where (b,c) = toTied a
type instance Part (HarmonicT a)                         = Part a
instance HasPart a => HasPart (HarmonicT a) where
    getPart (HarmonicT (_,a))                       = getPart a
    modifyPart f (HarmonicT (n,x))                  = HarmonicT (n, modifyPart f x)
instance HasChord a => HasChord (HarmonicT a) where
    type ChordNote (HarmonicT a)                         = HarmonicT (ChordNote a)
    getChord (HarmonicT (n,x))                      = fmap (\x -> HarmonicT (n,x)) (getChord x)

type instance Pitch (HarmonicT a) = Pitch a
instance HasGetPitch a => HasGetPitch (HarmonicT a) where
    __getPitch = __getPitch . get1
instance HasSetPitch a b => HasSetPitch (HarmonicT a) (HarmonicT b) where
    type SetPitch g (HarmonicT a) = HarmonicT (SetPitch g a)
    __mapPitch f = fmap (__mapPitch f)

deriving instance HasDynamic a => HasDynamic (HarmonicT a)
deriving instance HasArticulation a => HasArticulation (HarmonicT a)
deriving instance HasTremolo a => HasTremolo (HarmonicT a)
deriving instance HasSlide a => HasSlide (HarmonicT a)
deriving instance HasText a => HasText (HarmonicT a)


-- SlideT


instance Tiable a => Tiable (SlideT a) where
    toTied (SlideT (v,x)) = (SlideT (v,a), SlideT (v,b)) where (a,b) = toTied x

type instance Part (SlideT a) = Part a
instance HasPart a => HasPart (SlideT a) where
    getPart = getPart . get1
    modifyPart f = fmap (modifyPart f)
instance HasChord a => HasChord (SlideT a) where
    type ChordNote (SlideT a) = SlideT (ChordNote a)
    getChord (SlideT (x,as)) = fmap (\a -> SlideT (x,a)) (getChord as)

type instance Pitch (SlideT a) = Pitch a
instance HasGetPitch a => HasGetPitch (SlideT a) where
    __getPitch = __getPitch . get1
instance HasSetPitch a b => HasSetPitch (SlideT a) (SlideT b) where
    type SetPitch g (SlideT a) = SlideT (SetPitch g a)
    __mapPitch f = fmap (__mapPitch f)

deriving instance HasDynamic a => HasDynamic (SlideT a)
deriving instance HasArticulation a => HasArticulation (SlideT a)
deriving instance HasTremolo a => HasTremolo (SlideT a)
deriving instance HasHarmonic a => HasHarmonic (SlideT a)
deriving instance HasText a => HasText (SlideT a)



-------------------------------------------------------------------------------------
-- Literal instances
-------------------------------------------------------------------------------------

instance Alterable a => Alterable (Score a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (ChordT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (DynamicT a) where
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

instance Alterable a => Alterable (ArticulationT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

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

instance Augmentable a => Augmentable (ChordT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (DynamicT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (SlideT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (TieT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (HarmonicT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (ArticulationT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (TextT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (TremoloT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (PartT n a) where
    augment = fmap augment
    diminish = fmap diminish



-------------------------------------------------------------------------------------
-- Num, Integral, Enum and Bounded
-------------------------------------------------------------------------------------

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


-- ChordT

-- instance Num a => Num (ChordT a) where
--     ChordT [a] + ChordT [b]   = ChordT [a+b]
--     ChordT [a] * ChordT [b]   = ChordT [a*b]
--     ChordT [a] - ChordT [b]   = ChordT [a-b]
--     abs (ChordT [a])          = ChordT [abs a]
--     signum (ChordT [a])       = ChordT [signum a]
--     fromInteger a             = ChordT [fromInteger a]

instance Enum a => Enum (ChordT a) where
    toEnum a                  = ChordT [toEnum a]
    fromEnum (ChordT [a])     = fromEnum a

instance Bounded a => Bounded (ChordT a) where
    minBound = ChordT [minBound]
    maxBound = ChordT [maxBound]

-- instance (Num a, Ord a, Real a) => Real (ChordT a) where
--     toRational (ChordT [a]) = toRational a

-- instance (Real a, Enum a, Integral a) => Integral (ChordT a) where
--     ChordT [a] `quotRem` ChordT [b] = (ChordT [q], ChordT [r]) where (q,r) = a `quotRem` b
--     toInteger (ChordT [a]) = toInteger a


-- TieT

instance Num a => Num (TieT a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (TieT a) where
    recip        = fmap recip
    fromRational = pure . fromRational

instance Floating a => Floating (TieT a) where
    pi    = pure pi
    sqrt  = fmap sqrt
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acos

instance Enum a => Enum (TieT a) where
    toEnum = pure . toEnum
    fromEnum = fromEnum . get1

instance Bounded a => Bounded (TieT a) where
    minBound = pure minBound
    maxBound = pure maxBound

instance (Num a, Ord a, Real a) => Real (TieT a) where
    toRational = toRational . get1

instance (Real a, Enum a, Integral a) => Integral (TieT a) where
    quot = liftA2 quot
    rem = liftA2 rem
    toInteger = toInteger . get1


-- DynamicT

instance Num a => Num (DynamicT a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (DynamicT a) where
    recip        = fmap recip
    fromRational = pure . fromRational

instance Floating a => Floating (DynamicT a) where
    pi    = pure pi
    sqrt  = fmap sqrt
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acos

instance Enum a => Enum (DynamicT a) where
    toEnum = pure . toEnum
    fromEnum = fromEnum . get1

instance Bounded a => Bounded (DynamicT a) where
    minBound = pure minBound
    maxBound = pure maxBound

instance (Num a, Ord a, Real a) => Real (DynamicT a) where
    toRational = toRational . get1

instance (Real a, Enum a, Integral a) => Integral (DynamicT a) where
    quot = liftA2 quot
    rem = liftA2 rem
    toInteger = toInteger . get1


-- ArticulationT

instance Num a => Num (ArticulationT a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (ArticulationT a) where
    recip        = fmap recip
    fromRational = pure . fromRational

instance Floating a => Floating (ArticulationT a) where
    pi    = pure pi
    sqrt  = fmap sqrt
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acos

instance Enum a => Enum (ArticulationT a) where
    toEnum = pure . toEnum
    fromEnum = fromEnum . get1

instance Bounded a => Bounded (ArticulationT a) where
    minBound = pure minBound
    maxBound = pure maxBound

instance (Num a, Ord a, Real a) => Real (ArticulationT a) where
    toRational = toRational . get1

instance (Real a, Enum a, Integral a) => Integral (ArticulationT a) where
    quot = liftA2 quot
    rem = liftA2 rem
    toInteger = toInteger . get1

-- TremoloT

instance Num a => Num (TremoloT a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (TremoloT a) where
    recip        = fmap recip
    fromRational = pure . fromRational

instance Floating a => Floating (TremoloT a) where
    pi    = pure pi
    sqrt  = fmap sqrt
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acos

instance Enum a => Enum (TremoloT a) where
    toEnum = pure . toEnum
    fromEnum = fromEnum . get1

instance Bounded a => Bounded (TremoloT a) where
    minBound = pure minBound
    maxBound = pure maxBound

instance (Num a, Ord a, Real a) => Real (TremoloT a) where
    toRational = toRational . get1

instance (Real a, Enum a, Integral a) => Integral (TremoloT a) where
    quot = liftA2 quot
    rem = liftA2 rem
    toInteger = toInteger . get1

-- TextT

instance Num a => Num (TextT a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (TextT a) where
    recip        = fmap recip
    fromRational = pure . fromRational

instance Floating a => Floating (TextT a) where
    pi    = pure pi
    sqrt  = fmap sqrt
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acos

instance Enum a => Enum (TextT a) where
    toEnum = pure . toEnum
    fromEnum = fromEnum . get1

instance Bounded a => Bounded (TextT a) where
    minBound = pure minBound
    maxBound = pure maxBound

instance (Num a, Ord a, Real a) => Real (TextT a) where
    toRational = toRational . get1

instance (Real a, Enum a, Integral a) => Integral (TextT a) where
    quot = liftA2 quot
    rem = liftA2 rem
    toInteger = toInteger . get1


-- HarmonicT

instance Num a => Num (HarmonicT a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (HarmonicT a) where
    recip        = fmap recip
    fromRational = pure . fromRational

instance Floating a => Floating (HarmonicT a) where
    pi    = pure pi
    sqrt  = fmap sqrt
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acos

instance Enum a => Enum (HarmonicT a) where
    toEnum = pure . toEnum
    fromEnum = fromEnum . get1

instance Bounded a => Bounded (HarmonicT a) where
    minBound = pure minBound
    maxBound = pure maxBound

instance (Num a, Ord a, Real a) => Real (HarmonicT a) where
    toRational = toRational . get1

instance (Real a, Enum a, Integral a) => Integral (HarmonicT a) where
    quot = liftA2 quot
    rem = liftA2 rem
    toInteger = toInteger . get1

-- SlideT

instance Num a => Num (SlideT a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (SlideT a) where
    recip        = fmap recip
    fromRational = pure . fromRational

instance Floating a => Floating (SlideT a) where
    pi    = pure pi
    sqrt  = fmap sqrt
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acos

instance Enum a => Enum (SlideT a) where
    toEnum = pure . toEnum
    fromEnum = fromEnum . get1

instance Bounded a => Bounded (SlideT a) where
    minBound = pure minBound
    maxBound = pure maxBound

instance (Num a, Ord a, Real a) => Real (SlideT a) where
    toRational = toRational . get1

instance (Real a, Enum a, Integral a) => Integral (SlideT a) where
    quot = liftA2 quot
    rem = liftA2 rem
    toInteger = toInteger . get1


type instance Pitch (Behavior a) = Behavior (Pitch a)

-- TODO undecidable
instance (HasGetPitch a, HasSetPitch a b) => HasSetPitch (Behavior a) (Behavior b) where
    type SetPitch (Behavior p) (Behavior a) = Behavior (SetPitch p a)
    __mapPitch f a = liftA2 (__setPitch) (f $ (__getPitch) <$> a) a

instance Tiable a => Tiable (Behavior a) where toTied x = (x,x)



-- Safe for tuple-like types
get1 = head . toList
-- TODO replace with extract

fmaps :: Functor f => (a -> (b, c)) -> f a -> (f b, f c)
fmaps f x = ((fst . f) <$> x, (snd . f) <$> x)

liftsA2 :: Applicative f => (a -> b -> (c, d)) -> f a -> f b -> (f a, f b)
liftsA2 f x y = (fst <$> ((,) <$> x <*> y), snd <$> ((,) <$> x <*> y))
