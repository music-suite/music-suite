
{-# LANGUAGE
    TypeFamilies,
    NoMonomorphismRestriction,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    StandaloneDeriving,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    MultiParamTypeClasses,
    GeneralizedNewtypeDeriving #-}

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

import Control.Applicative
import Control.Monad
import Control.Comonad
import Data.Semigroup
import Data.Default
import Data.Ratio
import Data.Maybe
import Data.Foldable
import Data.Typeable
import Data.VectorSpace
import Data.AffineSpace
import qualified Data.List as List

import Music.Time
import Music.Pitch.Literal
import Music.Dynamics.Literal
import Music.Score.Rhythm
import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Pitch
import Music.Score.Ties
import Music.Score.Part
import Music.Score.Chord
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Ornaments
import Music.Score.Util

-------------------------------------------------------------------------------------

{-
    Rewrite pairs:

    CANNOT DO
        ChordT
        TieT
    TODO
        PartT
        DynamicT
        ArticulationT
        
    DONE
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
    fromPitch l                                     = TieT (False, fromPitch l, False)
instance IsDynamics a => IsDynamics (TieT a) where
    fromDynamics l                                  = TieT (False, fromDynamics l, False)

instance IsPitch a => IsPitch (DynamicT a) where
    fromPitch l                                     = DynamicT (False,False,Nothing,fromPitch l,False,False)
instance IsDynamics a => IsDynamics (DynamicT a) where
    fromDynamics l                                  = DynamicT (False,False,Nothing,fromDynamics l,False,False)

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
    rev (TieT (b,x,e)) = TieT (e,rev x,b)
instance Reversible a => Reversible (HarmonicT a) where
    rev = fmap rev
instance Reversible a => Reversible (ArticulationT a) where
    rev (ArticulationT (es,us,al,sl,a,bs)) = ArticulationT (bs,us,al,sl,rev a,es)
instance Reversible a => Reversible (TextT a) where
    rev = fmap rev
instance Reversible a => Reversible (TremoloT a) where
    rev = fmap rev
instance Reversible a => Reversible (PartT p a) where
    rev = fmap rev


-------------------------------------------------------------------------------------

instance Semigroup a => Semigroup (DynamicT a) where
    DynamicT (ec,ed,l,a,bc,bd) <> DynamicT (_,_,_,b,_,_) = DynamicT (ec,ed,l,a <> b,bc,bd)
instance Semigroup a => Semigroup (SlideT a) where
    (<>) = liftA2 (<>)
instance Semigroup a => Semigroup (TieT a) where
    TieT (b1,x1,e1) <> TieT (b2,x2,e2) = TieT (b1 && b2, x1 <> x2, e1 && e2)
    -- This instance is suspect: in general chord notes are not required to share ties,
    -- so this instance may be removed (provided that TieT is moved inside ChordT for
    -- all Preludes). See #134
instance Semigroup a => Semigroup (HarmonicT a) where
    (<>) = liftA2 (<>)
instance Semigroup a => Semigroup (TextT a) where
    (<>) = liftA2 (<>)
instance Semigroup a => Semigroup (TremoloT a) where
    TremoloT (n1,x1) <> TremoloT (n2,x2) = TremoloT (n1 `max` n2, x1 <> x2)
instance Semigroup a => Semigroup (PartT n a) where
    PartT (v1,x1) <> PartT (v2,x2) = PartT (v1, x1 <> x2)


-------------------------------------------------------------------------------------

-- Maybe

-- TODO this instance may be problematic with mapPhrase
instance HasArticulation a => HasArticulation (Maybe a) where
    setEndSlur    n (Just x)                        = Just (setEndSlur n x)
    setEndSlur    n Nothing                         = Nothing
    setContSlur   n (Just x)                        = Just (setContSlur n x)
    setContSlur   n Nothing                         = Nothing
    setBeginSlur  n (Just x)                        = Just (setBeginSlur n x)
    setBeginSlur  n Nothing                         = Nothing
    setAccLevel   n (Just x)                        = Just (setAccLevel n x)
    setAccLevel   n Nothing                         = Nothing
    setStaccLevel n (Just x)                        = Just (setStaccLevel n x)
    setStaccLevel n Nothing                         = Nothing
type instance Part (Maybe a)                             = Part a
instance HasPart a => HasPart (Maybe a) where
    getPart Nothing                                 = def
    getPart (Just a)                                = getPart a
    modifyPart f Nothing                            = Nothing
    modifyPart f (Just a)                           = Just (modifyPart f a) 
    -- TODO use cofunctor
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
-- No HasPart instance, PartT must be outside ChordT
-- This restriction assures all chord notes are in the same part

type instance Pitch (ChordT a) = Pitch a
-- instance HasGetPitch a => HasGetPitch (ChordT a) where
    -- __getPitch = __getPitch . get1
instance HasSetPitch a b => HasSetPitch (ChordT a) (ChordT b) where
    type SetPitch g (ChordT a) = ChordT (SetPitch g a)
    __mapPitch f = fmap (__mapPitch f)

instance HasDynamic a => HasDynamic (ChordT a) where
    setBeginCresc n (ChordT as)                     = ChordT (fmap (setBeginCresc n) as)
    setEndCresc   n (ChordT as)                     = ChordT (fmap (setEndCresc n) as)
    setBeginDim   n (ChordT as)                     = ChordT (fmap (setBeginDim n) as)
    setEndDim     n (ChordT as)                     = ChordT (fmap (setEndDim n) as)
    setLevel      n (ChordT as)                     = ChordT (fmap (setLevel n) as)
instance HasArticulation a => HasArticulation (ChordT a) where
    setEndSlur    n (ChordT as)                     = ChordT (fmap (setEndSlur n) as)
    setContSlur   n (ChordT as)                     = ChordT (fmap (setContSlur n) as)
    setBeginSlur  n (ChordT as)                     = ChordT (fmap (setBeginSlur n) as)
    setAccLevel   n (ChordT as)                     = ChordT (fmap (setAccLevel n) as)
    setStaccLevel n (ChordT as)                     = ChordT (fmap (setStaccLevel n) as)
instance HasTremolo a => HasTremolo (ChordT a) where
    setTrem      n (ChordT as)                      = ChordT (fmap (setTrem n) as)
instance HasHarmonic a => HasHarmonic (ChordT a) where
    setNatural    n (ChordT as)                     = ChordT (fmap (setNatural n) as)
    setHarmonic   n (ChordT as)                     = ChordT (fmap (setHarmonic n) as)
instance HasSlide a => HasSlide (ChordT a) where
    setBeginGliss n (ChordT as)                     = ChordT (fmap (setBeginGliss n) as)
    setBeginSlide n (ChordT as)                     = ChordT (fmap (setBeginSlide n) as)
    setEndGliss   n (ChordT as)                     = ChordT (fmap (setEndGliss n) as)
    setEndSlide   n (ChordT as)                     = ChordT (fmap (setEndSlide n) as)
instance HasText a => HasText (ChordT a) where
    addText      s (ChordT as)                      = ChordT (mapF (addText s) as)


-- TieT

type instance Part (TieT a)                              = Part a
instance HasPart a => HasPart (TieT a) where
    getPart (TieT (_,x,_))                          = getPart x
    modifyPart f (TieT (b,x,e))                     = TieT (b,modifyPart f x,e)
instance HasChord a => HasChord (TieT a) where
    type ChordNote (TieT a)                              = TieT (ChordNote a)
    getChord (TieT (b,x,e))                         = fmap (\x -> TieT (b,x,e)) (getChord x)

type instance Pitch (TieT a) = Pitch a
instance HasGetPitch a => HasGetPitch (TieT a) where
    __getPitch = __getPitch . get1
instance HasSetPitch a b => HasSetPitch (TieT a) (TieT b) where
    type SetPitch g (TieT a) = TieT (SetPitch g a)
    __mapPitch f = fmap (__mapPitch f)


instance HasDynamic a => HasDynamic (TieT a) where
    setBeginCresc n                                 = fmap (setBeginCresc n)
    setEndCresc   n                                 = fmap (setEndCresc n)
    setBeginDim   n                                 = fmap (setBeginDim n)
    setEndDim     n                                 = fmap (setEndDim n)
    setLevel      n                                 = fmap (setLevel n)
instance HasArticulation a => HasArticulation (TieT a) where
    setEndSlur    n                                 = fmap (setEndSlur n)
    setContSlur   n                                 = fmap (setContSlur n)
    setBeginSlur  n                                 = fmap (setBeginSlur n)
    setAccLevel   n                                 = fmap (setAccLevel n)
    setStaccLevel n                                 = fmap (setStaccLevel n)
instance HasTremolo a => HasTremolo (TieT a) where
    setTrem       n                                 = fmap (setTrem n)
instance HasHarmonic a => HasHarmonic (TieT a) where
    setNatural    n                                 = fmap (setNatural n)
    setHarmonic   n                                 = fmap (setHarmonic n)
instance HasSlide a => HasSlide (TieT a) where
    setBeginGliss n                                 = fmap (setBeginGliss n)
    setBeginSlide n                                 = fmap (setBeginSlide n)
    setEndGliss   n                                 = fmap (setEndGliss n)
    setEndSlide   n                                 = fmap (setEndSlide n)
instance HasText a => HasText (TieT a) where
    addText       s                                 = fmap (addText s)


-- DynamicT

-- end cresc/dim, level, begin cresc/dim
-- newtype DynamicT a = DynamicT { getDynamicT :: (Bool, Bool, Maybe Double, a, Bool, Bool) }

instance Tiable a => Tiable (DynamicT a) where
    toTied (DynamicT (ec,ed,l,a,bc,bd))             = (DynamicT (ec,ed,l,b,bc,bd),
                                                       DynamicT (False,False,Nothing,c,False,False)) where (b,c) = toTied a
type instance Part (DynamicT a)                          = Part a
instance HasPart a => HasPart (DynamicT a) where
    getPart (DynamicT (ec,ed,l,a,bc,bd))            = getPart a
    modifyPart f                                    = fmap (modifyPart f)
instance HasChord a => HasChord (DynamicT a) where
    type ChordNote (DynamicT a)                          = DynamicT (ChordNote a)
    getChord (DynamicT (ec,ed,l,a,bc,bd))           = fmap (\x -> DynamicT (ec,ed,l,x,bc,bd)) (getChord a)

type instance Pitch (DynamicT a) = Pitch a
instance HasGetPitch a => HasGetPitch (DynamicT a) where
    __getPitch (DynamicT (ec,ed,l,a,bc,bd)) = __getPitch a
instance HasSetPitch a b => HasSetPitch (DynamicT a) (DynamicT b) where
    type SetPitch g (DynamicT a) = DynamicT (SetPitch g a)
    __mapPitch f (DynamicT (ec,ed,l,a,bc,bd)) = DynamicT (ec,ed,l,__mapPitch f a,bc,bd)


instance HasArticulation a => HasArticulation (DynamicT a) where
    setEndSlur    n                                 = fmap (setEndSlur n)
    setContSlur   n                                 = fmap (setContSlur n)
    setBeginSlur  n                                 = fmap (setBeginSlur n)
    setAccLevel   n                                 = fmap (setAccLevel n)
    setStaccLevel n                                 = fmap (setStaccLevel n)
instance HasTremolo a => HasTremolo (DynamicT a) where
    setTrem       n                                 = fmap (setTrem n)
instance HasHarmonic a => HasHarmonic (DynamicT a) where
    setNatural    n                                 = fmap (setNatural n)
    setHarmonic   n                                 = fmap (setHarmonic n)
instance HasSlide a => HasSlide (DynamicT a) where
    setBeginGliss n                                 = fmap (setBeginGliss n)
    setBeginSlide n                                 = fmap (setBeginSlide n)
    setEndGliss   n                                 = fmap (setEndGliss n)
    setEndSlide   n                                 = fmap (setEndSlide n)
instance HasText a => HasText (DynamicT a) where
    addText       s                                 = fmap (addText s)


-- ArticulationT

-- end slur, cont slur, acc level, stacc level, begin slur
-- newtype ArticulationT a = ArticulationT { getArticulationT :: (Bool, Bool, Int, Int, a, Bool) }


instance Tiable a => Tiable (ArticulationT a) where
    toTied (ArticulationT (es,us,al,sl,a,bs))           = (ArticulationT (False,us || es , al,sl,b,bs),
                                                           ArticulationT (es,   us || bs , 0, 0, c,False)) where (b,c) = toTied a
type instance Part (ArticulationT a)                         = Part a
instance HasPart a => HasPart (ArticulationT a) where
    getPart (ArticulationT (es,us,al,sl,a,bs))          = getPart a
    modifyPart   f                                      = fmap (modifyPart f)
instance HasChord a => HasChord (ArticulationT a) where
    type ChordNote (ArticulationT a)                         = ArticulationT (ChordNote a)
    getChord (ArticulationT (es,us,al,sl,a,bs))         = fmap (\x -> ArticulationT (es,us,al,sl,x,bs)) (getChord a)

type instance Pitch (ArticulationT a) = Pitch a
instance HasGetPitch a => HasGetPitch (ArticulationT a) where
    __getPitch (ArticulationT (es,us,al,sl,a,bs)) = __getPitch a
instance HasSetPitch a b => HasSetPitch (ArticulationT a) (ArticulationT b) where
    type SetPitch g (ArticulationT a) = ArticulationT (SetPitch g a)
    __mapPitch f (ArticulationT (es,us,al,sl,a,bs)) = (ArticulationT (es,us,al,sl,__mapPitch f a,bs))

instance HasDynamic a => HasDynamic (ArticulationT a) where
    setBeginCresc n                                     = fmap (setBeginCresc n)
    setEndCresc   n                                     = fmap (setEndCresc n)
    setBeginDim   n                                     = fmap (setBeginDim n)
    setEndDim     n                                     = fmap (setEndDim n)
    setLevel      n                                     = fmap (setLevel n)
-- instance HasArticulation (ArticulationT a) where
instance HasTremolo a => HasTremolo (ArticulationT a) where
    setTrem       n                                     = fmap (setTrem n)
instance HasHarmonic a => HasHarmonic (ArticulationT a) where
    setNatural    n                                     = fmap (setNatural n)
    setHarmonic   n                                     = fmap (setHarmonic n)
instance HasSlide a => HasSlide (ArticulationT a) where
    setBeginGliss n                                     = fmap (setBeginGliss n)
    setBeginSlide n                                     = fmap (setBeginSlide n)
    setEndGliss   n                                     = fmap (setEndGliss n)
    setEndSlide   n                                     = fmap (setEndSlide n)
instance HasText a => HasText (ArticulationT a) where
    addText       s                                     = fmap (addText s)


-- TremoloT

-- newtype TremoloT a = TremoloT { getTremoloT :: (Int, a) }


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
-- instance HasTremolo (TremoloT a) where
deriving instance HasHarmonic a => HasHarmonic (TremoloT a)
deriving instance HasSlide a => HasSlide (TremoloT a)
deriving instance HasText a => HasText (TremoloT a)


-- TextT

-- newtype TextT a = TextT { getTextT :: (Int, a) }

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
-- instance HasText (TextT a) where


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
-- instance HasHarmonic (HarmonicT a) where
deriving instance HasSlide a => HasSlide (HarmonicT a)
deriving instance HasText a => HasText (HarmonicT a)


-- SlideT


instance Tiable a => Tiable (SlideT a) where
    toTied = fmaps toTied
    -- TODO avoid splitting ties

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

instance HasDynamic a => HasDynamic (SlideT a) where
    setBeginCresc n                                = fmap (setBeginCresc n)
    setEndCresc   n                                = fmap (setEndCresc n)
    setBeginDim   n                                = fmap (setBeginDim n)
    setEndDim     n                                = fmap (setEndDim n)
    setLevel      n                                = fmap (setLevel n)
instance HasArticulation a => HasArticulation (SlideT a) where
    setEndSlur    n                                = fmap (setEndSlur n)
    setContSlur   n                                = fmap (setContSlur n)
    setBeginSlur  n                                = fmap (setBeginSlur n)
    setAccLevel   n                                = fmap (setAccLevel n)
    setStaccLevel n                                = fmap (setStaccLevel n)
instance HasTremolo a => HasTremolo (SlideT a) where
    setTrem       n                                = fmap (setTrem n)
instance HasHarmonic a => HasHarmonic (SlideT a) where
    setNatural    n                                = fmap (setNatural n)
    setHarmonic   n                                = fmap (setHarmonic n)
-- instance HasSlide (SlideT a) where
instance HasText a => HasText (SlideT a) where
    addText       s                                = fmap (addText s)


-------------------------------------------------------------------------------------
-- Num, Integral, Enum and Bounded
-------------------------------------------------------------------------------------

-- TODO are these instances sane?
-- Enum and bounded seems harmless, but what about num/real/integral

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
    TieT (et,a,bt) + TieT (_,b,_) = TieT (et,a+b,bt)
    TieT (et,a,bt) * TieT (_,b,_) = TieT (et,a*b,bt)
    TieT (et,a,bt) - TieT (_,b,_) = TieT (et,a-b,bt)
    abs (TieT (et,a,bt))          = TieT (et,abs a,bt)
    signum (TieT (et,a,bt))       = TieT (et,signum a,bt)
    fromInteger a               = TieT (False,fromInteger a,False)

instance Enum a => Enum (TieT a) where
    toEnum a                = TieT (False,toEnum a,False)
    fromEnum (TieT (_,a,_)) = fromEnum a

instance Bounded a => Bounded (TieT a) where
    minBound = TieT (False,minBound,False)
    maxBound = TieT (False,maxBound,False)

instance (Num a, Ord a, Real a) => Real (TieT a) where
    toRational (TieT (_,a,_)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (TieT a) where
    TieT (et,a,bt) `quotRem` TieT (_,b,_) = (TieT (et,q,bt), TieT (et,r,bt)) where (q,r) = a `quotRem` b
    toInteger (TieT (_,a,_)) = toInteger a


-- DynamicT

instance Num a => Num (DynamicT a) where
    DynamicT (p,q,r,a,s,t) + DynamicT (_,_,_,b,_,_) = DynamicT (p,q,r,a+b,s,t)
    DynamicT (p,q,r,a,s,t) * DynamicT (_,_,_,b,_,_) = DynamicT (p,q,r,a*b,s,t)
    DynamicT (p,q,r,a,s,t) - DynamicT (_,_,_,b,_,_) = DynamicT (p,q,r,a-b,s,t)
    abs (DynamicT (p,q,r,a,s,t))                    = DynamicT (p,q,r,abs a,s,t)
    signum (DynamicT (p,q,r,a,s,t))                 = DynamicT (p,q,r,signum a,s,t)
    fromInteger a                                   = DynamicT (False,False,Nothing,fromInteger a,False,False)

instance Enum a => Enum (DynamicT a) where
    toEnum a                         = DynamicT (False,False,Nothing,toEnum a,False,False)
    fromEnum (DynamicT (_,_,_,a,_,_)) = fromEnum a

instance Bounded a => Bounded (DynamicT a) where
    minBound = DynamicT (False,False,Nothing,minBound,False,False)
    maxBound = DynamicT (False,False,Nothing,maxBound,False,False)

instance (Num a, Ord a, Real a) => Real (DynamicT a) where
    toRational (DynamicT (_,_,_,a,_,_)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (DynamicT a) where
    DynamicT (p,q,r,a,s,t) `quotRem` DynamicT (_,_,_,b,_,_) = (DynamicT (p,q,r,q',s,t), DynamicT (p,q,r,r',s,t)) where (q',r') = a `quotRem` b
    toInteger (DynamicT (_,_,_,a,_,_)) = toInteger a


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
