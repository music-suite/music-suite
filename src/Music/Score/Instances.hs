
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

module Music.Score.Instances (
  ) where

import Control.Monad
import Control.Comonad
import Data.Semigroup
import Data.Pointed
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
    fromPitch l                                     = TremoloT (0, fromPitch l)
instance IsDynamics a => IsDynamics (TremoloT a) where
    fromDynamics l                                  = TremoloT (0, fromDynamics l)

instance IsPitch a => IsPitch (TextT a) where
    fromPitch l                                     = TextT (mempty, fromPitch l)
instance IsDynamics a => IsDynamics (TextT a) where
    fromDynamics l                                  = TextT (mempty, fromDynamics l)

instance IsPitch a => IsPitch (HarmonicT a) where
    fromPitch = point . fromPitch
instance IsDynamics a => IsDynamics (HarmonicT a) where
    fromDynamics = point . fromDynamics

instance IsPitch a => IsPitch (SlideT a) where
    fromPitch l                                     = SlideT (False,False,fromPitch l,False,False)
instance IsDynamics a => IsDynamics (SlideT a) where
    fromDynamics l                                  = SlideT (False,False,fromDynamics l,False,False)

-------------------------------------------------------------------------------------

instance Reversible (ChordT a) where
    rev = id
instance Reversible a => Reversible (DynamicT a) where
    rev = fmap rev
instance Reversible a => Reversible (SlideT a) where
    rev (SlideT (eg,es,a,bg,bs)) = SlideT (bg,bs,rev a,eg,es)
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
    SlideT (eg,es,a,bg,bs) <> SlideT (_,_,b,_,_) = SlideT (eg,es,a <> b,bg,bs)
instance Semigroup a => Semigroup (TieT a) where
    TieT (b1,x1,e1) <> TieT (b2,x2,e2) = TieT (b1 && b2, x1 <> x2, e1 && e2)
    -- This instance is suspect: in general chord notes are not required to share ties,
    -- so this instance may be removed (provided that TieT is moved inside ChordT for
    -- all Preludes). See #134
instance Semigroup a => Semigroup (HarmonicT a) where
    HarmonicT (n1,x1) <> HarmonicT (n2,x2) = HarmonicT (n1, x1 <> x2)
instance Semigroup a => Semigroup (TextT a) where
    TextT (t1,x1) <> TextT (t2,x2) = TextT (t1 <> t2, x1 <> x2)
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
instance HasPart a => HasPart (Maybe a) where
    type Part (Maybe a)                             = Part a
    getPart Nothing                                 = def
    getPart (Just a)                                = getPart a
    modifyPart f Nothing                            = Nothing
    modifyPart f (Just a)                           = Just (modifyPart f a) 
    -- TODO use cofunctor
type instance Pitch (Maybe a) = Pitch a
instance HasSetPitch a b => HasSetPitch (Maybe a) (Maybe b) where
    type SetPitch g (Maybe a) = Maybe (SetPitch g a)
    mapPitch f (Nothing)                          = Nothing
    mapPitch f (Just a)                           = Just (mapPitch f a)


-- PartT


instance HasChord a => HasChord (PartT n a) where
    type ChordNote (PartT n a)                           = PartT n (ChordNote a)
    getChord (PartT (v,x))                          = fmap (\x -> PartT (v,x)) (getChord x)

type instance Pitch (PartT n a) = Pitch a
instance HasGetPitch a => HasGetPitch (PartT n a) where
    getPitch = getPitch . extract
instance HasSetPitch a b => HasSetPitch (PartT n a) (PartT n b) where
    type SetPitch g (PartT n a) = PartT n (SetPitch g a)
    mapPitch f = fmap (mapPitch f)

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
instance HasGetPitch a => HasGetPitch (ChordT a) where
    getPitch = getPitch . get1
instance HasSetPitch a b => HasSetPitch (ChordT a) (ChordT b) where
    type SetPitch g (ChordT a) = ChordT (SetPitch g a)
    mapPitch f = fmap (mapPitch f)

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

instance HasPart a => HasPart (TieT a) where
    type Part (TieT a)                              = Part a
    getPart (TieT (_,x,_))                          = getPart x
    modifyPart f (TieT (b,x,e))                     = TieT (b,modifyPart f x,e)
instance HasChord a => HasChord (TieT a) where
    type ChordNote (TieT a)                              = TieT (ChordNote a)
    getChord (TieT (b,x,e))                         = fmap (\x -> TieT (b,x,e)) (getChord x)

type instance Pitch (TieT a) = Pitch a
instance HasGetPitch a => HasGetPitch (TieT a) where
    getPitch = getPitch . get1
instance HasSetPitch a b => HasSetPitch (TieT a) (TieT b) where
    type SetPitch g (TieT a) = TieT (SetPitch g a)
    mapPitch f = fmap (mapPitch f)


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
instance HasPart a => HasPart (DynamicT a) where
    type Part (DynamicT a)                          = Part a
    getPart (DynamicT (ec,ed,l,a,bc,bd))            = getPart a
    modifyPart f                                    = fmap (modifyPart f)
instance HasChord a => HasChord (DynamicT a) where
    type ChordNote (DynamicT a)                          = DynamicT (ChordNote a)
    getChord (DynamicT (ec,ed,l,a,bc,bd))           = fmap (\x -> DynamicT (ec,ed,l,x,bc,bd)) (getChord a)

type instance Pitch (DynamicT a) = Pitch a
instance HasGetPitch a => HasGetPitch (DynamicT a) where
    getPitch (DynamicT (ec,ed,l,a,bc,bd)) = getPitch a
instance HasSetPitch a b => HasSetPitch (DynamicT a) (DynamicT b) where
    type SetPitch g (DynamicT a) = DynamicT (SetPitch g a)
    mapPitch f (DynamicT (ec,ed,l,a,bc,bd)) = DynamicT (ec,ed,l,mapPitch f a,bc,bd)


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
instance HasPart a => HasPart (ArticulationT a) where
    type Part (ArticulationT a)                         = Part a
    getPart (ArticulationT (es,us,al,sl,a,bs))          = getPart a
    modifyPart   f                                      = fmap (modifyPart f)
instance HasChord a => HasChord (ArticulationT a) where
    type ChordNote (ArticulationT a)                         = ArticulationT (ChordNote a)
    getChord (ArticulationT (es,us,al,sl,a,bs))         = fmap (\x -> ArticulationT (es,us,al,sl,x,bs)) (getChord a)

type instance Pitch (ArticulationT a) = Pitch a
instance HasGetPitch a => HasGetPitch (ArticulationT a) where
    getPitch (ArticulationT (es,us,al,sl,a,bs)) = getPitch a
instance HasSetPitch a b => HasSetPitch (ArticulationT a) (ArticulationT b) where
    type SetPitch g (ArticulationT a) = ArticulationT (SetPitch g a)
    mapPitch f (ArticulationT (es,us,al,sl,a,bs)) = (ArticulationT (es,us,al,sl,mapPitch f a,bs))

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
instance HasPart a => HasPart (TremoloT a) where
    type Part (TremoloT a)                          = Part a
    getPart (TremoloT (_,a))                        = getPart a
    modifyPart f (TremoloT (n,x))                   = TremoloT (n, modifyPart f x)
instance HasChord a => HasChord (TremoloT a) where
    type ChordNote (TremoloT a)                          = TremoloT (ChordNote a)
    getChord (TremoloT (n,x))                       = fmap (\x -> TremoloT (n,x)) (getChord x)

type instance Pitch (TremoloT a) = Pitch a
instance HasGetPitch a => HasGetPitch (TremoloT a) where
    getPitch = getPitch . get1
instance HasSetPitch a b => HasSetPitch (TremoloT a) (TremoloT b) where
    type SetPitch g (TremoloT a) = TremoloT (SetPitch g a)
    mapPitch f = fmap (mapPitch f)

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
instance HasPart a => HasPart (TextT a) where
    type Part (TextT a)                             = Part a
    getPart (TextT (_,a))                           = getPart a
    modifyPart f (TextT (n,x))                      = TextT (n, modifyPart f x)
instance HasChord a => HasChord (TextT a) where
    type ChordNote (TextT a)                             = TextT (ChordNote a)
    getChord (TextT (n,x))                          = fmap (\x -> TextT (n,x)) (getChord x)

type instance Pitch (TextT a) = Pitch a
instance HasGetPitch a => HasGetPitch (TextT a) where
    getPitch = getPitch . get1
instance HasSetPitch a b => HasSetPitch (TextT a) (TextT b) where
    type SetPitch g (TextT a) = TextT (SetPitch g a)
    mapPitch f = fmap (mapPitch f)


deriving instance HasDynamic a => HasDynamic (TextT a)
deriving instance HasArticulation a => HasArticulation (TextT a)
deriving instance HasTremolo a => HasTremolo (TextT a)
deriving instance HasHarmonic a => HasHarmonic (TextT a)
deriving instance HasSlide a => HasSlide (TextT a)
-- instance HasText (TextT a) where


-- HarmonicT

instance Tiable a => Tiable (HarmonicT a) where
    toTied (HarmonicT (n,a))                        = (HarmonicT (n,b), HarmonicT (n,c)) where (b,c) = toTied a
instance HasPart a => HasPart (HarmonicT a) where
    type Part (HarmonicT a)                         = Part a
    getPart (HarmonicT (_,a))                       = getPart a
    modifyPart f (HarmonicT (n,x))                  = HarmonicT (n, modifyPart f x)
instance HasChord a => HasChord (HarmonicT a) where
    type ChordNote (HarmonicT a)                         = HarmonicT (ChordNote a)
    getChord (HarmonicT (n,x))                      = fmap (\x -> HarmonicT (n,x)) (getChord x)

type instance Pitch (HarmonicT a) = Pitch a
instance HasGetPitch a => HasGetPitch (HarmonicT a) where
    getPitch = getPitch . get1
instance HasSetPitch a b => HasSetPitch (HarmonicT a) (HarmonicT b) where
    type SetPitch g (HarmonicT a) = HarmonicT (SetPitch g a)
    mapPitch f = fmap (mapPitch f)

deriving instance HasDynamic a => HasDynamic (HarmonicT a)
deriving instance HasArticulation a => HasArticulation (HarmonicT a)
deriving instance HasTremolo a => HasTremolo (HarmonicT a)
-- instance HasHarmonic (HarmonicT a) where
deriving instance HasSlide a => HasSlide (HarmonicT a)
deriving instance HasText a => HasText (HarmonicT a)


-- SlideT


instance Tiable a => Tiable (SlideT a) where
    toTied (SlideT (eg,es,a,bg,bs))                = (SlideT (eg,   es,   b,False,False),
                                                      SlideT (False,False,c,bg,   bs)) where (b,c) = toTied a
instance HasPart a => HasPart (SlideT a) where
    type Part (SlideT a)                           = Part a
    getPart (SlideT (eg,es,a,bg,bs))               = getPart a
    modifyPart f (SlideT (eg,es,a,bg,bs))          = SlideT (eg,es,modifyPart f a,bg,bs)
instance HasChord a => HasChord (SlideT a) where
    type ChordNote (SlideT a)                           = SlideT (ChordNote a)
    getChord (SlideT (eg,es,a,bg,bs))              = fmap (\x -> SlideT (eg,es,x,bg,bs)) (getChord a)

type instance Pitch (SlideT a) = Pitch a
instance HasGetPitch a => HasGetPitch (SlideT a) where
    getPitch = getPitch . get1
instance HasSetPitch a b => HasSetPitch (SlideT a) (SlideT b) where
    type SetPitch g (SlideT a) = SlideT (SetPitch g a)
    mapPitch f = fmap (mapPitch f)

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
    TremoloT (v,a) + TremoloT (_,b) = TremoloT (v,a+b)
    TremoloT (v,a) * TremoloT (_,b) = TremoloT (v,a*b)
    TremoloT (v,a) - TremoloT (_,b) = TremoloT (v,a-b)
    abs (TremoloT (v,a))          = TremoloT (v,abs a)
    signum (TremoloT (v,a))       = TremoloT (v,signum a)
    fromInteger a               = TremoloT (toEnum 0,fromInteger a)

instance Enum a => Enum (TremoloT a) where
    toEnum a = TremoloT (0, toEnum a) -- TODO use def, mempty or minBound?
    fromEnum (TremoloT (v,a)) = fromEnum a

instance Bounded a => Bounded (TremoloT a) where
    minBound = TremoloT (0, minBound)
    maxBound = TremoloT (0, maxBound)

instance (Num a, Real a) => Real (TremoloT a) where
    toRational (TremoloT (_,a)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (TremoloT a) where
    TremoloT (v,a) `quotRem` TremoloT (_,b) = (TremoloT (v,q), TremoloT   (v,r)) where (q,r) = a `quotRem` b
    toInteger (TremoloT (_,a)) = toInteger a


-- TextT

instance Num a => Num (TextT a) where
    TextT (v,a) + TextT (_,b) = TextT (v,a+b)
    TextT (v,a) * TextT (_,b) = TextT (v,a*b)
    TextT (v,a) - TextT (_,b) = TextT (v,a-b)
    abs (TextT (v,a))          = TextT (v,abs a)
    signum (TextT (v,a))       = TextT (v,signum a)
    fromInteger a               = TextT (mempty,fromInteger a)

instance Enum a => Enum (TextT a) where
    toEnum a = TextT (mempty, toEnum a) -- TODO use def, mempty or minBound?
    fromEnum (TextT (v,a)) = fromEnum a

instance Bounded a => Bounded (TextT a) where
    minBound = TextT (mempty, minBound)
    maxBound = TextT (mempty, maxBound)

instance (Num a, Ord a, Real a) => Real (TextT a) where
    toRational (TextT (v,a)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (TextT a) where
    TextT (v,a) `quotRem` TextT (_,b) = (TextT (v,q), TextT   (v,r)) where (q,r) = a `quotRem` b
    toInteger (TextT (v,a)) = toInteger a


-- HarmonicT

instance Num a => Num (HarmonicT a) where
    HarmonicT (v,a) + HarmonicT (_,b) = HarmonicT (v,a+b)
    HarmonicT (v,a) * HarmonicT (_,b) = HarmonicT (v,a*b)
    HarmonicT (v,a) - HarmonicT (_,b) = HarmonicT (v,a-b)
    abs (HarmonicT (v,a))          = HarmonicT (v,abs a)
    signum (HarmonicT (v,a))       = HarmonicT (v,signum a)
    fromInteger = point . fromInteger

instance Enum a => Enum (HarmonicT a) where
    toEnum = point . toEnum
    fromEnum = fromEnum . get1

instance Bounded a => Bounded (HarmonicT a) where
    minBound = point minBound
    maxBound = point maxBound

instance (Num a, Ord a, Real a) => Real (HarmonicT a) where
    toRational (HarmonicT (v,a)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (HarmonicT a) where
    HarmonicT (v,a) `quotRem` HarmonicT (_,b) = (HarmonicT (v,q), HarmonicT   (v,r)) where (q,r) = a `quotRem` b
    toInteger (HarmonicT (v,a)) = toInteger a


-- SlideT

instance Num a => Num (SlideT a) where
    SlideT (eg,es,a,bg,bs) + SlideT (_,_,b,_,_) = SlideT (eg,es,a+b,bg,bs)
    SlideT (eg,es,a,bg,bs) * SlideT (_,_,b,_,_) = SlideT (eg,es,a*b,bg,bs)
    SlideT (eg,es,a,bg,bs) - SlideT (_,_,b,_,_) = SlideT (eg,es,a-b,bg,bs)
    abs (SlideT (eg,es,a,bg,bs))                = SlideT (eg,es,abs a,bg,bs)
    signum (SlideT (eg,es,a,bg,bs))             = SlideT (eg,es,signum a,bg,bs)
    fromInteger a                               = SlideT (False,False,fromInteger a,False,False)

instance Enum a => Enum (SlideT a) where
    toEnum a                        = SlideT (False,False,toEnum a,False,False)
    fromEnum (SlideT (_,_,a,_,_))   = fromEnum a

instance Bounded a => Bounded (SlideT a) where
    minBound = SlideT (False,False,minBound,False,False)
    maxBound = SlideT (False,False,maxBound,False,False)

instance (Num a, Ord a, Real a) => Real (SlideT a) where
    toRational (SlideT (_,_,a,_,_)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (SlideT a) where
    SlideT (eg,es,a,bg,bs) `quotRem` SlideT (_,_,b,_,_) = (SlideT (eg,es,q',bg,bs), SlideT (eg,es,r',bg,bs)) where (q',r') = a `quotRem` b
    toInteger (SlideT (_,_,a,_,_)) = toInteger a


-- Safe for tuple-like types
get1 = head . toList
-- TODO replace with extract

