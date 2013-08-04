
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
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
import Data.Semigroup
import Data.Ratio
import Data.Maybe
import Data.Foldable
import Data.Typeable
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace

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

-------------------------------------------------------------------------------------


instance (IsPitch a, Enum n) => IsPitch (PartT n a) where
    fromPitch l                                     = PartT (toEnum 0, fromPitch l)
instance (IsDynamics a, Enum n) => IsDynamics (PartT n a) where
    fromDynamics l                                  = PartT (toEnum 0, fromDynamics l)

instance IsPitch a => IsPitch (TieT a) where
    fromPitch l                                     = TieT (False, fromPitch l, False)
instance IsDynamics a => IsDynamics (TieT a) where
    fromDynamics l                                  = TieT (False, fromDynamics l, False)

instance IsPitch a => IsPitch (DynamicT a) where
    fromPitch l                                     = DynamicT (False,False,Nothing,fromPitch l,False,False)
instance IsDynamics a => IsDynamics (DynamicT a) where
    fromDynamics l                                  = DynamicT (False,False,Nothing,fromDynamics l,False,False)

instance IsPitch a => IsPitch (ArticulationT a) where
    fromPitch l                                     = ArticulationT (False,False,0,0,fromPitch l,False)
instance IsDynamics a => IsDynamics (ArticulationT a) where
    fromDynamics l                                  = ArticulationT (False,False,0,0,fromDynamics l,False)

instance IsPitch a => IsPitch (TremoloT a) where
    fromPitch l                                     = TremoloT (0, fromPitch l)
instance IsDynamics a => IsDynamics (TremoloT a) where
    fromDynamics l                                  = TremoloT (0, fromDynamics l)

instance IsPitch a => IsPitch (TextT a) where
    fromPitch l                                     = TextT (mempty, fromPitch l)
instance IsDynamics a => IsDynamics (TextT a) where
    fromDynamics l                                  = TextT (mempty, fromDynamics l)

instance IsPitch a => IsPitch (HarmonicT a) where
    fromPitch l                                     = HarmonicT (0, fromPitch l)
instance IsDynamics a => IsDynamics (HarmonicT a) where
    fromDynamics l                                  = HarmonicT (0, fromDynamics l)

instance IsPitch a => IsPitch (SlideT a) where
    fromPitch l                                     = SlideT (False,False,fromPitch l,False,False)
instance IsDynamics a => IsDynamics (SlideT a) where
    fromDynamics l                                  = SlideT (False,False,fromDynamics l,False,False)


-------------------------------------------------------------------------------------
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
    type Part (Maybe a)                             = Maybe (Part a) -- !
    getPart Nothing                                 = Nothing
    getPart (Just a)                                = Just (getPart a)
    modifyPart f (Nothing)                          = Nothing
    modifyPart f (Just a)                           = Just (modifyPart (fromJust . f . Just) a) -- TODO use cofunctor
instance HasPitch a => HasPitch (Maybe a) where
    type PitchOf (Maybe a)                           = Maybe (PitchOf a) -- !
    getPitch Nothing                                 = Nothing
    getPitch (Just a)                                = Just (getPitch a)
    modifyPitch f (Nothing)                          = Nothing
    modifyPitch f (Just a)                           = Just (modifyPitch (fromJust . f . Just) a)


-- PitchT


-- PartT


instance HasPart (PartT n a) where
    type Part (PartT n a)                           = n
    getPart (PartT (v,_))                           = v
    modifyPart f (PartT (v,x))                      = PartT (f v, x)
instance HasChord a => HasChord (PartT n a) where
    type ChordNote (PartT n a)                      = PartT n (ChordNote a)
    getChord (PartT (v,x))                          = fmap (\x -> PartT (v,x)) (getChord x)
instance HasPitch a => HasPitch (PartT n a) where
    type PitchOf (PartT n a)                        = PitchOf a
    getPitch (PartT (v,a))                          = getPitch a
    modifyPitch f (PartT (v,x))                     = PartT (v, modifyPitch f x)
instance Tiable a => Tiable (PartT n a) where
    beginTie = fmap beginTie
    endTie   = fmap endTie
    toTied (PartT (v,a)) = (PartT (v,b), PartT (v,c)) where (b,c) = toTied a
instance HasDynamic a => HasDynamic (PartT n a) where
    setBeginCresc n (PartT (v,x))                   = PartT (v, setBeginCresc n x)
    setEndCresc   n (PartT (v,x))                   = PartT (v, setEndCresc n x)
    setBeginDim   n (PartT (v,x))                   = PartT (v, setBeginDim n x)
    setEndDim     n (PartT (v,x))                   = PartT (v, setEndDim n x)
    setLevel      n (PartT (v,x))                   = PartT (v, setLevel n x)
instance HasArticulation a => HasArticulation (PartT n a) where
    setEndSlur    n (PartT (v,x))                   = PartT (v, setEndSlur n x)
    setContSlur   n (PartT (v,x))                   = PartT (v, setContSlur n x)
    setBeginSlur  n (PartT (v,x))                   = PartT (v, setBeginSlur n x)
    setAccLevel   n (PartT (v,x))                   = PartT (v, setAccLevel n x)
    setStaccLevel n (PartT (v,x))                   = PartT (v, setStaccLevel n x)
instance HasTremolo a => HasTremolo (PartT n a) where
    setTrem       n (PartT (v,x))                   = PartT (v, setTrem n x)
instance HasHarmonic a => HasHarmonic (PartT n a) where
    setHarmonic   n (PartT (v,x))                   = PartT (v, setHarmonic n x)
instance HasSlide a => HasSlide (PartT n a) where
    setBeginGliss n (PartT (v,x))                   = PartT (v, setBeginGliss n x)
    setBeginSlide n (PartT (v,x))                   = PartT (v, setBeginSlide n x)
    setEndGliss   n (PartT (v,x))                   = PartT (v, setEndGliss n x)
    setEndSlide   n (PartT (v,x))                   = PartT (v, setEndSlide n x)
instance HasText a => HasText (PartT n a) where
    addText       s (PartT (v,x))                   = PartT (v, addText s x)


-- ChordT

instance Tiable a => Tiable (ChordT a) where
    beginTie = fmap beginTie
    endTie   = fmap endTie
    toTied (ChordT as)                              = (ChordT bs, ChordT cs) where (bs,cs) = (unzip . fmap toTied) as
-- No HasPart instance, PartT must be outside ChordT
-- This restriction assures all chord notes are in the same part
instance HasChord (ChordT a) where
    type ChordNote (ChordT a)                       = a
    getChord (ChordT as)                            = as
-- Derived form the [a] instance
instance HasPitch a => HasPitch (ChordT a) where
    type PitchOf (ChordT a)                         = PitchOf a
    getPitch (ChordT as)                            = getPitch as
    modifyPitch f (ChordT as)                       = ChordT (modifyPitch f as)
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
    setHarmonic   n (ChordT as)                     = ChordT (fmap (setHarmonic n) as)
instance HasSlide a => HasSlide (ChordT a) where
    setBeginGliss n (ChordT as)                     = ChordT (fmap (setBeginGliss n) as)
    setBeginSlide n (ChordT as)                     = ChordT (fmap (setBeginSlide n) as)
    setEndGliss   n (ChordT as)                     = ChordT (fmap (setEndGliss n) as)
    setEndSlide   n (ChordT as)                     = ChordT (fmap (setEndSlide n) as)
instance HasText a => HasText (ChordT a) where
    addText      s (ChordT as)                      = ChordT (mapFirstL (addText s) as)


-- TieT

instance HasPart a => HasPart (TieT a) where
    type Part (TieT a)                              = Part a
    getPart (TieT (_,x,_))                          = getPart x
    modifyPart f (TieT (b,x,e))                     = TieT (b,modifyPart f x,e)
instance HasChord a => HasChord (TieT a) where
    type ChordNote (TieT a   )                      = TieT (ChordNote a)
    getChord (TieT (b,x,e))                         = fmap (\x -> TieT (b,x,e)) (getChord x)
instance HasPitch a => HasPitch (TieT a) where
    type PitchOf (TieT a)                           = PitchOf a
    getPitch (TieT (_,x,_))                         = getPitch x
    modifyPitch f (TieT (b,x,e))                    = TieT (b,modifyPitch f x,e)
instance HasDynamic a => HasDynamic (TieT a) where
    setBeginCresc n (TieT (b,x,e))                  = TieT (b,setBeginCresc n x,e)
    setEndCresc   n (TieT (b,x,e))                  = TieT (b,setEndCresc n x,e)
    setBeginDim   n (TieT (b,x,e))                  = TieT (b,setBeginDim n x,e)
    setEndDim     n (TieT (b,x,e))                  = TieT (b,setEndDim n x,e)
    setLevel      n (TieT (b,x,e))                  = TieT (b,setLevel n x,e)
instance HasArticulation a => HasArticulation (TieT a) where
    setEndSlur    n (TieT (b,x,e))                  = TieT (b,setEndSlur n x,e)
    setContSlur   n (TieT (b,x,e))                  = TieT (b,setContSlur n x,e)
    setBeginSlur  n (TieT (b,x,e))                  = TieT (b,setBeginSlur n x,e)
    setAccLevel   n (TieT (b,x,e))                  = TieT (b,setAccLevel n x,e)
    setStaccLevel n (TieT (b,x,e))                  = TieT (b,setStaccLevel n x,e)
instance HasTremolo a => HasTremolo (TieT a) where
    setTrem       n (TieT (b,x,e))                  = TieT (b,setTrem n x,e)
instance HasHarmonic a => HasHarmonic (TieT a) where
    setHarmonic   n (TieT (b,x,e))                  = TieT (b,setHarmonic n x,e)
instance HasSlide a => HasSlide (TieT a) where
    setBeginGliss n (TieT (b,x,e))                  = TieT (b,setBeginGliss n x,e)
    setBeginSlide n (TieT (b,x,e))                  = TieT (b,setBeginSlide n x,e)
    setEndGliss   n (TieT (b,x,e))                  = TieT (b,setEndGliss n x,e)
    setEndSlide   n (TieT (b,x,e))                  = TieT (b,setEndSlide n x,e)
instance HasText a => HasText (TieT a) where
    addText       s (TieT (b,x,e))                  = TieT (b, addText s x, e)


-- DynamicT

-- end cresc/dim, level, begin cresc/dim
-- newtype DynamicT a = DynamicT { getDynamicT :: (Bool, Bool, Maybe Double, a, Bool, Bool) }

instance Tiable a => Tiable (DynamicT a) where
    beginTie = fmap beginTie
    endTie   = fmap endTie
    toTied (DynamicT (ec,ed,l,a,bc,bd))             = (DynamicT (ec,ed,l,b,bc,bd),
                                                       DynamicT (False,False,Nothing,c,False,False)) where (b,c) = toTied a
instance HasPart a => HasPart (DynamicT a) where
    type Part (DynamicT a)                          = Part a
    getPart (DynamicT (ec,ed,l,a,bc,bd))            = getPart a
    modifyPart f (DynamicT (ec,ed,l,a,bc,bd))       = DynamicT (ec,ed,l,modifyPart f a,bc,bd)
instance HasChord a => HasChord (DynamicT a) where
    type ChordNote (DynamicT a)                     = DynamicT (ChordNote a)
    getChord (DynamicT (ec,ed,l,a,bc,bd))            = fmap (\x -> DynamicT (ec,ed,l,x,bc,bd)) (getChord a)
instance HasPitch a => HasPitch (DynamicT a) where
    type PitchOf (DynamicT a)                       = PitchOf a
    getPitch (DynamicT (ec,ed,l,a,bc,bd))           = getPitch a
    modifyPitch f (DynamicT (ec,ed,l,a,bc,bd))      = DynamicT (ec,ed,l,modifyPitch f a,bc,bd)
instance HasDynamic (DynamicT a) where
    setBeginCresc bc (DynamicT (ec,ed,l,a,_ ,bd))   = DynamicT (ec,ed,l,a,bc,bd)
    setEndCresc   ec (DynamicT (_ ,ed,l,a,bc,bd))   = DynamicT (ec,ed,l,a,bc,bd)
    setBeginDim   bd (DynamicT (ec,ed,l,a,bc,_ ))   = DynamicT (ec,ed,l,a,bc,bd)
    setEndDim     ed (DynamicT (ec,_ ,l,a,bc,bd))   = DynamicT (ec,ed,l,a,bc,bd)
    setLevel      l  (DynamicT (ec,ed,_,a,bc,bd))   = DynamicT (ec,ed,Just l,a,bc,bd)
instance HasArticulation a => HasArticulation (DynamicT a) where
    setEndSlur    n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setEndSlur n a,bc,bd)
    setContSlur   n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setContSlur n a,bc,bd)
    setBeginSlur  n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setBeginSlur n a,bc,bd)
    setAccLevel   n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setAccLevel n a,bc,bd)
    setStaccLevel n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setStaccLevel n a,bc,bd)
instance HasTremolo a => HasTremolo (DynamicT a) where
    setTrem       n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setTrem n a,bc,bd)
instance HasHarmonic a => HasHarmonic (DynamicT a) where
    setHarmonic   n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setHarmonic n a,bc,bd)
instance HasSlide a => HasSlide (DynamicT a) where
    setBeginGliss n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setBeginGliss n a,bc,bd)
    setBeginSlide n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setBeginSlide n a,bc,bd)
    setEndGliss   n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setEndGliss n a,bc,bd)
    setEndSlide   n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setEndSlide n a,bc,bd)
instance HasText a => HasText (DynamicT a) where
    addText       s (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,addText s a,bc,bd)


-- ArticulationT

-- end slur, cont slur, acc level, stacc level, begin slur
-- newtype ArticulationT a = ArticulationT { getArticulationT :: (Bool, Bool, Int, Int, a, Bool) }


instance Tiable a => Tiable (ArticulationT a) where
    beginTie = fmap beginTie
    endTie   = fmap endTie
    toTied (ArticulationT (es,us,al,sl,a,bs))           = (ArticulationT (False,us,al,sl,b,bs),
                                                           ArticulationT (es,   us,0,0,c,False)) where (b,c) = toTied a
instance HasPart a => HasPart (ArticulationT a) where
    type Part (ArticulationT a)                         = Part a
    getPart (ArticulationT (es,us,al,sl,a,bs))          = getPart a
    modifyPart f (ArticulationT (es,us,al,sl,a,bs))     = ArticulationT (es,us,al,sl,modifyPart f a,bs)
instance HasChord a => HasChord (ArticulationT a) where
    type ChordNote (ArticulationT a)                    = ArticulationT (ChordNote a)
    getChord (ArticulationT (es,us,al,sl,a,bs))         = fmap (\x -> ArticulationT (es,us,al,sl,x,bs)) (getChord a)
instance HasPitch a => HasPitch (ArticulationT a) where
    type PitchOf (ArticulationT a)                      = PitchOf a
    getPitch (ArticulationT (es,us,al,sl,a,bs))         = getPitch a
    modifyPitch f (ArticulationT (es,us,al,sl,a,bs))    = ArticulationT (es,us,al,sl,modifyPitch f a,bs)
instance HasDynamic a => HasDynamic (ArticulationT a) where
    setBeginCresc n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setBeginCresc n a,bs)
    setEndCresc   n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setEndCresc n a,bs)
    setBeginDim   n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setBeginDim n a,bs)
    setEndDim     n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setEndDim n a,bs)
    setLevel      n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setLevel n a,bs)
instance HasArticulation (ArticulationT a) where
    setEndSlur    es (ArticulationT (_ ,us,al,sl,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
    setContSlur   us (ArticulationT (es,_ ,al,sl,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
    setBeginSlur  bs (ArticulationT (es,us,al,sl,a,_ )) = ArticulationT (es,us,al,sl,a,bs)
    setAccLevel   al (ArticulationT (es,us,_ ,sl,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
    setStaccLevel sl (ArticulationT (es,us,al,_ ,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
instance HasTremolo a => HasTremolo (ArticulationT a) where
    setTrem n (ArticulationT (es,us,al,sl,a,bs))        = ArticulationT (es,us,al,sl,setTrem n a,bs)
instance HasHarmonic a => HasHarmonic (ArticulationT a) where
    setHarmonic   n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setHarmonic n a,bs)
instance HasSlide a => HasSlide (ArticulationT a) where
    setBeginGliss n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setBeginGliss n a,bs)
    setBeginSlide n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setBeginSlide n a,bs)
    setEndGliss   n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setEndGliss n a,bs)
    setEndSlide   n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setEndSlide n a,bs)
instance HasText a => HasText (ArticulationT a) where
    addText      s (ArticulationT (es,us,al,sl,a,bs))   = ArticulationT (es,us,al,sl,addText s a,bs)


-- TremoloT

-- newtype TremoloT a = TremoloT { getTremoloT :: (Int, a) }


instance Tiable a => Tiable (TremoloT a) where
    beginTie = fmap beginTie
    endTie   = fmap endTie
    toTied (TremoloT (n,a))                         = (TremoloT (n,b), TremoloT (n,c)) where (b,c) = toTied a
instance HasPart a => HasPart (TremoloT a) where
    type Part (TremoloT a)                          = Part a
    getPart (TremoloT (_,a))                        = getPart a
    modifyPart f (TremoloT (n,x))                   = TremoloT (n, modifyPart f x)
instance HasChord a => HasChord (TremoloT a) where
    type ChordNote (TremoloT a)                     = TremoloT (ChordNote a)
    getChord (TremoloT (n,x))                       = fmap (\x -> TremoloT (n,x)) (getChord x)
instance HasPitch a => HasPitch (TremoloT a) where
    type PitchOf (TremoloT a)                       = PitchOf a
    getPitch (TremoloT (_,a))                       = getPitch a
    modifyPitch f (TremoloT (n,x))                  = TremoloT (n, modifyPitch f x)
instance HasDynamic a => HasDynamic (TremoloT a) where
    setBeginCresc n (TremoloT (v,x))                = TremoloT (v, setBeginCresc n x)
    setEndCresc   n (TremoloT (v,x))                = TremoloT (v, setEndCresc n x)
    setBeginDim   n (TremoloT (v,x))                = TremoloT (v, setBeginDim n x)
    setEndDim     n (TremoloT (v,x))                = TremoloT (v, setEndDim n x)
    setLevel      n (TremoloT (v,x))                = TremoloT (v, setLevel n x)
instance HasArticulation a => HasArticulation (TremoloT a) where
    setEndSlur    n (TremoloT (v,x))                = TremoloT (v, setEndSlur n x)
    setContSlur   n (TremoloT (v,x))                = TremoloT (v, setContSlur n x)
    setBeginSlur  n (TremoloT (v,x))                = TremoloT (v, setBeginSlur n x)
    setAccLevel   n (TremoloT (v,x))                = TremoloT (v, setAccLevel n x)
    setStaccLevel n (TremoloT (v,x))                = TremoloT (v, setStaccLevel n x)
instance HasTremolo (TremoloT a) where
    setTrem      n (TremoloT (_,x))                 = TremoloT (n,x)
instance HasHarmonic a => HasHarmonic (TremoloT a) where
    setHarmonic   n (TremoloT (v,x))                = TremoloT (v, setHarmonic n x)
instance HasSlide a => HasSlide (TremoloT a) where
    setBeginGliss n (TremoloT (v,x))                = TremoloT (v, setBeginGliss n x)
    setBeginSlide n (TremoloT (v,x))                = TremoloT (v, setBeginSlide n x)
    setEndGliss   n (TremoloT (v,x))                = TremoloT (v, setEndGliss n x)
    setEndSlide   n (TremoloT (v,x))                = TremoloT (v, setEndSlide n x)
instance HasText a => HasText (TremoloT a) where
    addText      s (TremoloT (n,x))                 = TremoloT (n,addText s x)


-- TextT

-- newtype TextT a = TextT { getTextT :: (Int, a) }

instance Tiable a => Tiable (TextT a) where
    beginTie = fmap beginTie
    endTie   = fmap endTie
    toTied (TextT (n,a))                            = (TextT (n,b), TextT (mempty,c)) where (b,c) = toTied a
instance HasPart a => HasPart (TextT a) where
    type Part (TextT a)                             = Part a
    getPart (TextT (_,a))                           = getPart a
    modifyPart f (TextT (n,x))                      = TextT (n, modifyPart f x)
instance HasChord a => HasChord (TextT a) where
    type ChordNote (TextT a)                         = TextT (ChordNote a)
    getChord (TextT (n,x))                           = fmap (\x -> TextT (n,x)) (getChord x)
instance HasPitch a => HasPitch (TextT a) where
    type PitchOf (TextT a)                          = PitchOf a
    getPitch (TextT (_,a))                          = getPitch a
    modifyPitch f (TextT (n,x))                     = TextT (n, modifyPitch f x)
instance HasDynamic a => HasDynamic (TextT a) where
    setBeginCresc n (TextT (v,x))                   = TextT (v, setBeginCresc n x)
    setEndCresc   n (TextT (v,x))                   = TextT (v, setEndCresc n x)
    setBeginDim   n (TextT (v,x))                   = TextT (v, setBeginDim n x)
    setEndDim     n (TextT (v,x))                   = TextT (v, setEndDim n x)
    setLevel      n (TextT (v,x))                   = TextT (v, setLevel n x)
instance HasArticulation a => HasArticulation (TextT a) where
    setEndSlur    n (TextT (v,x))                   = TextT (v, setEndSlur n x)
    setContSlur   n (TextT (v,x))                   = TextT (v, setContSlur n x)
    setBeginSlur  n (TextT (v,x))                   = TextT (v, setBeginSlur n x)
    setAccLevel   n (TextT (v,x))                   = TextT (v, setAccLevel n x)
    setStaccLevel n (TextT (v,x))                   = TextT (v, setStaccLevel n x)
instance HasTremolo a => HasTremolo (TextT a) where
    setTrem       n (TextT (s,x))                   = TextT (s,setTrem n x)
instance HasHarmonic a => HasHarmonic (TextT a) where
    setHarmonic   n (TextT (v,x))                   = TextT (v, setHarmonic n x)
instance HasSlide a => HasSlide (TextT a) where
    setBeginGliss n (TextT (v,x))                   = TextT (v, setBeginGliss n x)
    setBeginSlide n (TextT (v,x))                   = TextT (v, setBeginSlide n x)
    setEndGliss   n (TextT (v,x))                   = TextT (v, setEndGliss n x)
    setEndSlide   n (TextT (v,x))                   = TextT (v, setEndSlide n x)
instance HasText (TextT a) where
    addText      s (TextT (t,x))                    = TextT (t ++ [s],x)


-- HarmonicT

instance Tiable a => Tiable (HarmonicT a) where
    beginTie = fmap beginTie
    endTie   = fmap endTie
    toTied (HarmonicT (n,a))                        = (HarmonicT (n,b), HarmonicT (n,c)) where (b,c) = toTied a
instance HasPart a => HasPart (HarmonicT a) where
    type Part (HarmonicT a)                         = Part a
    getPart (HarmonicT (_,a))                       = getPart a
    modifyPart f (HarmonicT (n,x))                  = HarmonicT (n, modifyPart f x)
instance HasChord a => HasChord (HarmonicT a) where
    type ChordNote (HarmonicT a)                    = HarmonicT (ChordNote a)
    getChord (HarmonicT (n,x))                      = fmap (\x -> HarmonicT (n,x)) (getChord x)
instance HasPitch a => HasPitch (HarmonicT a) where
    type PitchOf (HarmonicT a)                      = PitchOf a
    getPitch (HarmonicT (_,a))                      = getPitch a
    modifyPitch f (HarmonicT (n,x))                 = HarmonicT (n, modifyPitch f x)
instance HasDynamic a => HasDynamic (HarmonicT a) where
    setBeginCresc n (HarmonicT (v,x))               = HarmonicT (v, setBeginCresc n x)
    setEndCresc   n (HarmonicT (v,x))               = HarmonicT (v, setEndCresc n x)
    setBeginDim   n (HarmonicT (v,x))               = HarmonicT (v, setBeginDim n x)
    setEndDim     n (HarmonicT (v,x))               = HarmonicT (v, setEndDim n x)
    setLevel      n (HarmonicT (v,x))               = HarmonicT (v, setLevel n x)
instance HasArticulation a => HasArticulation (HarmonicT a) where
    setEndSlur    n (HarmonicT (v,x))               = HarmonicT (v, setEndSlur n x)
    setContSlur   n (HarmonicT (v,x))               = HarmonicT (v, setContSlur n x)
    setBeginSlur  n (HarmonicT (v,x))               = HarmonicT (v, setBeginSlur n x)
    setAccLevel   n (HarmonicT (v,x))               = HarmonicT (v, setAccLevel n x)
    setStaccLevel n (HarmonicT (v,x))               = HarmonicT (v, setStaccLevel n x)
instance HasTremolo a => HasTremolo (HarmonicT a) where
    setTrem       n (HarmonicT (s,x))               = HarmonicT (s,setTrem n x)
instance HasHarmonic (HarmonicT a) where
    setHarmonic   n (HarmonicT (_,x))               = HarmonicT (n,x)
instance HasSlide a => HasSlide (HarmonicT a) where
    setBeginGliss n (HarmonicT (s,x))               = HarmonicT (s,setBeginGliss n x)
    setBeginSlide n (HarmonicT (s,x))               = HarmonicT (s,setBeginSlide n x)
    setEndGliss   n (HarmonicT (s,x))               = HarmonicT (s,setEndGliss n x)
    setEndSlide   n (HarmonicT (s,x))               = HarmonicT (s,setEndSlide n x)
instance HasText a => HasText (HarmonicT a) where
    addText      s (HarmonicT (n,x))                = HarmonicT (n,addText s x)


-- SlideT


instance Tiable a => Tiable (SlideT a) where
    beginTie = fmap beginTie
    endTie   = fmap endTie
    toTied (SlideT (eg,es,a,bg,bs))                = (SlideT (eg,   es,   b,False,False),
                                                 SlideT (False,False,c,bg,   bs)) where (b,c) = toTied a
instance HasPart a => HasPart (SlideT a) where
    type Part (SlideT a)                           = Part a
    getPart (SlideT (eg,es,a,bg,bs))               = getPart a
    modifyPart f (SlideT (eg,es,a,bg,bs))          = SlideT (eg,es,modifyPart f a,bg,bs)
instance HasChord a => HasChord (SlideT a) where
    type ChordNote (SlideT a)                      = SlideT (ChordNote a)
    getChord (SlideT (eg,es,a,bg,bs))              = fmap (\x -> SlideT (eg,es,x,bg,bs)) (getChord a)
instance HasPitch a => HasPitch (SlideT a) where
    type PitchOf (SlideT a)                        = PitchOf a
    getPitch (SlideT (eg,es,a,bg,bs))              = getPitch a
    modifyPitch f (SlideT (eg,es,a,bg,bs))         = SlideT (eg,es,modifyPitch f a,bg,bs)
instance HasDynamic a => HasDynamic (SlideT a) where
    setBeginCresc n (SlideT (eg,es,a,bg,bs))       = SlideT (eg,es,setBeginCresc n a,bg,bs)
    setEndCresc   n (SlideT (eg,es,a,bg,bs))       = SlideT (eg,es,setEndCresc n a,bg,bs)
    setBeginDim   n (SlideT (eg,es,a,bg,bs))       = SlideT (eg,es,setBeginDim n a,bg,bs)
    setEndDim     n (SlideT (eg,es,a,bg,bs))       = SlideT (eg,es,setEndDim n a,bg,bs)
    setLevel      n (SlideT (eg,es,a,bg,bs))       = SlideT (eg,es,setLevel n a,bg,bs)
instance HasArticulation a => HasArticulation (SlideT a) where
    setEndSlur    n (SlideT (eg,es,a,bg,bs))       = SlideT (eg,es,setEndSlur n a,bg,bs)
    setContSlur   n (SlideT (eg,es,a,bg,bs))       = SlideT (eg,es,setContSlur n a,bg,bs)
    setBeginSlur  n (SlideT (eg,es,a,bg,bs))       = SlideT (eg,es,setBeginSlur n a,bg,bs)
    setAccLevel   n (SlideT (eg,es,a,bg,bs))       = SlideT (eg,es,setAccLevel n a,bg,bs)
    setStaccLevel n (SlideT (eg,es,a,bg,bs))       = SlideT (eg,es,setStaccLevel n a,bg,bs)
instance HasTremolo a => HasTremolo (SlideT a) where
    setTrem       n (SlideT (eg,es,a,bg,bs))       = SlideT (eg,es,setTrem n a,bg,bs)
instance HasHarmonic a => HasHarmonic (SlideT a) where
    setHarmonic   n (SlideT (eg,es,a,bg,bs))       = SlideT (eg,es,setHarmonic n a,bg,bs)
instance HasSlide (SlideT a) where
    setBeginGliss bg (SlideT (eg,es,a,_,bs))       = SlideT (eg,es,a,bg,bs)
    setBeginSlide bs (SlideT (eg,es,a,bg,_))       = SlideT (eg,es,a,bg,bs)
    setEndGliss   eg (SlideT (_,es,a,bg,bs))       = SlideT (eg,es,a,bg,bs)
    setEndSlide   es (SlideT (eg,_,a,bg,bs))       = SlideT (eg,es,a,bg,bs)
instance HasText a => HasText (SlideT a) where
    addText       s (SlideT (eg,es,a,bg,bs))       = SlideT (eg,es,addText s a,bg,bs)


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


-- ArticulationT

instance Num a => Num (ArticulationT a) where
    ArticulationT (p,q,r,s,a,t) + ArticulationT (_,_,_,_,b,_) = ArticulationT (p,q,r,s,a+b,t)
    ArticulationT (p,q,r,s,a,t) * ArticulationT (_,_,_,_,b,_) = ArticulationT (p,q,r,s,a*b,t)
    ArticulationT (p,q,r,s,a,t) - ArticulationT (_,_,_,_,b,_) = ArticulationT (p,q,r,s,a-b,t)
    abs (ArticulationT (p,q,r,s,a,t))                         = ArticulationT (p,q,r,s,abs a,t)
    signum (ArticulationT (p,q,r,s,a,t))                      = ArticulationT (p,q,r,s,signum a,t)
    fromInteger a                                             = ArticulationT (False,False,0,0,fromInteger a,False)

instance Enum a => Enum (ArticulationT a) where
    toEnum a                               = ArticulationT (False,False,0,0,toEnum a,False)
    fromEnum (ArticulationT (_,_,_,_,a,_)) = fromEnum a

instance Bounded a => Bounded (ArticulationT a) where
    minBound = ArticulationT (False,False,0,0,minBound,False)
    maxBound = ArticulationT (False,False,0,0,maxBound,False)

instance (Num a, Ord a, Real a) => Real (ArticulationT a) where
    toRational (ArticulationT (_,_,_,_,a,_)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (ArticulationT a) where
    ArticulationT (p,q,r,s,a,t) `quotRem` ArticulationT (_,_,_,_,b,_) = (ArticulationT (p,q,r,s,q',t), ArticulationT (p,q,r,s,r',t)) where (q',r') = a `quotRem` b
    toInteger (ArticulationT (_,_,_,_,a,_)) = toInteger a


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
    fromInteger a               = HarmonicT (toEnum 0,fromInteger a)

instance Enum a => Enum (HarmonicT a) where
    toEnum a = HarmonicT (0, toEnum a) -- TODO use def, mempty or minBound?
    fromEnum (HarmonicT (v,a)) = fromEnum a

instance Bounded a => Bounded (HarmonicT a) where
    minBound = HarmonicT (0, minBound)
    maxBound = HarmonicT (0, maxBound)

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





mapFirstL f = mapFirstMiddleLast f id id

mapFirstMiddleLast :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapFirstMiddleLast f g h = go
    where
        go []    = []
        go [a]   = [f a]
        go [a,b] = [f a, h b]
        go xs    = [f $ head xs]          ++
                   map g (tail $ init xs) ++
                   [h $ last xs]
