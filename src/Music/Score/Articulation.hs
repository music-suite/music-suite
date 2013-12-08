
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction #-}

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
-- Provides functions for manipulating articulation.
--
-------------------------------------------------------------------------------------


module Music.Score.Articulation (
        -- * Representation
        HasArticulation(..),
        ArticulationT(..),

        -- * Transformations
        -- ** Accents
        accent,
        marcato,
        accentLast,
        marcatoLast,
        accentAll,
        marcatoAll,

        -- ** Phrasing
        tenuto,
        separated,
        staccato,
        portato,
        legato,
        spiccato,

        -- ** Miscellaneous
        resetArticulation,

  ) where

import Data.Pointed
import Data.Foldable
import Data.Typeable
import Data.Semigroup

import Music.Score.Score
import Music.Score.Part
import Music.Score.Combinators
import Music.Pitch.Literal
import Music.Dynamics.Literal

class HasArticulation a where
    setBeginSlur :: Bool -> a -> a
    setContSlur :: Bool -> a -> a
    setEndSlur :: Bool -> a -> a
    setAccLevel :: Int -> a -> a
    setStaccLevel :: Int -> a -> a

newtype ArticulationT a = ArticulationT { getArticulationT :: (Bool, Bool, Int, Int, a, Bool) }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable)

instance Pointed ArticulationT where
    point x = ArticulationT (False,False,0,0,x,False)

instance Semigroup a => Semigroup (ArticulationT a) where
    ArticulationT (es,us,al,sl,a,bs) <> ArticulationT (_,_,_,_,b,_) = ArticulationT (es,us,al,sl,a <> b,bs)

instance (Semigroup a, Monoid a) => Monoid (ArticulationT a) where
    mempty = point mempty
    mappend = (<>)

instance IsPitch a => IsPitch (ArticulationT a) where
    fromPitch l = point (fromPitch l)

instance IsDynamics a => IsDynamics (ArticulationT a) where
    fromDynamics l = point (fromDynamics l)

instance Num a => Num (ArticulationT a) where
    ArticulationT (p,q,r,s,a,t) + ArticulationT (_,_,_,_,b,_) = ArticulationT (p,q,r,s,a+b,t)
    ArticulationT (p,q,r,s,a,t) * ArticulationT (_,_,_,_,b,_) = ArticulationT (p,q,r,s,a*b,t)
    ArticulationT (p,q,r,s,a,t) - ArticulationT (_,_,_,_,b,_) = ArticulationT (p,q,r,s,a-b,t)
    abs (ArticulationT (p,q,r,s,a,t))                         = ArticulationT (p,q,r,s,abs a,t)
    signum (ArticulationT (p,q,r,s,a,t))                      = ArticulationT (p,q,r,s,signum a,t)
    fromInteger a                                             = ArticulationT (False,False,0,0,fromInteger a,False)

instance Enum a => Enum (ArticulationT a) where
    toEnum = point . toEnum
    fromEnum = fromEnum . get1 

instance Bounded a => Bounded (ArticulationT a) where
    minBound = point minBound
    maxBound = point maxBound

instance (Num a, Ord a, Real a) => Real (ArticulationT a) where
    toRational = toRational . get1

instance (Real a, Enum a, Integral a) => Integral (ArticulationT a) where
    ArticulationT (p,q,r,s,a,t) `quotRem` ArticulationT (_,_,_,_,b,_) = (ArticulationT (p,q,r,s,q',t), ArticulationT (p,q,r,s,r',t)) where (q',r') = a `quotRem` b
    toInteger = toInteger . get1

instance HasArticulation (ArticulationT a) where
    setEndSlur    es (ArticulationT (_ ,us,al,sl,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
    setContSlur   us (ArticulationT (es,_ ,al,sl,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
    setBeginSlur  bs (ArticulationT (es,us,al,sl,a,_ )) = ArticulationT (es,us,al,sl,a,bs)
    setAccLevel   al (ArticulationT (es,us,_ ,sl,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
    setStaccLevel sl (ArticulationT (es,us,al,_ ,a,bs)) = ArticulationT (es,us,al,sl,a,bs)

instance HasArticulation b => HasArticulation (a,b) where
    setEndSlur    n = fmap (setEndSlur n)
    setContSlur   n = fmap (setContSlur n)
    setBeginSlur  n = fmap (setBeginSlur n)
    setAccLevel   n = fmap (setAccLevel n)
    setStaccLevel n = fmap (setStaccLevel n)

--------------------------------------------------------------------------------
-- Articulation
--------------------------------------------------------------------------------

-- Accents

-- | Add a normal accent at the beginning of each phrase in each part in the given score.
accent      :: (HasPart' a, HasArticulation a) => Score a -> Score a
accent      = mapPhrase (setAccLevel 1) id id

-- | Add a marcato accent at the beginning of each phrase in each part in the given score.
marcato     :: (HasPart' a, HasArticulation a) => Score a -> Score a
marcato     = mapPhrase (setAccLevel 2) id id

-- | Add a normal accent to all notes in the given score.
accentAll   :: (HasPart' a, HasArticulation a) => Score a -> Score a
accentAll   = mapPhrase (setAccLevel 1) (setAccLevel 1) (setAccLevel 1)

-- | Add a marcato accent to all notes in the given score.
marcatoAll  :: (HasPart' a, HasArticulation a) => Score a -> Score a
marcatoAll  = mapPhrase (setAccLevel 2) (setAccLevel 2) (setAccLevel 2)

-- | Add a normal accent at the end of each phrase in each part in the given score.
accentLast  :: (HasPart' a, HasArticulation a) => Score a -> Score a
accentLast  = mapPhrase id id (setAccLevel 1)

-- | Add a marcato accent at the end of each phrase in each part in the given score.
marcatoLast :: (HasPart' a, HasArticulation a) => Score a -> Score a
marcatoLast = mapPhrase id id (setAccLevel 2)


-- Phrasing

-- | Add tenuto marks to each phrase in each part in the given score.
tenuto      :: (HasPart' a, HasArticulation a) => Score a -> Score a
tenuto      = mapPhrase (setStaccLevel (-2)) (setStaccLevel (-2)) (setStaccLevel (-2))

-- | Add combined staccato and tenuto marks to each phrase in each part in the given score.
separated   :: (HasPart' a, HasArticulation a) => Score a -> Score a
separated   = mapPhrase (setStaccLevel (-1)) (setStaccLevel (-1)) (setStaccLevel (-1))

-- | Add staccato marks to each phrase in each part in the given score.
staccato    :: (HasPart' a, HasArticulation a) => Score a -> Score a
staccato    = mapPhrase (setStaccLevel 1) (setStaccLevel 1) (setStaccLevel 1)

-- | Add portato marks to each phrase in each part in the given score.
portato     :: (HasPart' a, HasArticulation a) => Score a -> Score a
portato     = staccato . legato

-- | Add legato marks to each phrase in each part in the given score.
legato      :: (HasPart' a, HasArticulation a) => Score a -> Score a
legato      = mapPhrase (setBeginSlur True) id (setEndSlur True)

-- | Add spiccatto marks to the given score.
spiccato    :: (HasPart' a, HasArticulation a) => Score a -> Score a
spiccato    = mapPhrase (setStaccLevel 2) (setStaccLevel 2) (setStaccLevel 2)

-- | Remove all articulation from the given note or notes.
resetArticulation :: HasArticulation c => c -> c
resetArticulation = setBeginSlur False . setContSlur False . setEndSlur False . setAccLevel 0 . setStaccLevel 0

-- Safe for tuple-like types
get1 = head . toList

