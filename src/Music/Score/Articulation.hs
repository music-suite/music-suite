
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

import Data.Ratio
import Data.Foldable
import Data.Typeable
import Data.Semigroup
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace

import Music.Score.Voice
import Music.Score.Score
import Music.Time
import Music.Score.Part
import Music.Score.Combinators

class HasArticulation a where
    setBeginSlur :: Bool -> a -> a
    setContSlur :: Bool -> a -> a
    setEndSlur :: Bool -> a -> a
    setAccLevel :: Int -> a -> a
    setStaccLevel :: Int -> a -> a

newtype ArticulationT a = ArticulationT { getArticulationT :: (Bool, Bool, Int, Int, a, Bool) }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable)

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

accent      :: (HasPart' a, HasArticulation a) => Score a -> Score a
marcato     :: (HasPart' a, HasArticulation a) => Score a -> Score a
accentAll   :: (HasPart' a, HasArticulation a) => Score a -> Score a
marcatoAll  :: (HasPart' a, HasArticulation a) => Score a -> Score a
accentLast  :: (HasPart' a, HasArticulation a) => Score a -> Score a
marcatoLast :: (HasPart' a, HasArticulation a) => Score a -> Score a
accent      = mapPhrase (setAccLevel 1) id id
marcato     = mapPhrase (setAccLevel 2) id id
accentAll   = mapPhrase (setAccLevel 1) (setAccLevel 1) (setAccLevel 1)
marcatoAll  = mapPhrase (setAccLevel 2) (setAccLevel 2) (setAccLevel 2)
accentLast  = mapPhrase id id (setAccLevel 1)
marcatoLast = mapPhrase id id (setAccLevel 2)

-- Phrasing

tenuto      :: (HasPart' a, HasArticulation a) => Score a -> Score a
separated   :: (HasPart' a, HasArticulation a) => Score a -> Score a
staccato    :: (HasPart' a, HasArticulation a) => Score a -> Score a
portato     :: (HasPart' a, HasArticulation a) => Score a -> Score a
legato      :: (HasPart' a, HasArticulation a) => Score a -> Score a
spiccato    :: (HasPart' a, HasArticulation a) => Score a -> Score a
tenuto      = mapPhrase (setStaccLevel (-2)) (setStaccLevel (-2)) (setStaccLevel (-2))
separated   = mapPhrase (setStaccLevel (-1)) (setStaccLevel (-1)) (setStaccLevel (-1))
staccato    = mapPhrase (setStaccLevel 1) (setStaccLevel 1) (setStaccLevel 1)
portato     = staccato . legato
legato      = mapPhrase (setBeginSlur True) id (setEndSlur True)
spiccato    = mapPhrase (setStaccLevel 2) (setStaccLevel 2) (setStaccLevel 2)

resetArticulation :: HasArticulation c => c -> c
resetArticulation = setBeginSlur False . setContSlur False . setEndSlur False . setAccLevel 0 . setStaccLevel 0


