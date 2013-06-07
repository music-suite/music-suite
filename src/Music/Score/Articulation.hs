                              
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
-- Provides a musical score represenation.
--
-------------------------------------------------------------------------------------


module Music.Score.Articulation (
        HasArticulation(..),
        ArticulationT(..),
        
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
import Music.Time.Relative
import Music.Time.Absolute
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

--------------------------------------------------------------------------------
-- Articulation
--------------------------------------------------------------------------------

-- Accents

accent :: (HasArticulation a, HasPart' a) => Score a -> Score a
accent = mapPhrase (setAccLevel 1) id id

marcato :: (HasArticulation a, HasPart' a) => Score a -> Score a
marcato = mapPhrase (setAccLevel 2) id id

accentAll :: (HasArticulation a, HasPart' a) => Score a -> Score a
accentAll = mapPhrase (setAccLevel 1) (setAccLevel 1) (setAccLevel 1)

marcatoAll :: (HasArticulation a, HasPart' a) => Score a -> Score a
marcatoAll = mapPhrase (setAccLevel 2) (setAccLevel 2) (setAccLevel 2)

accentLast :: (HasArticulation a, HasPart' a) => Score a -> Score a
accentLast = mapPhrase id id (setAccLevel 1)

marcatoLast :: (HasArticulation a, HasPart' a) => Score a -> Score a
marcatoLast = mapPhrase id id (setAccLevel 2)

-- Phrasing

tenuto :: (HasArticulation a, HasPart' a) => Score a -> Score a
tenuto = mapPhrase (setStaccLevel (-2)) (setStaccLevel (-2)) (setStaccLevel (-2)) 

separated :: (HasArticulation a, HasPart' a) => Score a -> Score a
separated = mapPhrase (setStaccLevel (-1)) (setStaccLevel (-1)) (setStaccLevel (-1)) 

staccato :: (HasArticulation a, HasPart' a) => Score a -> Score a
staccato = mapPhrase (setStaccLevel 1) (setStaccLevel 1) (setStaccLevel 1) 

portato :: (HasArticulation a, HasPart' a) => Score a -> Score a
portato = staccato . legato 

legato :: (HasArticulation a, HasPart' a) => Score a -> Score a
legato = mapPhrase (setBeginSlur True) id (setEndSlur True) 

spiccato :: (HasArticulation a, HasPart' a) => Score a -> Score a
spiccato = mapPhrase (setStaccLevel 2) (setStaccLevel 2) (setStaccLevel 2) 

resetArticulation :: HasArticulation c => c -> c
resetArticulation = setBeginSlur False . setContSlur False . setEndSlur False . setAccLevel 0 . setStaccLevel 0


