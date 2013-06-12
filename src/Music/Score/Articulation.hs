                              
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
-- Provides articulation.
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

--------------------------------------------------------------------------------
-- Articulation
--------------------------------------------------------------------------------

-- Accents

accent :: (HasEvents s a, HasPart' a, HasArticulation a) => s a -> s a
marcato :: (HasEvents s a, HasPart' a, HasArticulation a) => s a -> s a
accentAll :: (HasEvents s a, HasPart' a, HasArticulation a) => s a -> s a
marcatoAll :: (HasEvents s a, HasPart' a, HasArticulation a) => s a -> s a
accentLast :: (HasEvents s a, HasPart' a, HasArticulation a) => s a -> s a
marcatoLast :: (HasEvents s a, HasPart' a, HasArticulation a) => s a -> s a

tenuto :: (HasEvents s a, HasPart' a, HasArticulation a) => s a -> s a
separated :: (HasEvents s a, HasPart' a, HasArticulation a) => s a -> s a
staccato :: (HasEvents s a, HasPart' a, HasArticulation a) => s a -> s a
portato :: (HasEvents s a, HasPart' a, HasArticulation a) => s a -> s a
legato :: (HasEvents s a, HasPart' a, HasArticulation a) => s a -> s a
spiccato :: (HasEvents s a, HasPart' a, HasArticulation a) => s a -> s a

accent = mapPhrase (setAccLevel 1) id id
marcato = mapPhrase (setAccLevel 2) id id
accentAll = mapPhrase (setAccLevel 1) (setAccLevel 1) (setAccLevel 1)
marcatoAll = mapPhrase (setAccLevel 2) (setAccLevel 2) (setAccLevel 2)
accentLast = mapPhrase id id (setAccLevel 1)
marcatoLast = mapPhrase id id (setAccLevel 2)

-- Phrasing
tenuto = mapPhrase (setStaccLevel (-2)) (setStaccLevel (-2)) (setStaccLevel (-2)) 
separated = mapPhrase (setStaccLevel (-1)) (setStaccLevel (-1)) (setStaccLevel (-1)) 
staccato = mapPhrase (setStaccLevel 1) (setStaccLevel 1) (setStaccLevel 1) 
portato = staccato . legato 
legato = mapPhrase (setBeginSlur True) id (setEndSlur True) 
spiccato = mapPhrase (setStaccLevel 2) (setStaccLevel 2) (setStaccLevel 2) 

resetArticulation :: HasArticulation c => c -> c
resetArticulation = setBeginSlur False . setContSlur False . setEndSlur False . setAccLevel 0 . setStaccLevel 0


