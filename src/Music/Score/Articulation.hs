                              
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

-- accent :: (HasArticulation a, MonadPlus' s, HasPart' a, Performable s, Delayable (s a), Stretchable (s a)) => s a -> s a
accent = mapPhrase (setAccLevel 1) id id

-- marcato :: (HasArticulation a, MonadPlus' s, HasPart' a, Performable s, Delayable (s a), Stretchable (s a)) => s a -> s a
marcato = mapPhrase (setAccLevel 2) id id

-- accentAll :: (HasArticulation a, MonadPlus' s, HasPart' a, Performable s, Delayable (s a), Stretchable (s a)) => s a -> s a
accentAll = mapPhrase (setAccLevel 1) (setAccLevel 1) (setAccLevel 1)

-- marcatoAll :: (HasArticulation a, MonadPlus' s, HasPart' a, Performable s, Delayable (s a), Stretchable (s a)) => s a -> s a
marcatoAll = mapPhrase (setAccLevel 2) (setAccLevel 2) (setAccLevel 2)

-- accentLast :: (HasArticulation a, MonadPlus' s, HasPart' a, Performable s, Delayable (s a), Stretchable (s a)) => s a -> s a
accentLast = mapPhrase id id (setAccLevel 1)

-- marcatoLast :: (HasArticulation a, MonadPlus' s, HasPart' a, Performable s, Delayable (s a), Stretchable (s a)) => s a -> s a
marcatoLast = mapPhrase id id (setAccLevel 2)

-- Phrasing

-- tenuto :: (HasArticulation a, MonadPlus' s, HasPart' a, Performable s, Delayable (s a), Stretchable (s a)) => s a -> s a
tenuto = mapPhrase (setStaccLevel (-2)) (setStaccLevel (-2)) (setStaccLevel (-2)) 

-- separated :: (HasArticulation a, MonadPlus' s, HasPart' a, Performable s, Delayable (s a), Stretchable (s a)) => s a -> s a
separated = mapPhrase (setStaccLevel (-1)) (setStaccLevel (-1)) (setStaccLevel (-1)) 

-- staccato :: (HasArticulation a, MonadPlus' s, HasPart' a, Performable s, Delayable (s a), Stretchable (s a)) => s a -> s a
staccato = mapPhrase (setStaccLevel 1) (setStaccLevel 1) (setStaccLevel 1) 

-- portato :: (HasArticulation a, MonadPlus' s, HasPart' a, Performable s, Delayable (s a), Stretchable (s a)) => s a -> s a
portato = staccato . legato 

-- legato :: (HasArticulation a, MonadPlus' s, HasPart' a, Performable s, Delayable (s a), Stretchable (s a)) => s a -> s a
legato = mapPhrase (setBeginSlur True) id (setEndSlur True) 

-- spiccato :: (HasArticulation a, MonadPlus' s, HasPart' a, Performable s, Delayable (s a), Stretchable (s a)) => s a -> s a
spiccato = mapPhrase (setStaccLevel 2) (setStaccLevel 2) (setStaccLevel 2) 

-- resetArticulation :: HasArticulation c => c -> c
resetArticulation = setBeginSlur False . setContSlur False . setEndSlur False . setAccLevel 0 . setStaccLevel 0


