                              
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleInstances,
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
    deriving (Eq, Show, Ord, Functor, Foldable)

--------------------------------------------------------------------------------
-- Articulation
--------------------------------------------------------------------------------

-- Accents

accent :: (HasArticulation a, HasPart a, Ord v, v ~ Part a) => Score a -> Score a
accent = mapSep (setAccLevel 1) id id

marcato :: (HasArticulation a, HasPart a, Ord v, v ~ Part a) => Score a -> Score a
marcato = mapSep (setAccLevel 2) id id

accentAll :: (HasArticulation a, HasPart a, Ord v, v ~ Part a) => Score a -> Score a
accentAll = mapSep (setAccLevel 1) (setAccLevel 1) (setAccLevel 1)

marcatoAll :: (HasArticulation a, HasPart a, Ord v, v ~ Part a) => Score a -> Score a
marcatoAll = mapSep (setAccLevel 2) (setAccLevel 2) (setAccLevel 2)

accentLast :: (HasArticulation a, HasPart a, Ord v, v ~ Part a) => Score a -> Score a
accentLast = mapSep id id (setAccLevel 1)

marcatoLast :: (HasArticulation a, HasPart a, Ord v, v ~ Part a) => Score a -> Score a
marcatoLast = mapSep id id (setAccLevel 2)

-- Phrasing

tenuto :: (HasArticulation a, HasPart a, Ord v, v ~ Part a) => Score a -> Score a
tenuto = mapSep (setStaccLevel (-2)) (setStaccLevel (-2)) (setStaccLevel (-2)) 

separated :: (HasArticulation a, HasPart a, Ord v, v ~ Part a) => Score a -> Score a
separated = mapSep (setStaccLevel (-1)) (setStaccLevel (-1)) (setStaccLevel (-1)) 

staccato :: (HasArticulation a, HasPart a, Ord v, v ~ Part a) => Score a -> Score a
staccato = mapSep (setStaccLevel 1) (setStaccLevel 1) (setStaccLevel 1) 

portato :: (HasArticulation a, HasPart a, Ord v, v ~ Part a) => Score a -> Score a
portato = staccato . legato 

legato :: (HasArticulation a, HasPart a, Ord v, v ~ Part a) => Score a -> Score a
legato = mapSep (setBeginSlur True) id (setEndSlur True) 

spiccato :: (HasArticulation a, HasPart a, Ord v, v ~ Part a) => Score a -> Score a
spiccato = mapSep (setStaccLevel 2) (setStaccLevel 2) (setStaccLevel 2) 

resetArticulation :: HasArticulation c => c -> c
resetArticulation = setBeginSlur False . setContSlur False . setEndSlur False . setAccLevel 0 . setStaccLevel 0



-- FIXME consolidate

-- | 
-- Map over first, middle and last elements of list.
-- Biased on first, then on first and last for short lists.
-- 
mapSepL :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapSepL f g h []      = []
mapSepL f g h [a]     = [f a]
mapSepL f g h [a,b]   = [f a, h b]
mapSepL f g h xs      = [f $ head xs] ++ map g (tail $ init xs) ++ [h $ last xs]

mapSep :: (HasPart a, Ord v, v ~ Part a) => (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapSep f g h = {-fixDur .-} mapParts (fmap $ mapSepVoice f g h)
    -- where
        -- fixDur a = padAfter (duration sc - duration a) a

mapSepVoice :: (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapSepVoice f g h sc = mconcat . mapSepL (fmap f) (fmap g) (fmap h) . fmap toSc . perform $ sc
    where
        toSc (t,d,x) = delay (t .-. 0) . stretch d $ note x
        third f (a,b,c) = (a,b,f c)

