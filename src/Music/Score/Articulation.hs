                              
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
        
  ) where

import Data.Ratio
import Data.Foldable
import Data.Semigroup
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace

import Music.Score.Part
import Music.Score.Score
import Music.Score.Duration
import Music.Score.Time
import Music.Score.Voice
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

accent :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
accent = mapSep (setAccLevel 1) id id

marcato :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
marcato = mapSep (setAccLevel 2) id id

accentAll :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
accentAll = mapSep (setAccLevel 1) (setAccLevel 1) (setAccLevel 1)

marcatoAll :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
marcatoAll = mapSep (setAccLevel 2) (setAccLevel 2) (setAccLevel 2)

accentLast :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
accentLast = mapSep id id (setAccLevel 1)

marcatoLast :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
marcatoLast = mapSep id id (setAccLevel 2)

-- Phrasing

tenuto :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
tenuto = mapSep (setStaccLevel (-2)) (setStaccLevel (-2)) (setStaccLevel (-2)) 

separated :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
separated = mapSep (setStaccLevel (-1)) (setStaccLevel (-1)) (setStaccLevel (-1)) 

staccato :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
staccato = mapSep (setStaccLevel 1) (setStaccLevel 1) (setStaccLevel 1) 

portato :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
portato = staccato . legato 

legato :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
legato = mapSep (setBeginSlur True) id (setEndSlur True) 

spiccato :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
spiccato = mapSep (setStaccLevel 2) (setStaccLevel 2) (setStaccLevel 2) 




-- FIXME consolidate

-- | 
-- Map over first, middle and last elements of list.
-- Biased on first, then on first and last for short lists.
-- 
mapSepL :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapSepL f g h []      = []
mapSepL f g h [a]     = [f a]
mapSepL f g h [a,b]   = [f a, h b]
mapSepL f g h xs      = [f $ head xs] ++ (map g $ tail $ init xs) ++ [h $ last xs]

mapSep :: (HasVoice a, Ord v, v ~ Voice a) => (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapSep f g h sc = fixDur . mapVoices (fmap $ mapSepPart f g h) $ sc
    where
        fixDur a = padAfter (duration sc - duration a) a

mapSepPart :: (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapSepPart f g h sc = mconcat . mapSepL (fmap f) (fmap g) (fmap h) . fmap toSc . perform $ sc
    where             
        fixDur a = padAfter (duration sc - duration a) a
        toSc (t,d,x) = delay (t .-. 0) . stretch d $ note x
        third f (a,b,c) = (a,b,f c)

padAfter :: Duration -> Score a -> Score a
padAfter d a = a |> (rest^*d)       


