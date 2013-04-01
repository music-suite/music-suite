                              
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


module Music.Score.Ornaments (
        HasTremolo(..),
        TremoloT(..),
        HasText(..),
        TextT(..),
        HasHarmonic(..),
        HarmonicT(..),
        HasSlide(..),
        SlideT(..),
        
        tremolo,
        text,
        harmonic,
        artificial,
        slide,
  ) where

import Data.Ratio
import Data.Foldable
import Data.Monoid
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace

import Music.Score.Part
import Music.Score.Score
import Music.Score.Duration
import Music.Score.Time
import Music.Score.Voice
import Music.Score.Combinators

class HasTremolo a where
    setTrem :: Int -> a -> a

newtype TremoloT a = TremoloT { getTremoloT :: (Int, a) }
    deriving (Eq, Show, Ord, Functor{-, Foldable-})

class HasText a where
    addText :: String -> a -> a

newtype TextT a = TextT { getTextT :: ([String], a) }
    deriving (Eq, Show, Ord, Functor{-, Foldable-})


-- 0 for none, positive for natural, negative for artificial
class HasHarmonic a where
    setHarmonic :: Int -> a -> a

newtype HarmonicT a = HarmonicT { getHarmonicT :: (Int, a) }
    deriving (Eq, Show, Ord, Functor{-, Foldable-})

-- end gliss/slide, level, begin gliss/slide
class HasSlide a where
    setBeginGliss :: Bool -> a -> a
    setBeginSlide :: Bool -> a -> a
    setEndGliss   :: Bool -> a -> a
    setEndSlide   :: Bool -> a -> a

newtype SlideT a = SlideT { getSlideT :: (Bool, Bool, a, Bool, Bool) }
    deriving (Eq, Show, Ord, Functor{-, Foldable-})


-- |
-- Add tremolo cross-beams to all notes in the score.
--
tremolo :: (Functor f, HasTremolo b) => Int -> f b -> f b
tremolo n = fmap (setTrem n)

-- |
-- Add text to the first note in the score.
--
text :: (Ord v, v ~ Voice b, HasVoice b, HasText b) => String -> Score b -> Score b
text s = mapSep (addText s) id id

-- |
-- Slide between the first and the last note.
--
slide :: (Ord v, v ~ Voice b, HasVoice b, HasSlide b) => Score b -> Score b
slide = mapSep (setBeginSlide True) id (setEndSlide True)

-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
--
harmonic :: (Ord v, v ~ Voice b, HasVoice b, HasHarmonic b) => Int -> Score b -> Score b
harmonic n = mapSep f f f where f = setHarmonic n

-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
--
artificial :: (Ord v, v ~ Voice b, HasVoice b, HasHarmonic b) => Score b -> Score b
artificial = mapSep f f f where f = setHarmonic (-4)

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

                               