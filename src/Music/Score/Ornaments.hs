                              
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
import Data.Typeable
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace

import Music.Score.Voice
import Music.Score.Score
import Music.Time.Relative
import Music.Time.Absolute
import Music.Score.Part
import Music.Score.Combinators

class HasTremolo a where
    setTrem :: Int -> a -> a

newtype TremoloT a = TremoloT { getTremoloT :: (Int, a) }
    deriving (Eq, Show, Ord, Functor{-, Foldable-}, Typeable)

class HasText a where
    addText :: String -> a -> a

newtype TextT a = TextT { getTextT :: ([String], a) }
    deriving (Eq, Show, Ord, Functor{-, Foldable-}, Typeable)


-- 0 for none, positive for natural, negative for artificial
class HasHarmonic a where
    setHarmonic :: Int -> a -> a

newtype HarmonicT a = HarmonicT { getHarmonicT :: (Int, a) }
    deriving (Eq, Show, Ord, Functor{-, Foldable-}, Typeable)

-- end gliss/slide, level, begin gliss/slide
class HasSlide a where
    setBeginGliss :: Bool -> a -> a
    setBeginSlide :: Bool -> a -> a
    setEndGliss   :: Bool -> a -> a
    setEndSlide   :: Bool -> a -> a

newtype SlideT a = SlideT { getSlideT :: (Bool, Bool, a, Bool, Bool) }
    deriving (Eq, Show, Ord, Functor{-, Foldable-}, Typeable)


-- |
-- Set the number of tremolo divisions for all notes in the score.
--
tremolo :: (Functor f, HasTremolo b) => Int -> f b -> f b
tremolo n = fmap (setTrem n)

-- |
-- Attach the given text to the first note in the score.
--
text :: (HasPart' b, HasText b) => String -> Score b -> Score b
text s = mapPhrase (addText s) id id

-- |
-- Slide between the first and the last note.
--
slide :: (HasPart' b, HasSlide b) => Score b -> Score b
slide = mapPhrase (setBeginSlide True) id (setEndSlide True)

-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
--
harmonic :: (HasPart' b, HasHarmonic b) => Int -> Score b -> Score b
harmonic n = mapPhrase f f f where f = setHarmonic n

-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
--
artificial :: (HasPart' b, HasHarmonic b) => Score b -> Score b
artificial = mapPhrase f f f where f = setHarmonic (-4)

