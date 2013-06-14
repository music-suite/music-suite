
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
-- Provides functions for manipulating ornaments (and some other things...).
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
import Music.Time
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
tremolo :: (Functor s, HasTremolo b) => Int -> s b -> s b
tremolo n = fmap (setTrem n)

-- |
-- Attach the given text to the first note in the score.
--
text :: (HasEvents s, HasPart' a, HasText a) => String -> s a -> s a
text s = mapPhrase (addText s) id id

-- |
-- Slide between the first and the last note.
--
slide :: (HasEvents s, HasPart' a, HasSlide a) => s a -> s a
slide = mapPhrase (setBeginSlide True) id (setEndSlide True)

glissando :: (HasEvents s, HasPart' a, HasSlide a) => s a -> s a
glissando = mapPhrase (setBeginGliss True) id (setEndGliss True)
-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
--
harmonic :: (Functor s, HasHarmonic a) => Int -> s a -> s a
harmonic n = fmap (setHarmonic n)

-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
--
artificial :: (Functor s, HasHarmonic a) => s a -> s a
artificial = fmap f where f = setHarmonic (-4)

