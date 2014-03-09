
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

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
-- Provides functions for manipulating ornaments (and miscellaneous stuff to be
-- given its own module soon...).
--
-------------------------------------------------------------------------------------


module Music.Score.Ornaments (
        -- * Tremolo
        HasTremolo(..),
        TremoloT(..),
        tremolo,

        -- * Text
        HasText(..),
        TextT(..),
        text,

        -- * Harmonics
        HasHarmonic(..),
        HarmonicT(..),
        harmonic,
        artificial,

        -- * Slides and glissando
        HasSlide(..),
        SlideT(..),
        slide,
        glissando,
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Foldable
import           Data.Foldable
import           Data.Ratio
import           Data.Semigroup
import           Data.Typeable

import           Music.Score.Combinators
import           Music.Score.Part
import           Music.Score.Score
import           Music.Score.Voice
import           Music.Time

class HasTremolo a where
    setTrem :: Int -> a -> a

newtype TremoloT a = TremoloT { getTremoloT :: (Sum Int, a) }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad)

instance HasTremolo (TremoloT a) where
    setTrem      n (TremoloT (_,x))                 = TremoloT (Sum n,x)

instance HasTremolo b => HasTremolo (a, b) where
    setTrem n = fmap (setTrem n)

instance HasTremolo a => HasTremolo (Score a) where
    setTrem n = fmap (setTrem n)





class HasText a where
    addText :: String -> a -> a

newtype TextT a = TextT { getTextT :: ([String], a) }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad)

instance HasText (TextT a) where
    addText      s (TextT (t,x))                    = TextT (t ++ [s],x)

instance HasText a => HasText (b, a) where
    addText       s                                 = fmap (addText s)

instance HasText a => HasText (Score a) where
    addText       s                                 = fmap (addText s)




-- 0 for none, positive for natural, negative for artificial
class HasHarmonic a where
    setNatural :: Bool -> a -> a
    setHarmonic :: Int -> a -> a

-- (isNatural, overtone series index where 0 is fundamental)
newtype HarmonicT a = HarmonicT { getHarmonicT :: ((Any, Sum Int), a) }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad)

instance HasHarmonic (HarmonicT a) where
    setNatural b (HarmonicT ((_,n),x)) = HarmonicT ((Any b,n),x)
    setHarmonic n (HarmonicT ((nat,_),x)) = HarmonicT ((nat,Sum n),x)

instance HasHarmonic a => HasHarmonic (b, a) where
    setNatural b = fmap (setNatural b)
    setHarmonic n = fmap (setHarmonic n)

instance HasHarmonic a => HasHarmonic (Score a) where
    setNatural b = fmap (setNatural b)
    setHarmonic n = fmap (setHarmonic n)



class HasSlide a where
    setBeginGliss :: Bool -> a -> a
    setBeginSlide :: Bool -> a -> a
    setEndGliss   :: Bool -> a -> a
    setEndSlide   :: Bool -> a -> a

-- (eg,es,a,bg,bs)
newtype SlideT a = SlideT { getSlideT :: (((Any, Any), (Any, Any)), a) }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad)

instance Wrapped (SlideT a) where
    type Unwrapped (SlideT a) = (((Any, Any), (Any, Any)), a)
    _Wrapped' = iso getSlideT SlideT 

bg, bs, eg, es :: Lens' (SlideT a) Any
bg = _Wrapped' . _1 . _2 . _1
bs = _Wrapped' . _1 . _2 . _2
eg = _Wrapped' . _1 . _1 . _1
es = _Wrapped' . _1 . _1 . _2

instance HasSlide (SlideT a) where
    setBeginGliss x = bg .~ Any x
    setBeginSlide x = bs .~ Any x
    setEndGliss   x = eg .~ Any x
    setEndSlide   x = es .~ Any x

instance HasSlide a => HasSlide (b, a) where
    setBeginGliss n = fmap (setBeginGliss n)
    setBeginSlide n = fmap (setBeginSlide n)
    setEndGliss   n = fmap (setEndGliss n)
    setEndSlide   n = fmap (setEndSlide n)

instance HasSlide a => HasSlide (Score a) where
    setBeginGliss n = fmap (setBeginGliss n)
    setBeginSlide n = fmap (setBeginSlide n)
    setEndGliss   n = fmap (setEndGliss n)
    setEndSlide   n = fmap (setEndSlide n)

-- |
-- Set the number of tremolo divisions for all notes in the score.
--
tremolo :: HasTremolo a => Int -> a -> a
tremolo = setTrem

-- |
-- Attach the given text to the first note in the score.
--
text :: (HasPart' a, HasText a) => String -> Score a -> Score a
text s = mapPhrase (addText s) id id

-- |
-- Add a slide between the first and the last note.
--
slide :: (HasPart' a, HasSlide a) => Score a -> Score a
slide = mapPhrase (setBeginSlide True) id (setEndSlide True)

-- |
-- Add a glissando between the first and the last note.
--
glissando :: (HasPart' a, HasSlide a) => Score a -> Score a
glissando = mapPhrase (setBeginGliss True) id (setEndGliss True)

-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
--
harmonic :: HasHarmonic a => Int -> a -> a
harmonic n = setNatural True . setHarmonic n
-- TODO verify this can actually be played

-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
--
artificial :: HasHarmonic a => a -> a
artificial =  setNatural False . setHarmonic 3

