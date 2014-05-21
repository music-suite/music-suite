
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
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
import           Control.Comonad
import           Control.Lens hiding (transform)
import           Data.Functor.Couple
import           Data.Foldable
import           Data.Foldable
import           Data.Ratio
import           Data.Word
import           Data.Semigroup
import           Data.Typeable

-- import           Music.Score.Combinators
import           Music.Score.Part
import           Music.Time
import           Music.Pitch.Literal
import           Music.Dynamics.Literal
import           Music.Pitch.Alterable
import           Music.Pitch.Augmentable
import           Music.Score.Phrases

class HasTremolo a where
    setTrem :: Int -> a -> a

instance HasTremolo a => HasTremolo (b, a) where
    setTrem n = fmap (setTrem n)

instance HasTremolo a => HasTremolo (Couple b a) where
    setTrem n = fmap (setTrem n)

instance HasTremolo a => HasTremolo [a] where
    setTrem n = fmap (setTrem n)

instance HasTremolo a => HasTremolo (Score a) where
    setTrem n = fmap (setTrem n)



newtype TremoloT a = TremoloT { getTremoloT :: Couple (Max Word) a }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad, Comonad)
-- 
-- We use Word instead of Int to get (mempty = Max 0), as (Max.mempty = Max minBound)
-- Preferably we would use Natural but unfortunately this is not an instance of Bounded
-- 

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (TremoloT a) where
  type Unwrapped (TremoloT a) = Couple (Max Word) a
  _Wrapped' = iso getTremoloT TremoloT

instance Rewrapped (TremoloT a) (TremoloT b)

instance HasTremolo (TremoloT a) where
    setTrem n (TremoloT (Couple (_,x))) = TremoloT (Couple (Max $ fromIntegral n,x))

-- Lifted instances
deriving instance Num a => Num (TremoloT a)
deriving instance Fractional a => Fractional (TremoloT a)
deriving instance Floating a => Floating (TremoloT a)
deriving instance Enum a => Enum (TremoloT a)
deriving instance Bounded a => Bounded (TremoloT a)
deriving instance (Num a, Ord a, Real a) => Real (TremoloT a)
deriving instance (Real a, Enum a, Integral a) => Integral (TremoloT a)





class HasText a where
    addText :: String -> a -> a

newtype TextT a = TextT { getTextT :: Couple [String] a }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad, Comonad)

instance HasText a => HasText (b, a) where
    addText       s                                 = fmap (addText s)

instance HasText a => HasText (Couple b a) where
    addText       s                                 = fmap (addText s)

instance HasText a => HasText [a] where
    addText       s                                 = fmap (addText s)

instance HasText a => HasText (Score a) where
    addText       s                                 = fmap (addText s)


-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (TextT a) where
  type Unwrapped (TextT a) = Couple [String] a
  _Wrapped' = iso getTextT TextT

instance Rewrapped (TextT a) (TextT b)

instance HasText (TextT a) where
    addText      s (TextT (Couple (t,x)))                    = TextT (Couple (t ++ [s],x))

-- Lifted instances
deriving instance Num a => Num (TextT a)
deriving instance Fractional a => Fractional (TextT a)
deriving instance Floating a => Floating (TextT a)
deriving instance Enum a => Enum (TextT a)
deriving instance Bounded a => Bounded (TextT a)
deriving instance (Num a, Ord a, Real a) => Real (TextT a)
deriving instance (Real a, Enum a, Integral a) => Integral (TextT a)





-- 0 for none, positive for natural, negative for artificial
class HasHarmonic a where
    setNatural :: Bool -> a -> a
    setHarmonic :: Int -> a -> a

-- (isNatural, overtone series index where 0 is fundamental)
newtype HarmonicT a = HarmonicT { getHarmonicT :: Couple (Any, Sum Int) a }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad, Comonad)

instance HasHarmonic a => HasHarmonic (b, a) where
    setNatural b = fmap (setNatural b)
    setHarmonic n = fmap (setHarmonic n)

instance HasHarmonic a => HasHarmonic (Couple b a) where
    setNatural b = fmap (setNatural b)
    setHarmonic n = fmap (setHarmonic n)

instance HasHarmonic a => HasHarmonic [a] where
    setNatural b = fmap (setNatural b)
    setHarmonic n = fmap (setHarmonic n)

instance HasHarmonic a => HasHarmonic (Score a) where
    setNatural b = fmap (setNatural b)
    setHarmonic n = fmap (setHarmonic n)


-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (HarmonicT a) where
  type Unwrapped (HarmonicT a) = Couple (Any, Sum Int) a
  _Wrapped' = iso getHarmonicT HarmonicT

instance Rewrapped (HarmonicT a) (HarmonicT b)

instance HasHarmonic (HarmonicT a) where
    setNatural  b = over (_Wrapped'._Wrapped') $ \((_,n),x)   -> ((Any b,n),x)
    setHarmonic n = over (_Wrapped'._Wrapped') $ \((nat,_),x) -> ((nat,Sum n),x)

-- Lifted instances
deriving instance Num a => Num (HarmonicT a)
deriving instance Fractional a => Fractional (HarmonicT a)
deriving instance Floating a => Floating (HarmonicT a)
deriving instance Enum a => Enum (HarmonicT a)
deriving instance Bounded a => Bounded (HarmonicT a)
deriving instance (Num a, Ord a, Real a) => Real (HarmonicT a)
deriving instance (Real a, Enum a, Integral a) => Integral (HarmonicT a)



class HasSlide a where
    setBeginGliss :: Bool -> a -> a
    setBeginSlide :: Bool -> a -> a
    setEndGliss   :: Bool -> a -> a
    setEndSlide   :: Bool -> a -> a

instance HasSlide a => HasSlide (b, a) where
    setBeginGliss n = fmap (setBeginGliss n)
    setBeginSlide n = fmap (setBeginSlide n)
    setEndGliss   n = fmap (setEndGliss n)
    setEndSlide   n = fmap (setEndSlide n)

instance HasSlide a => HasSlide (Couple b a) where
    setBeginGliss n = fmap (setBeginGliss n)
    setBeginSlide n = fmap (setBeginSlide n)
    setEndGliss   n = fmap (setEndGliss n)
    setEndSlide   n = fmap (setEndSlide n)

instance HasSlide a => HasSlide [a] where
    setBeginGliss n = fmap (setBeginGliss n)
    setBeginSlide n = fmap (setBeginSlide n)
    setEndGliss   n = fmap (setEndGliss n)
    setEndSlide   n = fmap (setEndSlide n)

instance HasSlide a => HasSlide (Score a) where
    setBeginGliss n = fmap (setBeginGliss n)
    setBeginSlide n = fmap (setBeginSlide n)
    setEndGliss   n = fmap (setEndGliss n)
    setEndSlide   n = fmap (setEndSlide n)


-- (eg,es,a,bg,bs)
newtype SlideT a = SlideT { getSlideT :: Couple ((Any, Any), (Any, Any)) a }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad, Comonad)

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (SlideT a) where
  type Unwrapped (SlideT a) = Couple ((Any, Any), (Any, Any)) a
  _Wrapped' = iso getSlideT SlideT

instance Rewrapped (SlideT a) (SlideT b)

_bg, _bs, _eg, _es :: Lens' (SlideT a) Any
_bg = (_Wrapped'._Wrapped') . _1 . _2 . _1
_bs = (_Wrapped'._Wrapped') . _1 . _2 . _2
_eg = (_Wrapped'._Wrapped') . _1 . _1 . _1
_es = (_Wrapped'._Wrapped') . _1 . _1 . _2

instance HasSlide (SlideT a) where
    setBeginGliss x = _bg .~ Any x
    setBeginSlide x = _bs .~ Any x
    setEndGliss   x = _eg .~ Any x
    setEndSlide   x = _es .~ Any x

-- Lifted instances
deriving instance Num a => Num (SlideT a)
deriving instance Fractional a => Fractional (SlideT a)
deriving instance Floating a => Floating (SlideT a)
deriving instance Enum a => Enum (SlideT a)
deriving instance Bounded a => Bounded (SlideT a)
deriving instance (Num a, Ord a, Real a) => Real (SlideT a)
deriving instance (Real a, Enum a, Integral a) => Integral (SlideT a)

-- |
-- Set the number of tremolo divisions for all notes in the score.
--
tremolo :: HasTremolo a => Int -> a -> a
tremolo = setTrem

-- |
-- Attach the given text to the first note in the score.
--
text :: (HasPhrases' s a, HasText a) => String -> s -> s
text s = over (phrases'.headV) (addText s)
-- text s = mapPhraseWise3 (addText s) id id

-- |
-- Add a slide between the first and the last note.
--
slide :: (HasPhrases' s a, HasSlide a) => s -> s
slide = mapPhraseWise3 (setBeginSlide True) id (setEndSlide True)

-- |
-- Add a glissando between the first and the last note.
--
glissando :: (HasPhrases' s a, HasSlide a) => s -> s
glissando = mapPhraseWise3 (setBeginGliss True) id (setEndGliss True)

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

-- TODO allow polymorphic update
mapPhraseWise3 :: HasPhrases' s a => (a -> a) -> (a -> a) -> (a -> a) -> s -> s
mapPhraseWise3 f g h = over phrases' (over headV f . over middleV g . over lastV h)


deriving instance (Monoid b, IsPitch a) => IsPitch (Couple b a)
deriving instance (Monoid b, IsDynamics a) => IsDynamics (Couple b a)
deriving instance (Monoid b, Transformable a) => Transformable (Couple b a)
deriving instance (Monoid b, Reversible a) => Reversible (Couple b a)
deriving instance (Monoid b, Alterable a) => Alterable (Couple b a)
deriving instance (Monoid b, Augmentable a) => Augmentable (Couple b a)

deriving instance IsPitch a => IsPitch (TremoloT a)
deriving instance IsDynamics a => IsDynamics (TremoloT a)
deriving instance IsPitch a => IsPitch (TextT a)
deriving instance IsDynamics a => IsDynamics (TextT a)
deriving instance IsPitch a => IsPitch (HarmonicT a)
deriving instance IsDynamics a => IsDynamics (HarmonicT a)
deriving instance IsPitch a => IsPitch (SlideT a)
deriving instance IsDynamics a => IsDynamics (SlideT a)

deriving instance Transformable a => Transformable (SlideT a)
deriving instance Transformable a => Transformable (HarmonicT a)
deriving instance Transformable a => Transformable (TextT a)
deriving instance Transformable a => Transformable (TremoloT a)

deriving instance Reversible a => Reversible (SlideT a)
deriving instance Reversible a => Reversible (HarmonicT a)
deriving instance Reversible a => Reversible (TextT a)
deriving instance Reversible a => Reversible (TremoloT a)

deriving instance Alterable a => Alterable (SlideT a)
deriving instance Alterable a => Alterable (HarmonicT a)
deriving instance Alterable a => Alterable (TextT a)
deriving instance Alterable a => Alterable (TremoloT a)

deriving instance Augmentable a => Augmentable (SlideT a)
deriving instance Augmentable a => Augmentable (HarmonicT a)
deriving instance Augmentable a => Augmentable (TextT a)
deriving instance Augmentable a => Augmentable (TremoloT a)

