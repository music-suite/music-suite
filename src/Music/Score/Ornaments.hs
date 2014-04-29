
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
import           Control.Lens
import           Data.Foldable
import           Data.Foldable
import           Data.Ratio
import           Data.Word
import           Data.Semigroup
import           Data.Typeable

-- import           Music.Score.Combinators
import           Music.Score.Part
import           Music.Time

class HasTremolo a where
    setTrem :: Int -> a -> a

instance HasTremolo a => HasTremolo (b, a) where
    setTrem n = fmap (setTrem n)

instance HasTremolo a => HasTremolo [a] where
    setTrem n = fmap (setTrem n)

instance HasTremolo a => HasTremolo (Score a) where
    setTrem n = fmap (setTrem n)



newtype TremoloT a = TremoloT { getTremoloT :: (Max Word, a) }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad)
-- 
-- We use Word instead of Int to get (mempty = Max 0), as (Max.mempty = Max minBound)
-- Preferably we would use Natural but unfortunately this is not an instance of Bounded
-- 

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (TremoloT a) where
  type Unwrapped (TremoloT a) = (Max Word, a)
  _Wrapped' = iso getTremoloT TremoloT

instance Rewrapped (TremoloT a) (TremoloT b)

instance HasTremolo (TremoloT a) where
    setTrem n (TremoloT (_,x)) = TremoloT (Max $ fromIntegral n,x)

-- Lifted instances

instance Num a => Num (TremoloT a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (TremoloT a) where
    recip        = fmap recip
    fromRational = pure . fromRational

instance Floating a => Floating (TremoloT a) where
    pi    = pure pi
    sqrt  = fmap sqrt
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acos

instance Enum a => Enum (TremoloT a) where
    toEnum = pure . toEnum
    fromEnum = fromEnum . get1

instance Bounded a => Bounded (TremoloT a) where
    minBound = pure minBound
    maxBound = pure maxBound

instance (Num a, Ord a, Real a) => Real (TremoloT a) where
    toRational = toRational . get1

instance (Real a, Enum a, Integral a) => Integral (TremoloT a) where
    quot = liftA2 quot
    rem = liftA2 rem
    toInteger = toInteger . get1








class HasText a where
    addText :: String -> a -> a

newtype TextT a = TextT { getTextT :: ([String], a) }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad)

instance HasText a => HasText (b, a) where
    addText       s                                 = fmap (addText s)

instance HasText a => HasText [a] where
    addText       s                                 = fmap (addText s)

instance HasText a => HasText (Score a) where
    addText       s                                 = fmap (addText s)


-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (TextT a) where
  type Unwrapped (TextT a) = ([String], a)
  _Wrapped' = iso getTextT TextT

instance Rewrapped (TextT a) (TextT b)

instance HasText (TextT a) where
    addText      s (TextT (t,x))                    = TextT (t ++ [s],x)

-- Lifted instances

instance Num a => Num (TextT a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (TextT a) where
    recip        = fmap recip
    fromRational = pure . fromRational

instance Floating a => Floating (TextT a) where
    pi    = pure pi
    sqrt  = fmap sqrt
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acos

instance Enum a => Enum (TextT a) where
    toEnum = pure . toEnum
    fromEnum = fromEnum . get1

instance Bounded a => Bounded (TextT a) where
    minBound = pure minBound
    maxBound = pure maxBound

instance (Num a, Ord a, Real a) => Real (TextT a) where
    toRational = toRational . get1

instance (Real a, Enum a, Integral a) => Integral (TextT a) where
    quot = liftA2 quot
    rem = liftA2 rem
    toInteger = toInteger . get1




-- 0 for none, positive for natural, negative for artificial
class HasHarmonic a where
    setNatural :: Bool -> a -> a
    setHarmonic :: Int -> a -> a

-- (isNatural, overtone series index where 0 is fundamental)
newtype HarmonicT a = HarmonicT { getHarmonicT :: ((Any, Sum Int), a) }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad)

instance HasHarmonic a => HasHarmonic (b, a) where
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
  type Unwrapped (HarmonicT a) = ((Any, Sum Int), a)
  _Wrapped' = iso getHarmonicT HarmonicT

instance Rewrapped (HarmonicT a) (HarmonicT b)

instance HasHarmonic (HarmonicT a) where
    setNatural b (HarmonicT ((_,n),x)) = HarmonicT ((Any b,n),x)
    setHarmonic n (HarmonicT ((nat,_),x)) = HarmonicT ((nat,Sum n),x)

-- Lifted instances

instance Num a => Num (HarmonicT a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (HarmonicT a) where
    recip        = fmap recip
    fromRational = pure . fromRational

instance Floating a => Floating (HarmonicT a) where
    pi    = pure pi
    sqrt  = fmap sqrt
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acos

instance Enum a => Enum (HarmonicT a) where
    toEnum = pure . toEnum
    fromEnum = fromEnum . get1

instance Bounded a => Bounded (HarmonicT a) where
    minBound = pure minBound
    maxBound = pure maxBound

instance (Num a, Ord a, Real a) => Real (HarmonicT a) where
    toRational = toRational . get1

instance (Real a, Enum a, Integral a) => Integral (HarmonicT a) where
    quot = liftA2 quot
    rem = liftA2 rem
    toInteger = toInteger . get1
                                        



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
newtype SlideT a = SlideT { getSlideT :: (((Any, Any), (Any, Any)), a) }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad)

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (SlideT a) where
  type Unwrapped (SlideT a) = (((Any, Any), (Any, Any)), a)
  _Wrapped' = iso getSlideT SlideT

instance Rewrapped (SlideT a) (SlideT b)

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

-- Lifted instances

instance Num a => Num (SlideT a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (SlideT a) where
    recip        = fmap recip
    fromRational = pure . fromRational

instance Floating a => Floating (SlideT a) where
    pi    = pure pi
    sqrt  = fmap sqrt
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acos

instance Enum a => Enum (SlideT a) where
    toEnum = pure . toEnum
    fromEnum = fromEnum . get1

instance Bounded a => Bounded (SlideT a) where
    minBound = pure minBound
    maxBound = pure maxBound

instance (Num a, Ord a, Real a) => Real (SlideT a) where
    toRational = toRational . get1

instance (Real a, Enum a, Integral a) => Integral (SlideT a) where
    quot = liftA2 quot
    rem = liftA2 rem
    toInteger = toInteger . get1

-- |
-- Set the number of tremolo divisions for all notes in the score.
--
tremolo :: HasTremolo a => Int -> a -> a
tremolo = setTrem

-- |
-- Attach the given text to the first note in the score.
--
text :: (HasParts' a, HasText a) => String -> Score a -> Score a
text s = mapPhraseWise3 (addText s) id id

-- |
-- Add a slide between the first and the last note.
--
slide :: (HasParts' a, HasSlide a) => Score a -> Score a
slide = mapPhraseWise3 (setBeginSlide True) id (setEndSlide True)

-- |
-- Add a glissando between the first and the last note.
--
glissando :: (HasParts' a, HasSlide a) => Score a -> Score a
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


mapPhraseWise3 :: (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapPhraseWise3 f _ _ = fmap f
-- TODO


-- TODO replace with (^?!), extract or similar
get1 = head . toList

