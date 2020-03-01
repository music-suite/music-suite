{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints
  #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Pitch range or ambitus.
module Music.Pitch.Ambitus
  ( Ambitus,
    ambitus,
    -- ambitus',
    mapAmbitus,
    ambitusHighest,
    ambitusLowest,
    ambitusInterval,
    inAmbitus,
  )
where

import Control.Lens
import Data.AffineSpace
import Data.Interval hiding (Interval, interval)
import qualified Data.Interval as I
import Data.VectorSpace
import Music.Pitch.Common.Semitones

-- | An ambitus is (mathematical) interval.
--
-- Also known as /range/ or /tessitura/, this type can be used to restrict the
-- range instruments, chords, melodies etc.
data Ambitus a = Ambitus !a !a -- {getAmbitus :: (I.Interval a)}

instance Show a => Show (Ambitus a) where
  show a = show (a ^. from ambitus) ++ "^.ambitus"

ambitus :: () => Iso (a, a) (b, b) (Ambitus a) (Ambitus b)
ambitus = iso f g
  where
    f (x, y) = Ambitus x y
    g (Ambitus x y) = (x, y)

-- | Not a true functor for similar reasons as sets.
mapAmbitus :: () => (a -> b) -> Ambitus a -> Ambitus b
mapAmbitus f (Ambitus x y) = Ambitus (f x) (f y)

-- | Returns a postive interval (or _P1 for empty ambitus)
ambitusInterval :: (AffineSpace a) => Ambitus a -> Diff a
ambitusInterval (Ambitus x y) = x .-. y

ambitusLowest :: () => Ambitus a -> a
ambitusLowest (Ambitus x _y) = x

ambitusHighest :: () => Ambitus a -> a
ambitusHighest (Ambitus _x y) = y

inAmbitus :: (AffineSpace a, HasSemitones (Diff a)) => Ambitus a -> a -> Bool
inAmbitus (Ambitus a c) b =
  semitones (c .-. b) >= 0 && semitones (b .-. a) >= 0
{-
Misc stuff from data-interval and friends. What is relevant?

member :: Ord r => r -> Interval r -> Bool
isSubsetOf :: Ord r => Interval r -> Interval r -> Bool
isProperSubsetOf :: Ord r => Interval r -> Interval r -> Bool
-- TODO exact type here depends on what type of interval we use underneath
lowerBound :: Interval r -> Extended r
width :: (Num r, Ord r) => Interval r -> r
intersection :: Ord a => Interval a -> Interval a -> Interval a
hull :: Ord a => Interval a -> Interval a -> Interval a
bisect :: Fractional a => Interval a -> (Interval a, Interval a)
distance :: (Num a, Ord a) => Interval a -> Interval a -> a
(...) :: Ord a => a -> a -> Interval a infix 3
(+/-) :: (Num a, Ord a) => a -> a -> Interval a infixl 6
null :: Interval a -> Bool
singleton :: a -> Interval a
elem :: Ord a => a -> Interval a -> Bool
singular :: Ord a => Interval a -> Bool
width :: Num a => Interval a -> a
midpoint :: Fractional a => Interval a -> a
inflate/deflate :: (Num a, Ord a) => a -> Interval a -> Interval a
-}
