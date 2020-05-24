{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

-- | Pitch range or ambitus.
module Music.Pitch.Ambitus
  ( Ambitus (..),
    ambitusInterval,
    inAmbitus,
  )
where

import Control.Lens
import Data.AffineSpace
import Data.AffineSpace.Point.Offsets (AffinePair)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Interval hiding (Interval, interval)
import Data.VectorSpace
import Music.Pitch.Common.Semitones

-- | A set of pitches between two extremes.
--
-- Also known as range, tessitura or (in maths) interval.
data Ambitus v p = Ambitus {low :: !p, high :: !p}
  deriving (Functor, Foldable, Traversable, Show)

instance Bifunctor Ambitus where
  bimap _ g (Ambitus x y) = Ambitus (g x) (g y)

instance Bifoldable Ambitus where
  bifoldMap _ g (Ambitus x y) = g x <> g y

instance Bitraversable Ambitus where
  bitraverse _ g (Ambitus x y) = Ambitus <$> g x <*> g y

ambitusInterval :: (AffinePair v p) => Ambitus v p -> v
ambitusInterval (Ambitus x y) = x .-. y

inAmbitus :: (AffinePair v p, HasSemitones v) => Ambitus v p -> p -> Bool
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
