-- | Pitch range or ambitus.
module Music.Pitch.Ambitus
  ( Ambitus,
    ambitus,
    -- ambitus',
    mapAmbitus,
    ambitusHighest,
    ambitusLowest,
    ambitusInterval,
  )
where

import Control.Lens
import Data.AffineSpace
import Data.Interval hiding (Interval, interval)
import qualified Data.Interval as I
import Data.VectorSpace

-- | An ambitus is (mathematical) interval.
--
-- Also known as /range/ or /tessitura/, this type can be used to restrict the
-- range instruments, chords, melodies etc.
newtype Ambitus a = Ambitus {getAmbitus :: (I.Interval a)}

instance Wrapped (Ambitus a) where

  type Unwrapped (Ambitus a) = I.Interval a

  _Wrapped' = iso getAmbitus Ambitus

instance Rewrapped (Ambitus a) (Ambitus b)

instance (Show a, Num a, Ord a) => Show (Ambitus a) where
  show a = show (a ^. from ambitus) ++ "^.ambitus"

ambitus :: (Num a, Ord a) => Iso (a, a) (b, b) (Ambitus a) (Ambitus b)
ambitus = iso toA unA . _Unwrapped
  where
    toA = (\(m, n) -> (I.<=..<=) (Finite m) (Finite n))
    unA a = case (I.lowerBound a, I.upperBound a) of
      (Finite m, Finite n) -> (m, n)
      -- FIXME this can happen as empty span can be represented as PosInf..NegInf
      -- _                    -> error $ "Strange ambitus: " ++ show (I.lowerBound a, I.upperBound a)
      _ -> error $ "Strange ambitus"

-- ambitus' :: (Num a, Ord a) => Iso' (a, a) (Ambitus a)
-- ambitus' = ambitus

-- | Not a true functor for similar reasons as sets.
mapAmbitus :: (Ord b, Num b) => (a -> b) -> Ambitus a -> Ambitus b
mapAmbitus = over (from ambitus . both)

-- | Returns a postive interval (or _P1 for empty ambitus)
ambitusInterval :: (Num a, Ord a, AffineSpace a) => Ambitus a -> Diff a
ambitusInterval x = let (m, n) = x ^. from ambitus in n .-. m

ambitusLowest :: (Num a, Ord a) => Ambitus a -> a
ambitusLowest x = let (m, n) = x ^. from ambitus in m

ambitusHighest :: (Num a, Ord a) => Ambitus a -> a
ambitusHighest x = let (m, n) = x ^. from ambitus in n

inAmbitus :: (Ord a, Num a) => Ambitus a -> a -> Bool
inAmbitus amb p = m <= p && p <= n
  where
    (m,n) = amb^.from ambitus
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
