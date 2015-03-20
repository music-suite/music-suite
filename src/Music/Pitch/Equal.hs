
{-# LANGUAGE RankNTypes #-}

{-| Generic equal temperament pitch.

Use the type-level numbers to construct an temperement dividing
the octave in any number of equal-sized steps.

Common cases such as 6, 12 and 24 are provided for convenience.
-}
module Music.Pitch.Equal
(
    -- * Equal temperament
    Equal,
    toEqual,
    fromEqual,
    equalToRatio,
    size,
    cast,

    -- ** Synonyms
    Equal6,
    Equal12,
    Equal17,
    Equal24,
    Equal36,

    -- ** Extra type-level naturals
    N20,
    N30,
    N17,
    N24,
    N36,
)
where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad
import Control.Applicative

import Music.Pitch.Absolute

import TypeUnary.Nat

-- Based on Data.Fixed

newtype Equal a = Equal { getEqual :: Int }

deriving instance Eq (Equal a)
deriving instance Ord (Equal a)

instance Show (Equal a) where
  show (Equal a) = show a
  -- OR:
  -- showsPrec d (Equal x) = showParen (d > app_prec) $
  --      showString "Equal " . showsPrec (app_prec+1) x
  --   where app_prec = 10

instance IsNat a => Num (Equal a) where
  Equal a + Equal b = Equal (a + b)
  Equal a * Equal b = Equal (a * b)
  negate (Equal a)  = Equal (negate a)
  abs (Equal a)     = Equal (abs a)
  signum (Equal a)  = Equal (signum a)
  fromInteger       = toEqual . fromIntegral

instance IsNat a => Semigroup (Equal a) where
  (<>)    = (+)

instance IsNat a => Monoid (Equal a) where
  mempty  = 0
  mappend = (+)

instance IsNat a => AdditiveGroup (Equal a) where
  zeroV   = 0
  (^+^)   = (+)
  negateV = negate

instance IsNat a => VectorSpace (Equal a) where
  type Scalar (Equal a) = Equal a
  (*^) = (*)

-- Convenience to avoid ScopedTypeVariables etc    
getSize :: IsNat a => Equal a -> Nat a
getSize _ = nat 

{-| Size of this type (value not evaluated).

>>> size (undefined :: Equal N2)
2

>>> size (undefined :: Equal N12)
12
-}
size :: IsNat a => Equal a -> Int
size = natToZ . getSize

-- TODO I got this part wrong
-- 
-- This type implements limited values (useful for interval *steps*)
-- An ET-interval is just an int, with a type-level size (divMod is "separate")

-- -- | Create an equal-temperament value.
-- toEqual :: IsNat a => Int -> Maybe (Equal a)
-- toEqual = checkSize . Equal
-- 
-- -- | Unsafely create an equal-temperament value.
-- unsafeToEqual :: IsNat a => Int -> Equal a
-- unsafeToEqual n = case toEqual n of
--   Nothing -> error $ "Bad equal: " ++ show n
--   Just x  -> x
-- 
-- checkSize :: IsNat a => Equal a -> Maybe (Equal a)
-- checkSize x = if 0 <= fromEqual x && fromEqual x < size x then Just x else Nothing
-- 

-- | Create an equal-temperament value.
toEqual :: IsNat a => Int -> Equal a
toEqual = Equal

-- | Extract an equal-temperament value.
fromEqual :: IsNat a => Equal a -> Int
fromEqual = getEqual

{-| Convert an equal-temeperament value to a frequency ratio.

>>> equalToRatio (7 :: Equal12)
1.4983070768766815

>>> equalToRatio (4 :: Equal12)
1.2599210498948732

-}
equalToRatio :: IsNat a => Equal a -> Double
equalToRatio x = 2**(realToFrac (fromEqual x) / realToFrac (size x))

{-| Safely cast a tempered value to another size.

>>> cast (1 :: Equal12) :: Equal24
2 :: Equal24

>>> cast (8 :: Equal12) :: Equal6
4 :: Equal6

>>> (2 :: Equal12) + cast (2 :: Equal24)
3 :: Equal12
-}
cast :: (IsNat a, IsNat b) => Equal a -> Equal b
cast = cast' undefined

cast' :: (IsNat a, IsNat b) => Equal b -> Equal a -> Equal b
cast' bDummy aDummy@(Equal a) = Equal $ (a * size bDummy) `div` size aDummy

type Equal6  = Equal N6
type Equal12 = Equal N12
type Equal17 = Equal N17
type Equal24 = Equal N24
type Equal36 = Equal N36

type N20 = N10 :*: N2
type N30 = N10 :*: N3
type N17 = N10 :+: N7
type N24 = N20 :+: N4
type N36 = N30 :+: N6

