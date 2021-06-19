{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-unticked-promoted-constructors
  -fno-warn-redundant-constraints #-}

-- | Generic equal temperament pitch.
--
-- Use the type-level numbers to construct an temperement dividing
-- the octave in any number of equal-sized steps.
--
-- Common cases such as 6, 12 and 24 are provided for convenience.
module Music.Pitch.Equal
  ( -- * Equal temperament
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

import Data.Kind (Type)
import Control.Applicative
import Control.Monad
import Data.AffineSpace
import Data.Either
import Data.Maybe
import Data.Proxy
import Data.Semigroup
import Data.VectorSpace
import Music.Pitch.Absolute

data Nat = Zero | Succ Nat

type N1 = Succ Zero
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4
type N6 = Succ N5
type N7 = Succ N6
type N8 = Succ N7
type N9 = Succ N8
type N10 = Succ N9
type N11 = Succ N10
type N12 = Succ N11

type Equal6 = Equal N6

type Equal12 = Equal N12

type Equal17 = Equal N17

type Equal24 = Equal N24

type Equal36 = Equal N36

type (:+:) :: Nat -> Nat -> Nat
type family a :+: b
type instance Zero :+: b = b
type instance Succ a :+: b = Succ (a :+: b)

type (:*:) :: Nat -> Nat -> Nat
type family a :*: b
type instance Zero :*: b = Zero
type instance Succ a :*: b = b :+: (a :*: b)

type N20 = N10 :*: N2

type N30 = N10 :*: N3

type N17 = N10 :+: N7

type N24 = N20 :+: N4

type N36 = N30 :+: N6

type Equal :: Nat -> Type
newtype Equal a = Equal {getEqual :: Int}
class IsNat a where
  size :: proxy a -> Int
instance IsNat Zero where
  size _ = 0
instance IsNat a => IsNat (Succ a) where
  size p = size (pred p) + 1
    where
      pred :: p (Succ a) -> Proxy a
      pred _ = Proxy
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

  negate (Equal a) = Equal (negate a)

  abs (Equal a) = Equal (abs a)

  signum (Equal a) = Equal (signum a)

  fromInteger = toEqual . fromIntegral

instance IsNat a => Semigroup (Equal a) where
  (<>) = (+)

instance IsNat a => Monoid (Equal a) where
  mempty = 0

instance IsNat a => AdditiveGroup (Equal a) where
  zeroV = 0

  (^+^) = (+)

  negateV = negate

instance IsNat a => VectorSpace (Equal a) where
  type Scalar (Equal a) = Equal a

  (*^) = (*)

{-
getSize :: IsNat a => f a -> Nat a
getSize _ = nat

-}

-- |  Create an equal-temperament value.
toEqual :: IsNat a => Int -> Equal a
toEqual = Equal

-- | Extract an equal-temperament value.
fromEqual :: IsNat a => Equal a -> Int
fromEqual = getEqual

-- | Convert an equal-temeperament value to a frequency ratio.
--
-- >>> equalToRatio (7 :: Equal12)
-- 1.4983070768766815
--
-- >>> equalToRatio (4 :: Equal12)
-- 1.2599210498948732
equalToRatio :: IsNat a => Equal a -> Double
equalToRatio x = 2 ** (realToFrac (fromEqual x) / realToFrac (size x))

-- | Safely cast a tempered value to another size.
--
-- >>> cast (1 :: Equal12) :: Equal24
-- 2
--
-- >>> cast (8 :: Equal12) :: Equal6
-- 4
--
-- >>> (2 :: Equal12) + cast (2 :: Equal24)
-- 3
cast :: (IsNat a, IsNat b) => Equal a -> Equal b
cast = cast' Proxy

cast' :: (IsNat a, IsNat b) => proxy b -> Equal a -> Equal b
cast' p aDummy@(Equal a) = Equal $ (a * size p) `div` size aDummy

