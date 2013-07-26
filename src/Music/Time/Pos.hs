
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
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
-------------------------------------------------------------------------------------

module Music.Time.Pos (
        Time(..),
        Duration(..),

        DurationT,
        fromDurationT,
        toDurationT,

        TimeT,
        fromTimeT,
        toTimeT
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace


-- |
-- This type function returns the time type for a given type.
--
-- It has kind
--
-- > (* -> *) -> *
--
-- meaning that an instance should be written on the form:
--
-- > type instance Time a = b
--
-- where /a/ and /b/ are type-level expression of kind @* -> *@ and @*@ respectively.
--
type family Time (s :: * -> *) :: *

-- |
-- This type function returns the duration type for a given type.
--
type Duration a = Diff (Time a)


-------------------------------------------------------------------------------------
-- Duration type
-------------------------------------------------------------------------------------

-- |
-- This type represents relative time in seconds.
--
newtype DurationT = DurationT { getDurationT :: Rational }                                  
    deriving (Eq, Ord, Num, Enum, Real, Fractional, RealFrac)

instance Show DurationT where 
    show = show . getDurationT

instance AdditiveGroup DurationT where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance VectorSpace DurationT where
    type Scalar DurationT = DurationT
    (*^) = (*)

instance InnerSpace DurationT where (<.>) = (*)

fromDurationT :: Fractional a => DurationT -> a
fromDurationT = fromRational . getDurationT

toDurationT :: Real a => a -> DurationT
toDurationT = DurationT . toRational


-------------------------------------------------------------------------------------
-- Time type
-------------------------------------------------------------------------------------

-- |
-- This type represents absolute time in seconds since the start time. Note
-- that time can be negative, representing events occuring before the start time.
-- The start time is usually the the beginning of the musical performance. 
--
-- Time forms an affine space with durations as the underlying vector space,
-- that is, we can add a time to a duration to get a new time using '.+^', 
-- take the difference of two times to get a duration using '.-.'.
--
newtype TimeT = TimeT { getTimeT :: Rational }
    deriving (Eq, Ord, Num, Enum, Real, Fractional, RealFrac)

instance Show TimeT where 
    show = show . getTimeT

instance AdditiveGroup TimeT where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance VectorSpace TimeT where
    type Scalar TimeT = TimeT
    (*^) = (*)

instance InnerSpace TimeT where 
    (<.>) = (*)

instance  AffineSpace TimeT where
    type Diff TimeT = DurationT
    a .-. b =  fromTimeT $ a - b
    a .+^ b =  a + fromDurationT b

fromTimeT :: Fractional a => TimeT -> a
fromTimeT = fromRational . getTimeT

toTimeT :: Real a => a -> TimeT
toTimeT = TimeT . toRational

{-
    Actually, I would prefer:

        type TimeT = Point DurationT

        fromTimeT :: Fractional a => TimeT -> a
        fromTimeT = fromDurationT . unPoint

        toTimeT :: Real a => a -> TimeT
        toTimeT = P . toDurationT
-}                 