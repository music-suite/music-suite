
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

module Music.Time.Time (
        TimeT,
        fromTimeT,
        toTimeT
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace

import Music.Time.Duration

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