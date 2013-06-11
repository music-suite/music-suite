
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
        Time,
        fromTime,
        toTime
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
newtype Time = Time { getTime :: Rational }
    deriving (Eq, Ord, Num, Enum, Real, Fractional, RealFrac)

instance Show Time where 
    show = show . getTime

instance AdditiveGroup Time where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance VectorSpace Time where
    type Scalar Time = Time
    (*^) = (*)

instance InnerSpace Time where 
    (<.>) = (*)

instance  AffineSpace Time where
    type Diff Time = Duration
    a .-. b =  fromTime $ a - b
    a .+^ b =  a + fromDuration b

fromTime :: Fractional a => Time -> a
fromTime = fromRational . getTime

toTime :: Real a => a -> Time
toTime = Time . toRational

