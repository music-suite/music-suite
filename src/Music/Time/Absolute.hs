
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
-- Provides a musical score represenation.
--
-------------------------------------------------------------------------------------

module Music.Time.Absolute where

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
import Control.Applicative
import Data.Traversable
import Data.Maybe
import Data.Either
import Data.Function (on)
import Data.Ord (comparing)
import Data.Ratio
import Data.VectorSpace
import Data.AffineSpace

import Music.Time.Relative

-------------------------------------------------------------------------------------
-- Time type
-------------------------------------------------------------------------------------

-- |
-- This type represents absolute time in seconds a known reference time.
-- The reference time can be any time, but is usually the the beginning of the 
-- musical performance.
--
-- Times forms an affine space with durations as the underlying vector space,
-- that is, we can add a time to a duration to get a new time using '.+^', 
-- take the difference of two times to get a duration using '.-.'.
--
newtype Time = Time { getTime::Rational }
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

instance InnerSpace Time where (<.>) = (*)

instance  AffineSpace Time where
    type Diff Time = Duration
    a .-. b =  t2d $ a - b      where t2d = Duration . getTime
    a .+^ b =  a + d2t b        where d2t = Time . getDuration

fromTime :: Fractional a => Time -> a
fromTime = fromRational . getTime

toTime :: Real a => a -> Time
toTime = Time . toRational

-- |
-- Class of types with a position in time.
--
-- Onset and offset are logical start and stop time, i.e. the preferred beginning and end of 
-- the sound., not o the the time of the attack and damp actions on an instrument,
--
-- If a type has an instance for both 'HasOnset' and 'HasDuration', the following laws
-- should hold:
-- 
-- > duration a = offset a - onset a
-- > offset a >= onset a
--
-- implying
--
-- > duration a >= 0
--
class HasOnset a where
    -- | 
    -- Get the onset of the given value.
    --
    onset  :: a -> Time

    -- | 
    -- Get the offset of the given value.
    --
    offset :: a -> Time
                              
instance HasOnset a => HasOnset (WrappedMonoid a) where 
    onset = onset . unwrapMonoid
    offset = offset . unwrapMonoid

class HasPreOnset a where
    preOnset :: a -> Time

class HasPostOnset a where
    postOnset :: a -> Time

class HasPostOffset a where
    postOffset :: a -> Time

