
{-# LANGUAGE
    TypeFamilies,
    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    DeriveFoldable,     
    ScopedTypeVariables,
    FlexibleInstances,
    NoMonomorphismRestriction #-} 

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides a musical score represenation.
--
-------------------------------------------------------------------------------------

module Music.Score.Time where

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

import Music.Score.Duration

-------------------------------------------------------------------------------------
-- Time type
-------------------------------------------------------------------------------------

-- |
-- This type represents absolute time. This means seconds elapsed since a known 
-- reference time. The reference time can be anything, but is usually the 
-- the beginning of the musical performance.
--
-- Times forms an affine space with durations as the underlying vector space.
--
newtype Time = Time { getTime::Rational }
    deriving (Eq, Ord, Num, Enum, Real, Fractional, RealFrac)
    -- Note: no Floating as we want to be able to switch to rational

instance Show Time where 
    show = show . getTime

instance AdditiveGroup Time where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance VectorSpace Time where
    -- FIXME shouldn't this be Duration?
    type Scalar Time = Time
    (*^) = (*)

instance InnerSpace Time where (<.>) = (*)

instance  AffineSpace Time where
    type Diff Time = Duration
    a .-. b =  t2d $ a - b      where t2d = Duration . getTime
    a .+^ b =  a + d2t b        where d2t = Time . getDuration

class HasOnset a where
    onset  :: a -> Time
    offset :: a -> Time
                              
                              

                 