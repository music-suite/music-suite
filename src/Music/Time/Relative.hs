                              
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleContexts,
    ConstraintKinds,
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

module Music.Time.Relative (
        Duration,
        fromDuration,
        toDuration,
        HasDuration(..),
        Delayable(..),
        Stretchable(..),
  ) where

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


-------------------------------------------------------------------------------------
-- Duration type
-------------------------------------------------------------------------------------

-- |
-- This type represents relative time in seconds.
--
newtype Duration = Duration { getDuration::Rational }                                  
    deriving (Eq, Ord, Num, Enum, Real, Fractional, RealFrac)

instance Show Duration where 
    show = show . getDuration

instance AdditiveGroup Duration where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance VectorSpace Duration where
    type Scalar Duration = Duration
    (*^) = (*)

instance InnerSpace Duration where (<.>) = (*)

class HasDuration a where
    duration :: a -> Duration

fromDuration :: Fractional a => Duration -> a
fromDuration = fromRational . getDuration

toDuration :: Real a => a -> Duration
toDuration = Duration . toRational


-- |
-- Delayable values. 
-- 
-- Similar to @(AffineSpace a, Diff a ~ Duration)@.
-- 
class Delayable a where

    -- |
    -- Delay a value.
    -- > Duration -> Score a -> Score a
    -- 
    delay :: Duration -> a -> a

-- |
-- Stretchable values.
-- 
type Stretchable a = (VectorSpace a, Scalar a ~ Duration)

