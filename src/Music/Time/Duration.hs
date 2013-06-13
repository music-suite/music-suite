                              
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

module Music.Time.Duration (
        DurationT,
        fromDurationT,
        toDurationT,
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace


-------------------------------------------------------------------------------------
-- Duration type
-------------------------------------------------------------------------------------

-- |
-- This type represents relative time in seconds.
--
newtype DurationT = DurationT { getDurationT::Rational }                                  
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

