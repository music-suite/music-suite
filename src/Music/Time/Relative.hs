                              
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

