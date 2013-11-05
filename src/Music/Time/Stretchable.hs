
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleContexts,
    FlexibleInstances,
    ConstraintKinds,
    UndecidableInstances,
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
-- Provides stretchable values.
--
-------------------------------------------------------------------------------------

module Music.Time.Stretchable (
        -- * Stretchable class
        Stretchable(..),
        compress,
  ) where

import Control.Arrow

import Data.Semigroup
import Data.VectorSpace hiding (Sum)
import Data.AffineSpace
import Data.AffineSpace.Point

import Music.Time.Time
import Music.Time.Relative

-- |
-- Stretchable values. 
-- 
class Stretchable a where

    -- |
    -- Stretch (augment) a value by the given factor.
    -- 
    -- > Duration -> Score a -> Score a
    -- 
    stretch :: Duration -> a -> a

instance Stretchable Time where
    stretch n = (n*.)

instance Stretchable Duration where
    stretch n = (n*^)

instance Stretchable (Time, a) where
    stretch n (t, a) = (n `stretch` t, a)

instance Stretchable (Duration, a) where
    stretch n (d, a) = (n `stretch` d, a)

instance Stretchable (Time, Duration, a) where
    stretch n (t, d, a) = (n `stretch` t, n `stretch` d, a)

instance Stretchable (Time -> a) where
    stretch n = (. relative origin (^/ n))

instance Stretchable (Duration -> a) where
    stretch n = (. (^/ n))

instance Stretchable a => Stretchable [a] where
    stretch n = fmap (stretch n)

instance Stretchable a => Stretchable (Product a) where
    stretch n (Product x) = Product (stretch n x)

instance Stretchable a => Stretchable (Sum a) where
    stretch n (Sum x) = Sum (stretch n x)


-- |
-- Compress (diminish) a score. Flipped version of 'stretch'.
--
-- > Duration -> Score a -> Score a
--
compress        :: (Stretchable a, Fractional d, d ~ Duration) =>
                d -> a -> a

compress x      = stretch (recip x)

