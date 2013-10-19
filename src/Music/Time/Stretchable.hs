
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

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point

import Music.Time.Time

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

instance Stretchable a => Stretchable [a] where
    stretch n = fmap (stretch n)

-- instance (d ~ Scalar d, t ~ Point d, VectorSpace d) => Stretchable (t, d, a) where
--     stretch n (t, d, a) = (n*.t, n*^d, a)
instance Stretchable (Time, Duration, a) where
    stretch n (t, d, a) = (n*.t, n*^d, a)

instance Stretchable (Duration -> a) where
    -- stretch n = flip (.) (^/ n)
    stretch n f = f . (^/ n)

instance Stretchable (Time -> a) where
    stretch n f = f . relative origin (^/ n)

relative :: AffineSpace p => p -> (Diff p -> Diff p) -> p -> p
relative p f = (p .+^) . f . (.-. p)

-- |
-- Compress (diminish) a score. Flipped version of 'stretch'.
--
-- > Duration -> Score a -> Score a
--
compress        :: (Stretchable a, Fractional d, d ~ Duration) =>
                d -> a -> a

compress x      = stretch (recip x)

