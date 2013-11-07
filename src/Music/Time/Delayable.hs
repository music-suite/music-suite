
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleInstances,
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
-- Provides delayable values.
--
-------------------------------------------------------------------------------------

module Music.Time.Delayable (
        -- * Delayable class
        Delayable(..),
        move,
        moveBack,     
        
        -- ** Utility functions
        delayTime,
  ) where

import Data.Semigroup
import Data.VectorSpace hiding (Sum)
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Music.Time.Time

-- |
-- Delayable values. 
-- 
class Delayable a where

    -- |
    -- Delay a value.
    -- 
    delay :: Duration -> a -> a

instance Delayable Time where
    delay n = (.+^ n)

instance Delayable (Time -> a) where
    delay n = (. delay (negateV n))

instance Delayable a => Delayable [a] where
    delay n = fmap (delay n)

instance Delayable a => Delayable (Map k a) where
    delay n = fmap (delay n)

instance Delayable (Time, a) where
    delay n (t, a) = (n `delay` t, a)

instance Delayable (Time, Duration, a) where
    delay n (t, d, a) = (n `delay` t, d, a)

instance Delayable a => Delayable (Sum a) where
    delay n (Sum x) = Sum (delay n x)

instance Delayable a => Delayable (Product a) where
    delay n (Product x) = Product (delay n x)



-- |
-- Move a score forward in time. Equivalent to 'delay'.
--
move :: Delayable a => Duration -> a -> a

-- |
-- Move a score backward in time. Negated verison of 'delay'.
--
moveBack :: Delayable a => Duration -> a -> a

-- |
-- Delay relative to 'origin'. Provided for situations when you have a value that
-- should forward based on the distance between some time @t@ and the origin, but
-- it does not necessarily have a start time.
--
delayTime :: Delayable a => Time   -> a -> a

move            = delay
moveBack t      = delay (negateV t)
delayTime t     = delay (t .-. origin)

