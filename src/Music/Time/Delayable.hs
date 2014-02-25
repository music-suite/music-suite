
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

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
        undelay,
        delaying,
        move,
        moveBack,

        -- ** Utility
        delayTime,
        NoDelay(..),
  ) where

import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.VectorSpace       hiding (Sum)

import           Music.Time.Time

-- |
-- Delayable values.
--
class Delayable a where

    -- |
    -- Delay a value.
    --
    delay :: Duration -> a -> a
    delay _ = id

instance Delayable Time where
    delay n = (.+^ n)

instance Delayable (Time -> a) where
    delay n = (. delay (negateV n))

instance Delayable a => Delayable [a] where
    delay n = fmap (delay n)

instance (Ord a, Delayable a) => Delayable (Set a) where
    delay n = Set.map (delay n)

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

undelay t       = delay (negateV t)
move            = delay
moveBack t      = delay (negateV t)
delayTime t     = delay (t .-. origin)


-- | Apply a function under delay.
--   See also 'sunder'.
delaying :: (Delayable a, Delayable b) => Duration -> (a -> b) -> a -> b
delaying t f = undelay t . f . delay t

newtype NoDelay a = NoDelay { getNoDelay :: a }
    deriving (Eq, Ord, Enum, Show, Semigroup, Monoid
        {-Delayable, HasOnset, HasOffset, HasDuration-})

instance Delayable (NoDelay a) where
    delay _ = id
