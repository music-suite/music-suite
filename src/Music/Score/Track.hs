
{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveDataTypeable, 
    DeriveTraversable, GeneralizedNewtypeDeriving #-}

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
-- Provides the 'Track' type.
--
-------------------------------------------------------------------------------------

module Music.Score.Track (
        -- * Track type
        Track,
        track,
        getTrack,
  ) where

import Data.Semigroup
import Control.Applicative
import Control.Monad            (ap, join, MonadPlus(..))
import Control.Arrow

import Data.Typeable
import Data.Foldable            (Foldable(..), foldMap)
import Data.Traversable         (Traversable(..))
import Data.Pointed
import Data.Ord                 (comparing)
import Data.Function            (on)
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Test.QuickCheck          (Arbitrary(..), Gen(..))

import Music.Time
import Music.Pitch.Literal
import Music.Dynamics.Literal   
import Music.Score.Util

import qualified Data.List as List

-- |
-- A track is a list of events with explicit onset. 
--
-- Track is a 'Monoid' under parallel composition. 'mempty' is the empty track
-- and 'mappend' interleaves values.
--
-- Track is a 'Monad'. 'return' creates a track containing a single value at time
-- zero, and '>>=' transforms the values of a track, allowing the addition and
-- removal of values relative to the time of the value. Perhaps more intuitively,
-- 'join' delays each inner track to start at the offset of an outer track, then
-- removes the intermediate structure.
--
-- > let t = Track [(0, 65),(1, 66)]
-- >
-- > t >>= \x -> Track [(0, 'a'), (10, toEnum x)]
-- >
-- >   ==> Track {getTrack = [ (0.0,  'a'),
-- >                           (1.0,  'a'),
-- >                           (10.0, 'A'),
-- >                           (11.0, 'B') ]}
--
-- Track is an instance of 'VectorSpace' using parallel composition as addition,
-- and time scaling as scalar multiplication.
--
newtype Track a = Track { getTrack' :: [(Time, a)] }
    deriving (Eq, Ord, Show, Functor, Foldable, Typeable, Traversable, Delayable, Stretchable)

inTrack f = Track . f . getTrack'

type instance Event (Track a) = a

track :: Real d => [(Point d, a)] -> Track a
track = Track . fmap (first $ fmap realToFrac)

getTrack :: Fractional d => Track a -> [(Point d, a)]
getTrack = fmap (first $ fmap realToFrac) . getTrack'

instance Semigroup (Track a) where
    (<>) = mappend

-- Equivalent to the derived Monoid, except for the sorted invariant.
instance Monoid (Track a) where
    mempty = Track []
    Track as `mappend` Track bs = Track (as `m` bs)
        where
            m = mergeBy (comparing fst)

-- TODO mcompose
instance Monad Track where
    return a = Track [(origin, a)]
    a >>= k = (join' . fmap k) a
        where
            join' (Track ts) = foldMap (uncurry delayTime) ts

instance Applicative Track where
    pure  = return
    (<*>) = ap

instance Alternative Track where
    empty = mempty
    (<|>) = mappend

-- Satisfies left distribution
instance MonadPlus Track where
    mzero = mempty
    mplus = mappend

instance HasOnset (Track a) where
    onset (Track a) = list origin (on . head) a where on (t,x) = t

instance IsPitch a => IsPitch (Track a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Track a) where
    fromDynamics = pure . fromDynamics

instance Arbitrary a => Arbitrary (Track a) where
    arbitrary = do
        x <- arbitrary
        t <- fmap realToFrac (arbitrary::Gen Double)
        d <- fmap realToFrac (arbitrary::Gen Double)
        return $ delay t $ stretch d $ return x

