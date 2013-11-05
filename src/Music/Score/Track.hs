
{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveDataTypeable, 
    DeriveTraversable, GeneralizedNewtypeDeriving, 
    FlexibleInstances,
    MultiParamTypeClasses #-}

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
import Control.Newtype
import Control.Applicative
import Control.Monad
import Control.Monad.Compose
import Control.Arrow

import Data.PairMonad ()
import Data.Typeable
import Data.Foldable (Foldable(..), foldMap)
import Data.Traversable (Traversable(..))
import Data.Pointed
import Data.VectorSpace hiding (Sum)
import Data.AffineSpace.Point
import Test.QuickCheck (Arbitrary(..), Gen(..))
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as List

import Music.Time
import Music.Pitch.Literal
import Music.Dynamics.Literal   
import Music.Score.Pitch
import Music.Score.Util


                                
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
newtype Track a = Track { getTrack' :: [Occ a] }
    deriving (Eq, Ord, Show, Functor, Foldable, Typeable, Traversable, Monoid, Semigroup, Delayable, Stretchable)

inTrack f = Track . f . getTrack'

type instance Event (Track a) = a

track :: Real d => [(Point d, a)] -> Track a
track = Track . fmap (uncurry occ . first (fmap realToFrac))

getTrack :: Fractional d => Track a -> [(Point d, a)]
getTrack = fmap (first (fmap realToFrac) . getOcc) . getTrack'

{-
instance Semigroup (Track a) where
    (<>) = mappend
-}

{-
-- Equivalent to the derived Monoid, except for the sorted invariant.
instance Monoid (Track a) where
    mempty = Track []
    Track as `mappend` Track bs = Track (as `m` bs)
        where
            m = mergeBy (comparing fst)
-}

instance Newtype (Track a) [Occ a] where
    pack = Track
    unpack = getTrack'

instance Monad Track where
    return = pack . return . return
    xs >>= f = pack $ mbind (unpack . f) (unpack xs)

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
    onset (Track a) = list origin (onset . head) a

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

instance HasPitch a => HasPitch (Track a) where
    type Pitch (Track a) = Pitch a
    getPitches as    = F.foldMap getPitches as
    modifyPitch f    = fmap (modifyPitch f)



newtype Occ a = Occ (Sum Time, a)
    deriving (Eq, Ord, Show, {-Read, -}Functor, Applicative, Monad, Foldable, Traversable)

occ t x = Occ (Sum t, x)
getOcc (Occ (Sum t, x)) = (t, x)

instance Delayable (Occ a) where
    delay n (Occ (s,x)) = Occ (delay n s, x)
instance Stretchable (Occ a) where
    stretch n (Occ (s,x)) = Occ (stretch n s, x)
instance HasOnset (Occ a) where
    onset (Occ (s,x)) = onset s

