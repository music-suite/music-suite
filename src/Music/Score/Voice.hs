
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
-- Provides the 'Voice' type.
--
-------------------------------------------------------------------------------------

module Music.Score.Voice (
        -- * Voice type
        Voice,
        voice,
        getVoice,
  ) where

import Data.Semigroup
import Control.Applicative
import Control.Monad            (ap, join, MonadPlus(..))

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

-- |
-- A voice is a list of events with explicit duration. Events can not overlap.
--
-- Voice is a 'Monoid' under sequential composition. 'mempty' is the empty part and 'mappend'
-- appends parts.
--
-- Voice is a 'Monad'. 'return' creates a part containing a single value of duration
-- one, and '>>=' transforms the values of a part, allowing the addition and
-- removal of values under relative duration. Perhaps more intuitively, 'join' scales
-- each inner part to the duration of the outer part, then removes the
-- intermediate structure.
--
-- > let p = Voice [(1, Just 0), (2, Just 1)] :: Voice Int
-- >
-- > p >>= \x -> Voice [ (1, Just $ toEnum $ x+65),
-- >                    (3, Just $ toEnum $ x+97) ] :: Voice Char
-- >
-- >     ===> Voice {getVoice = [ (1 % 1,Just 'A'),
-- >                            (3 % 1,Just 'a'),
-- >                            (2 % 1,Just 'B'),
-- >                            (6 % 1,Just 'b') ]}
--
-- Voice is a 'VectorSpace' using sequential composition as addition, and time scaling
-- as scalar multiplication.
--
newtype Voice a = Voice { getVoice' :: [(Duration, a)] }
    deriving (Eq, Ord, Show, Functor, Foldable, Monoid, Typeable, Traversable)

voice :: Real d => [(d, a)] -> Voice a
voice = Voice . fmap (first realToFrac)

getVoice :: Fractional d => Voice a -> [(d, a)]
getVoice = fmap (first realToFrac) . getVoice'

-- type instance Duration (Voice a) = DurationT
type instance Event (Voice a) = a

instance Semigroup (Voice a) where
    (<>) = mappend

instance Monad Voice where
    return a = Voice [(1, a)]
    a >>= k = (join' . fmap k) a
        where
            join' (Voice ps) = foldMap (uncurry stretch) ps

instance Pointed Voice where
    point = return

instance Applicative Voice where
    pure  = return
    (<*>) = ap

instance Stretchable (Voice a) where
    n `stretch` Voice as = Voice (fmap (first (n*^)) as)

instance HasDuration (Voice a) where
    duration (Voice as) = sum (fmap fst as)

instance IsPitch a => IsPitch (Voice a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Voice a) where
    fromDynamics = pure . fromDynamics

    
-------------------------------------------------------------------------------------

list z f [] = z
list z f xs = f xs

first f (x,y)  = (f x, y)
second f (x,y) = (x, f y)

