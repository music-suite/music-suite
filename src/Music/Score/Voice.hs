
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
newtype Voice a = Voice { getVoice' :: [Ev a] }
    deriving (Eq, Ord, Show, Functor, Foldable, Monoid, Semigroup, Typeable, Traversable, Stretchable)

inVoice f = Voice . f . getVoice'

voice :: Real d => [(d, a)] -> Voice a
voice = Voice . fmap (uncurry ev . first realToFrac)

getVoice :: Fractional d => Voice a -> [(d, a)]
getVoice = fmap (first realToFrac . getEv) . getVoice'

type instance Event (Voice a) = a

instance Newtype (Voice a) [Ev a] where
    pack = Voice
    unpack = getVoice'

instance Monad Voice where
    return = pack . return . return
    xs >>= f = pack $ mbind (unpack . f) (unpack xs)

instance Pointed Voice where
    point = return

instance Applicative Voice where
    pure  = return
    (<*>) = ap

instance HasDuration (Voice a) where
    duration = sum . fmap duration . getVoice'

instance IsPitch a => IsPitch (Voice a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Voice a) where
    fromDynamics = pure . fromDynamics

instance HasPitch a => HasPitch (Voice a) where
    type Pitch (Voice a) = Pitch a
    getPitches as    = F.foldMap getPitches as
    modifyPitch f    = fmap (modifyPitch f)



newtype Ev a = Ev (Product Duration, a)
    deriving (Eq, Ord, Show, {-Read, -}Functor, Applicative, Monad, Foldable, Traversable)

ev t x = Ev (Product t, x)
getEv (Ev (Product t, x)) = (t, x)

instance Stretchable (Ev a) where
    stretch n (Ev (s,x)) = Ev (stretch n s, x)

instance HasDuration (Ev a) where
    duration (Ev (s,x)) = duration s


