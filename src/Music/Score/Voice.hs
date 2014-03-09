
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

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
        voice',
        voice,

        zipVoice,
        zipVoiceWith,
        dzipVoiceWith,
        mergeEqual,
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Compose
import           Data.Semigroup

import           Data.Foldable          (Foldable (..), foldMap)
import qualified Data.Foldable          as F
import qualified Data.List              as List
import           Data.PairMonad         ()
import           Data.Traversable       (Traversable (..))
import qualified Data.Traversable       as T
import           Data.Typeable
import           Data.VectorSpace       hiding (Sum)

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score.Pitch
import           Music.Score.Util
import           Music.Time


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
-- > p >>= \x -> Voice [ (1, Just $ toEnum $ x+65),
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


instance Wrapped (Voice a) where
    type Unwrapped (Voice a) = [Ev a]
    _Wrapped' = iso getVoice' Voice

instance Applicative Voice where
    pure  = return
    (<*>) = ap

instance Monad Voice where
    return = (^. _Unwrapped') . return . return
    xs >>= f = (^. _Unwrapped') $ ((^. _Wrapped') . f) `mbind` ((^. _Wrapped') xs)

instance HasDuration (Voice a) where
    duration = sum . fmap duration . getVoice'

instance IsPitch a => IsPitch (Voice a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Voice a) where
    fromDynamics = pure . fromDynamics

instance IsInterval a => IsInterval (Voice a) where
    fromInterval = pure . fromInterval

-- TODO
instance Num a => Num (Voice a) where
    fromInteger = pure . fromInteger

type instance Pitch (Voice a) = Pitch a
instance (HasSetPitch a b, Transformable (Pitch a), Transformable (Pitch b)) => HasSetPitch (Voice a) (Voice b) where
    type SetPitch g (Voice a) = Voice (SetPitch g a)
    -- FIXME this is wrong, need to behave like __mapPitch'
    __mapPitch f   = fmap (__mapPitch f)

-- |
-- Create a voice from a list of events.
--
voice' :: Iso' [(Duration, a)] (Voice a)
voice' = voice

-- |
-- Create a voice from a list of events.
--
voice :: Iso [(Duration, a)] [(Duration, b)] (Voice a) (Voice b)
voice = iso mkVoice getVoice
    where
        mkVoice = Voice . fmap (uncurry ev . first realToFrac)
        getVoice = fmap (first realToFrac . getEv) . getVoice'

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice :: Voice a -> Voice b -> Voice (a, b)
zipVoice = zipVoiceWith (,)

-- |
-- Join the given voices by multiplying durations and combining values using the given function.
--
zipVoiceWith :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith f (Voice a) (Voice b) = Voice $ zipWith (\(Ev (dx,vx)) (Ev (dy,vy)) -> Ev (dx <> dy, f vx vy)) a b

-- |
-- Join the given voices by combining durations and values using the given function.
--
dzipVoiceWith :: (Duration -> Duration -> a -> b -> (Duration, c)) -> Voice a -> Voice b -> Voice c
dzipVoiceWith f (Voice a) (Voice b) = Voice $ zipWith (\(Ev (Product dx,vx)) (Ev (Product dy,vy)) -> Ev (first Product $ f dx dy vx vy)) a b

-- |
-- Merge consecutive equal note.
--
mergeEqual :: Eq a => Voice a -> Voice a
mergeEqual = over (from voice) $ fmap f . List.groupBy (inspecting snd)
    where
        f dsAs = let (ds,as) = unzip dsAs in (sum ds, head as)

inspecting :: Eq a => (b -> a) -> b -> b -> Bool
inspecting p x y = p x == p y

newtype Ev a = Ev (Product Duration, a)
    deriving (Eq, Ord, Show, {-Read, -}Functor, Applicative, Monad, Foldable, Traversable)

ev t x = Ev (Product t, x)
getEv (Ev (Product t, x)) = (t, x)

instance Stretchable (Ev a) where
    stretch n (Ev (s,x)) = Ev (stretch n s, x)

instance HasDuration (Ev a) where
    duration (Ev (s,x)) = duration s


