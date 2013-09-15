
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
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
-- Provides functions for manipulating pitch.
--
-------------------------------------------------------------------------------------


module Music.Score.Pitch (     
        -- * Pitch representation
        HasPitch(..),
        IntervalOf,  
        HasPitch',
        PitchT(..),
        getPitches,
        setPitches,
        modifyPitches,

        -- * Pitch transformations
        -- ** Transposition
        up,
        down,
        invertAround,
        octavesUp,
        octavesDown,
  ) where

import Control.Monad (ap, mfilter, join, liftM, MonadPlus(..))
import Data.String
import Data.Foldable
import Data.Typeable
import Data.Traversable
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace
import Data.Ratio

import Music.Pitch.Literal

class HasPitch a where
    -- |
    -- Associated pitch type. Should implement 'Eq' and 'Show' to be usable.
    --
    type PitchOf a :: *

    -- |
    -- Get the pitch of the given note.
    --
    getPitch :: a -> PitchOf a

    -- |
    -- Set the pitch of the given note.
    --
    setPitch :: PitchOf a -> a -> a

    -- |
    -- Modify the pitch of the given note.
    --
    modifyPitch :: (PitchOf a -> PitchOf a) -> a -> a

    setPitch n = modifyPitch (const n)
    modifyPitch f x = x

type IntervalOf a = Diff (PitchOf a)

type HasPitch' a = (
    HasPitch a, 
    VectorSpace (IntervalOf a), Integer ~ Scalar (IntervalOf a),
    AffineSpace (PitchOf a)
    )

newtype PitchT p a = PitchT { getPitchT :: (p, a) }
    deriving (Eq, Ord, Show, Functor)

instance HasPitch (PitchT p a) where
    type PitchOf (PitchT p a) = p
    getPitch (PitchT (v,_))      = v
    modifyPitch f (PitchT (v,x)) = PitchT (f v, x)

instance HasPitch ()                            where   { type PitchOf ()         = ()        ; getPitch = id; modifyPitch = id }
instance HasPitch Double                        where   { type PitchOf Double     = Double    ; getPitch = id; modifyPitch = id }
instance HasPitch Float                         where   { type PitchOf Float      = Float     ; getPitch = id; modifyPitch = id }
instance HasPitch Int                           where   { type PitchOf Int        = Int       ; getPitch = id; modifyPitch = id }
instance HasPitch Integer                       where   { type PitchOf Integer    = Integer   ; getPitch = id; modifyPitch = id }
instance Integral a => HasPitch (Ratio a)       where   { type PitchOf (Ratio a)  = (Ratio a) ; getPitch = id; modifyPitch = id }

instance HasPitch a => HasPitch [a] where
    type PitchOf [a] = PitchOf a
    getPitch []      = error "getPitch: Empty list"
    getPitch as      = getPitch (head as)
    modifyPitch f    = fmap (modifyPitch f)

-- |
-- Get all pitches in the given score. Returns a set of pitches. (TODO use set type?)
--
-- > Score a -> [Pitch]
--
getPitches :: (HasPitch a, Eq v, v ~ PitchOf a, Foldable f, p ~ PitchOf a) => f a -> [p]
getPitches = List.nub . fmap getPitch . toList

-- |
-- Set all pitches in the given score.
--
-- > Pitch -> Score a -> Score a
--
setPitches :: (HasPitch a, Functor s, p ~ PitchOf a) => p -> s a -> s a
setPitches n = fmap (setPitch n)

-- |
-- Modify all pitches in the given score.
--
-- > (Pitch -> Pitch) -> Score a -> Score a
--
modifyPitches :: (HasPitch a, Functor s, p ~ PitchOf a) => (p -> p) -> s a -> s a
modifyPitches f = fmap (modifyPitch f)

-- |
-- Transpose up.
--
-- > Interval -> Score a -> Score a
--
up :: (HasPitch a, Functor s, AffineSpace p, p ~ PitchOf a) => IntervalOf a -> s a -> s a
up a = modifyPitches (.+^ a)

-- |
-- Transpose down.
--
-- > Interval -> Score a -> Score a
--
down :: (HasPitch a, Functor s, AffineSpace p, p ~ PitchOf a) => IntervalOf a -> s a -> s a
down a = modifyPitches (.-^ a)

-- |
-- Invert around the given pitch.
--
-- > Pitch -> Score a -> Score a
--
invertAround :: (AffineSpace (PitchOf a), HasPitch a, Functor s) => PitchOf a -> s a -> s a
invertAround basePitch a = modifyPitches ((basePitch .+^) . negateV . (.-. basePitch)) a

-- |
-- Transpose up by the given number of octaves.
--
-- > Integer -> Score a -> Score a
--
octavesUp       :: (HasPitch' a, IsInterval (IntervalOf a), Functor s) => 
                Integer -> s a -> s a

-- |
-- Transpose down by the given number of octaves.
--
-- > Integer -> Score a -> Score a
--
octavesDown     :: (HasPitch' a, IsInterval (IntervalOf a), Functor s) => 
                Integer -> s a -> s a

octavesUp a     = up (_P8^*a)
octavesDown a   = down (_P8^*a)

