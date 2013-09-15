
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
        -- getPitches,
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
-- import Data.Foldable
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
    -- Get the pitches of the given note (TODO should be set?)getPitches
    --
    getPitches :: a -> [PitchOf a]

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
    getPitches (PitchT (v,_))    = [v]
    modifyPitch f (PitchT (v,x)) = PitchT (f v, x)

instance HasPitch ()                            where   { type PitchOf ()         = ()        ; getPitches = return; modifyPitch = id }
instance HasPitch Double                        where   { type PitchOf Double     = Double    ; getPitches = return; modifyPitch = id }
instance HasPitch Float                         where   { type PitchOf Float      = Float     ; getPitches = return; modifyPitch = id }
instance HasPitch Int                           where   { type PitchOf Int        = Int       ; getPitches = return; modifyPitch = id }
instance HasPitch Integer                       where   { type PitchOf Integer    = Integer   ; getPitches = return; modifyPitch = id }
instance Integral a => HasPitch (Ratio a)       where   { type PitchOf (Ratio a)  = (Ratio a) ; getPitches = return; modifyPitch = id }

instance HasPitch a => HasPitch (a,b) where
    type PitchOf (a,b)  = PitchOf a
    getPitches (a,b)    = getPitches a
    modifyPitch f (a,b) = (modifyPitch f a,b)

instance HasPitch a => HasPitch [a] where
    type PitchOf [a] = PitchOf a
    getPitches []    = error "getPitch: Empty list"
    getPitches as    = concatMap getPitches as
    modifyPitch f    = fmap (modifyPitch f)
    
{-
-- |
-- Get all pitches in the given score. Returns a set of pitches. (TODO use set type?)
--
-- > Score a -> [Pitch]
--
getPitches :: (HasPitch a, Eq v, v ~ PitchOf a, Foldable f, p ~ PitchOf a) => f a -> [p]
getPitches = List.nub . fmap getPitch . toList
-}

-- |
-- Set all pitches in the given score.
--
-- > Pitch -> Score a -> Score a
--
setPitches :: (HasPitch a, p ~ PitchOf a) => p -> a -> a
setPitches n = setPitch n

-- |
-- Modify all pitches in the given score.
--
-- > (Pitch -> Pitch) -> Score a -> Score a
--
modifyPitches :: (HasPitch a, p ~ PitchOf a) => (p -> p) -> a -> a
modifyPitches f = modifyPitch f

-- |
-- Transpose up.
--
-- > Interval -> Score a -> Score a
--
up :: (HasPitch a, AffineSpace p, p ~ PitchOf a) => IntervalOf a -> a -> a
up a = modifyPitches (.+^ a)

-- |
-- Transpose down.
--
-- > Interval -> Score a -> Score a
--
down :: (HasPitch a, AffineSpace p, p ~ PitchOf a) => IntervalOf a -> a -> a
down a = modifyPitches (.-^ a)

-- |
-- Invert around the given pitch.
--
-- > Pitch -> Score a -> Score a
--
invertAround :: (AffineSpace (PitchOf a), HasPitch a) => PitchOf a -> a -> a
invertAround basePitch a = modifyPitches ((basePitch .+^) . negateV . (.-. basePitch)) a

-- |
-- Transpose up by the given number of octaves.
--
-- > Integer -> Score a -> Score a
--
octavesUp       :: (HasPitch' a, IsInterval (IntervalOf a)) => 
                Integer -> a -> a

-- |
-- Transpose down by the given number of octaves.
--
-- > Integer -> Score a -> Score a
--
octavesDown     :: (HasPitch' a, IsInterval (IntervalOf a)) => 
                Integer -> a -> a

octavesUp a     = up (_P8^*a)
octavesDown a   = down (_P8^*a)

