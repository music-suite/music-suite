                              
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
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
-- Provides pitch manipulation.
--
-------------------------------------------------------------------------------------


module Music.Score.Pitch (
        HasPitch(..),
        PitchT(..),
        getPitches,
        setPitches,
        modifyPitches,
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

import Music.Score.Voice
import Music.Score.Score
import Music.Time.Relative
import Music.Time.Absolute
import Music.Score.Ties

class HasPitch a where
    -- | 
    -- Associated pitch type. Should implement 'Eq' and 'Show' to be usable.
    -- 
    type Pitch a :: *

    -- |
    -- Get the pitch of the given note.
    -- 
    getPitch :: a -> Pitch a

    -- |
    -- Set the pitch of the given note.
    -- 
    setPitch :: Pitch a -> a -> a

    -- |
    -- Modify the pitch of the given note.
    -- 
    modifyPitch :: (Pitch a -> Pitch a) -> a -> a
   
    setPitch n = modifyPitch (const n)
    modifyPitch f x = x


newtype PitchT p a = PitchT { getPitchT :: (p, a) }
    deriving (Eq, Ord, Show, Functor)

instance HasPitch (PitchT p a) where
    type Pitch (PitchT p a) = p
    getPitch (PitchT (v,_))      = v
    modifyPitch f (PitchT (v,x)) = PitchT (f v, x)

instance HasPitch ()                            where   { type Pitch ()         = ()        ; getPitch = id; modifyPitch = id }
instance HasPitch Double                        where   { type Pitch Double     = Double    ; getPitch = id; modifyPitch = id }
instance HasPitch Float                         where   { type Pitch Float      = Float     ; getPitch = id; modifyPitch = id }
instance HasPitch Int                           where   { type Pitch Int        = Int       ; getPitch = id; modifyPitch = id }
instance HasPitch Integer                       where   { type Pitch Integer    = Integer   ; getPitch = id; modifyPitch = id }
instance Integral a => HasPitch (Ratio a)       where   { type Pitch (Ratio a)  = (Ratio a) ; getPitch = id; modifyPitch = id }

-- |
-- Get all pitches in the given score. Returns a list of pitches.
--
-- > Score a -> [Pitch]
--
getPitches :: (HasPitch a, Eq v, v ~ Pitch a, Foldable s) => s a -> [Pitch a]
getPitches = List.nub . fmap getPitch . toList

-- |
-- Set all pitches in the given score.
--
-- > Pitch -> Score a -> Score a
--
setPitches :: (HasPitch a, Functor s) => Pitch a -> s a -> s a
setPitches n = fmap (setPitch n)

-- |
-- Modify all pitches in the given score.
--
-- > (Pitch -> Pitch) -> Score a -> Score a
--
modifyPitches :: (HasPitch a, Functor s) => (Pitch a -> Pitch a) -> s a -> s a
modifyPitches n = fmap (modifyPitch n)

