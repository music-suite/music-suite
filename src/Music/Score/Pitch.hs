                              
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
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
-- Provides a musical score represenation.
--
-------------------------------------------------------------------------------------


module Music.Score.Pitch (
        HasPitch(..),
        PitchT(..),
  ) where

import Data.Ratio
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace

import Music.Score.Part
import Music.Score.Score
import Music.Score.Duration
import Music.Score.Time

class HasPitch a where
    -- | 
    -- Associated voice type. Should implement 'Eq' and 'Show' to be usable.
    -- 
    type Pitch a :: *

    -- |
    -- Get the voice of the given note.
    -- 
    getPitch :: a -> Pitch a

    -- |
    -- Set the voice of the given note.
    -- 
    setPitch :: Pitch a -> a -> a

    -- |
    -- Modify the voice of the given note.
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

