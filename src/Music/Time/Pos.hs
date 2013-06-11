
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
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
-------------------------------------------------------------------------------------

module Music.Time.Pos (
        Pos(..),
        Dur(..),
  ) where

import Data.AffineSpace
import Music.Time.Time
import Music.Time.Duration

-- |
-- This type function returns the time type for a given type.
--
type family Pos a :: *

-- |
-- This type function returns the duration type for a given type.
--
type Dur a = Diff (Pos a)

type instance Pos Double     = Double
type instance Pos Rational   = Rational
type instance Pos (a -> b)   = Pos b
type instance Pos [a]        = Pos a
type instance Pos (Maybe a)  = Pos a

-- TODO move
type instance Pos Time       = Time
type instance Pos Duration   = Duration


-- type instance Pos (Option a) = Pos a
-- type instance Pos (Set a)    = Pos a
-- type instance Pos (Map k a)  = Pos a