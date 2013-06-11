
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
  ) where

import Music.Time.Time
import Music.Time.Duration
import Music.Time.Delayable
import Music.Time.Stretchable

-- |
-- This type function returns the time type for a given type.
--
type family Pos a :: *

type instance Pos Double    = Double
type instance Pos Rational  = Rational

type instance Pos (a -> b)   = Pos b
type instance Pos [a]        = Pos a
type instance Pos (Maybe a)  = Pos a

-- type instance Pos (Option a) = Pos a
-- type instance Pos (Set a)    = Pos a
-- type instance Pos (Map k a)  = Pos a