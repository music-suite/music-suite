
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
        Time(..),
        Duration(..),
  ) where

import Data.AffineSpace
import Music.Time.Time
import Music.Time.Duration

-- |
-- This type function returns the time type for a given type.
--
-- It has kind
--
-- > (* -> *) -> *
--
-- meaning that an instance should be written on the form:
--
-- > type instance Time a = b
--
-- where /a/ and /b/ are type-level expression of kind @* -> *@ and @*@ respectively.
--
type family Time (s :: * -> *) :: *

-- |
-- This type function returns the duration type for a given type.
--
type Duration a = Diff (Time a)

{-

type instance Time Double     = Double
type instance Time Rational   = Rational
type instance Time (a -> b)   = Time b
type instance Time [a]        = Time a
type instance Time (Maybe a)  = Time a

-- TODO move
type instance Time TimeT       = TimeT
type instance Time DurationT   = DurationT
-}


-- type instance Pos (Option a) = Pos a
-- type instance Pos (Set a)    = Pos a
-- type instance Pos (Map k a)  = Pos a





