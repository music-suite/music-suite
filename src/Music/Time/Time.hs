
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

module Music.Time.Time (
        Time(..),
        Duration(..),
        Event(..),

        DurationT(..),
        TimeT(..),
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point

type family Duration (s :: *) :: *
type family Event (s :: *) :: *
type Time a = Point (Duration a)

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

-- |
-- This type function returns the duration type for a given type.
--

type DurationT = Rational

type TimeT = Point DurationT


-------------------------------------------------------------------------------------
-- Duration type
-------------------------------------------------------------------------------------

-- |
-- This type represents relative time in seconds.
--



-------------------------------------------------------------------------------------
-- Time type
-------------------------------------------------------------------------------------

-- |
-- This type represents absolute time in seconds since the start time. Note
-- that time can be negative, representing events occuring before the start time.
-- The start time is usually the the beginning of the musical performance. 
--
-- Time forms an affine space with durations as the underlying vector space,
-- that is, we can add a time to a duration to get a new time using '.+^', 
-- take the difference of two times to get a duration using '.-.'.
--

{-
    Actually, I would prefer:

        type TimeT = Point DurationT

        fromTimeT :: Fractional a => TimeT -> a
        fromTimeT = fromDurationT . unPoint

        toTimeT :: Real a => a -> TimeT
        toTimeT = P . toDurationT
-}                 