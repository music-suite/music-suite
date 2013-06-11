
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

module Music.Time.TimeRep (
        TimeRep(..),
  ) where

import Music.Time.Time
import Music.Time.Duration
import Music.Time.Delayable
import Music.Time.Stretchable

-- |
-- This type function returns the time type for a given type.
--
type family TimeRep a :: *

type instance TimeRep Double    = Double
type instance TimeRep Rational  = Rational

type instance TimeRep (a -> b)   = TimeRep b
type instance TimeRep [a]        = TimeRep a
type instance TimeRep (Maybe a)  = TimeRep a

-- type instance TimeRep (Option a) = TimeRep a
-- type instance TimeRep (Set a)    = TimeRep a
-- type instance TimeRep (Map k a)  = TimeRep a