
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund, Edward Lilley 2012–2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Pitch.Scale (
  Scale,
  ScaleType,
  Chord,
  Function,
  ) where

import Data.VectorSpace
import Data.AffineSpace
import Music.Pitch -- Test


data ScaleType a = ScaleType [Diff a] (Diff a)
data Scale a = Scale a (ScaleType a)

data Function a = Function [Diff a] (Diff a)
data Chord a = Chord a (Function a)

-- Test
cMajorScale = Scale c majorScale

majorScale :: ScaleType Pitch
majorScale = ScaleType [_M2,_M2] _P8


data Mode a = Mode (Scale a) a

cMajorKey = Mode cMajorScale c
aMajorKey = Mode cMajorScale a
