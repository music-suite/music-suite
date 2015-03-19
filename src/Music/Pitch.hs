
------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides various representations of musical pitches and intervals. 
--
-------------------------------------------------------------------------------------

module Music.Pitch (
        module Data.Semigroup,
        module Data.VectorSpace,
        module Data.AffineSpace,
        module Data.AffineSpace.Point,

        module Music.Pitch.Augmentable,
        module Music.Pitch.Alterable,
        module Music.Pitch.Absolute,
        module Music.Pitch.Ambitus,
        module Music.Pitch.Equal,
        module Music.Pitch.Common,
        module Music.Pitch.Clef,
        module Music.Pitch.Intonation,
        module Music.Pitch.Literal,
  ) where

import Data.Semigroup
import Data.VectorSpace hiding (Sum, getSum)
import Data.AffineSpace
import Data.AffineSpace.Point

import Music.Pitch.Absolute
import Music.Pitch.Augmentable
import Music.Pitch.Alterable
import Music.Pitch.Ambitus
import Music.Pitch.Equal
import Music.Pitch.Common
import Music.Pitch.Common.Names
import Music.Pitch.Literal
import Music.Pitch.Clef
import Music.Pitch.Intonation

