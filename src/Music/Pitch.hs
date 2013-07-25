
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
-- Provides various pitch representations. See 'Pitch' and 'Interval' for common
-- representations. If you want to use an alternative represention, import the
-- relevant submodule.
--
-------------------------------------------------------------------------------------

module Music.Pitch (
        -- * Prerequisites
        module Data.Semigroup,
        module Data.VectorSpace,
        module Data.AffineSpace,

        -- * Pitch representation
        module Music.Pitch.Absolute,
        module Music.Pitch.Augmentable,
        module Music.Pitch.Alterable,
        module Music.Pitch.Common,
        
        -- * Pitch literals
        module Music.Pitch.Literal,
  ) where

import Data.Semigroup
import Data.VectorSpace hiding (Sum, getSum)
import Data.AffineSpace

import Music.Pitch.Absolute
import Music.Pitch.Augmentable
import Music.Pitch.Alterable
import Music.Pitch.Common
import Music.Pitch.Literal
