
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
-- The "music-pitch" package provides several pitch and interval types. 
--
-- See 'Pitch' and 'Interval' for common
-- representations. If you want to use an alternative represention, import the relevant
-- submodule.
--
-------------------------------------------------------------------------------------

module Music.Pitch (
        -- * Prerequisites
        module Data.Semigroup,
        module Data.VectorSpace,
        module Data.AffineSpace,

        -- * Pitch representation
        
        -- -- | Augmentation and diminishing, generalized to work on all interval types.
        module Music.Pitch.Augmentable,

        -- -- | Alteration, sharps and flats, generalized to work on all pitch types.
        module Music.Pitch.Alterable,

        -- -- | Frequency, cents and other absolute measurements of pitch.
        module Music.Pitch.Absolute,

        -- -- | Pitch and intervals, as defined in Common Music Theory (CMT).
        -- --
        -- --   Includes representation of common practice pitches and intervalssuch as /c sharp/, 
        -- --   /diminished second/ and so on.
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
