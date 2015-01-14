
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
-- Provides a representation of pitch as defined in Common Music Theory (CMT).
--
-------------------------------------------------------------------------------------

module Music.Pitch.Common (
    module Music.Pitch.Common.Semitones,
    module Music.Pitch.Common.Quality,
    module Music.Pitch.Common.Number,
    module Music.Pitch.Common.Diatonic,
    module Music.Pitch.Common.Chromatic,
    module Music.Pitch.Common.Interval,
    module Music.Pitch.Common.Pitch,
    module Music.Pitch.Common.Spell,
    module Music.Pitch.Common.Harmony,
  ) where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad
import Control.Applicative

import Music.Pitch.Absolute
import Music.Pitch.Literal
import Music.Pitch.Common.Pitch
import Music.Pitch.Common.Quality
import Music.Pitch.Common.Number
import Music.Pitch.Common.Diatonic
import Music.Pitch.Common.Chromatic
import Music.Pitch.Common.Interval
import Music.Pitch.Common.Semitones
import Music.Pitch.Common.Spell
import Music.Pitch.Common.Harmony
