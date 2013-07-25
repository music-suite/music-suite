
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
        module Music.Pitch.Absolute,
        module Music.Pitch.Common,
        module Music.Pitch.Literal,
        module Music.Pitch.Interval.Literal
  ) where

import Music.Pitch.Absolute
import Music.Pitch.Common
import Music.Pitch.Literal
import Music.Pitch.Interval.Literal

