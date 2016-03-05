
------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable
--
-- The Music Suite comes with many standard preludes.
--
-- This module reexports "Music.Prelude.Standard" for convenience.
--
-------------------------------------------------------------------------------------

module Music.Prelude (
        module Music.Prelude.Standard,
        module Music.Prelude.Inspectable
  ) where

import Music.Prelude.Standard
import Music.Prelude.Inspectable

