-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides abstract musical articulations.
module Music.Articulation
  ( Articulation,
  )
where

import Data.Monoid.Average

type Articulation = (Average Rational, Average Rational)
