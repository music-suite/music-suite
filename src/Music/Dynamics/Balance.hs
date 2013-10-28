
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
-- Provides overloaded pitch literals.
--
-------------------------------------------------------------------------------------

module Music.Dynamics.Balance -- (
-- )
where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Control.Monad
import Control.Applicative

-- http://smac2013.renconmusic.org/midi-calibration/
