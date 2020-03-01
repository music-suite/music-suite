{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints
  #-}
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
-- Provides Balances, i.e. mappings from relative to absolute dynamics.
module Music.Dynamics.Balance where -- (
-- )

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Semigroup

-- http://smac2013.renconmusic.org/midi-calibration/
