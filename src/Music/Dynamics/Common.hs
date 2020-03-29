{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

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
-- Provides relative dynamics.
module Music.Dynamics.Common
  ( Dynamics,
  )
where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Monoid.Average
import Data.Semigroup

type Dynamics = Average Rational
