
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
--
-------------------------------------------------------------------------------------

module Music.Articulation -- (
-- )
where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Control.Monad
import Control.Applicative


-- http://www.speech.kth.se/publications/masterprojects/2004/Jerkert.pdf
-- http://www.jbiomech.com/article/S0021-9290%2898%2900113-4/abstract
