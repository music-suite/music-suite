
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

module Music.Pitch.Intonation -- (
-- )
where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Control.Monad
import Control.Applicative
import Music.Pitch.Absolute

type Intonation a = a -> Hertz

pure :: Integral a => Intonation a
pure = undefined

pythagorean :: Integral a => Intonation a
pythagorean = undefined

twelveToneEqual :: Integral a => Intonation a
twelveToneEqual = undefined
