
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
-- Provides intonation.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Intonation -- (
-- )
where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad
import Control.Applicative
import Music.Pitch.Absolute
import Music.Pitch.Literal
import Music.Pitch.Common.Interval
import Music.Pitch.Common.Pitch

type Intonation p = p -> Hertz
type IntoneInterval i = i -> FreqRatio

synTune :: (Interval, FreqRatio) -> (Interval, FreqRatio) -> Interval -> FreqRatio
synTune (i1, i1rat) (i2, i2rat) (Interval (a1, d2)) =
  ((makeA1 (i1, i1rat) (i2, i2rat)) ^* (fromIntegral a1)) ^+^ ((maked2 (i1, i1rat) (i2, i2rat)) ^* (fromIntegral d2))
  where makeA1 = makeBasis basis_A1
        maked2 = makeBasis basis_d2

makeBasis :: Interval -> (Interval, FreqRatio) -> (Interval, FreqRatio) -> FreqRatio
makeBasis i (i1, r1) (i2, r2) = case (convertBasisFloat i i1 i2) of
  Just (x, y) -> (x *^ r1) ^+^ (y *^ r2)
  Nothing -> error ("Cannot use intervals " ++ (show i1) ++ " and " ++ (show i2) ++ " as basis pair to represent " ++ (show i))


tuneAbsolute :: IntoneInterval Interval -> (Pitch, Hertz) -> Pitch -> Hertz
tuneAbsolute t (b, f) p = f .+^ (t i) where i = p .-. b

pythagorean = synTune (basis_P8, 2) (basis_P5, 3/2)

twelveToneEqual = synTune (basis_P8, 2) (basis_d2, 1)

nineteenToneEqual = synTune (basis_P8, 2) (_dd2, 1) where _dd2 = Interval (-1, 1)

quarterCommaMeanTone = synTune (basis_P8, 2) (_M3, 5/4)

standardTuning :: Intonation Pitch
standardTuning = tuneAbsolute twelveToneEqual (a, 440)
