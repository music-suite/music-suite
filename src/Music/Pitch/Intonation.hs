
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

module Music.Pitch.Intonation (
      Intonation,
      Tuning,

      tuneAbsolute,
      -- makeBasis,
      synTune,
      tetTune,
      pureOctaveWith,

      -- * Specific tunings
      pythagorean,
      quarterCommaMeantone,
      schismaticMeantone,
      fiveToneEqual,
      sevenToneEqual,
      twelveToneEqual,
      nineteenToneEqual,
      thirtyOneToneEqual,
      fiftyThreeToneEqual,

      -- * Specific intonations
      -- standardTuning,
      standardIntonation,
)
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
type Tuning i = i -> {-FreqRatio-}Hertz

synTune :: (Interval, {-FreqRatio-}Hertz) -> (Interval, {-FreqRatio-}Hertz) -> Interval -> {-FreqRatio-}Hertz
synTune (i1, i1rat) (i2, i2rat) (Interval (a1, d2)) =
  ((makeA1 (i1, i1rat) (i2, i2rat)) ^* (fromIntegral a1)) ^+^ ((maked2 (i1, i1rat) (i2, i2rat)) ^* (fromIntegral d2))
  where makeA1 = makeBasis basis_A1
        maked2 = makeBasis basis_d2

makeBasis :: Interval -> (Interval, {-FreqRatio-}Hertz) -> (Interval, {-FreqRatio-}Hertz) -> {-FreqRatio-}Hertz
makeBasis i (i1, r1) (i2, r2) = case (convertBasisFloat i i1 i2) of
  Just (x, y) -> (x *^ r1) ^+^ (y *^ r2)
  Nothing -> error ("Cannot use intervals " ++ (show i1) ++ " and " ++ (show i2) ++ " as basis pair to represent " ++ (show i))


tuneAbsolute :: Tuning Interval -> (Pitch, Hertz) -> Pitch -> Hertz
tuneAbsolute t (b, f) p = f .+^ (t i) where i = p .-. b

-- Standard syntonic (meantone) tunings, with P8 = 2

pureOctaveWith = synTune (_P8, 2)

pythagorean :: Tuning Interval
pythagorean = pureOctaveWith (_P5, 3/2)

quarterCommaMeantone :: Tuning Interval
quarterCommaMeantone = pureOctaveWith (_M3, 5/4)

schismaticMeantone :: Tuning Interval
schismaticMeantone = pureOctaveWith (8 *^ _P4, 10)

-- TET tunings, i.e. where P8 = 2 and (some other interval) = 1

tetTune i = pureOctaveWith (i, 1)

fiveToneEqual :: Tuning Interval
fiveToneEqual = tetTune m2

sevenToneEqual :: Tuning Interval
sevenToneEqual = tetTune _A1

twelveToneEqual :: Tuning Interval
twelveToneEqual = tetTune d2

nineteenToneEqual :: Tuning Interval
nineteenToneEqual = tetTune dd2 where dd2 = d2 ^-^ _A1

thirtyOneToneEqual :: Tuning Interval
thirtyOneToneEqual = tetTune dddd3 where dddd3 = m3 ^-^ (4 *^ _A1)

fiftyThreeToneEqual :: Tuning Interval
fiftyThreeToneEqual = tetTune ddddddd6 where ddddddd6 = 31 *^ _P8 ^-^ 53 *^ _P5 -- (!)

standardIntonation :: Intonation Pitch
standardIntonation = tuneAbsolute twelveToneEqual (a, 440)
