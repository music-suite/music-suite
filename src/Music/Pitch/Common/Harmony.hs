
-- | Basic harmony.
module Music.Pitch.Common.Harmony (
        isDissonance,
        isConsonance,
        isPerfectConsonance,
        isImperfectConsonance,
        isMelodicDissonance,
        isMelodicConsonance,
  ) where

import Music.Pitch.Common.Interval
import Music.Pitch.Common.Number
import Music.Pitch.Common.Quality
import Music.Pitch.Literal      

import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set

{-
  TODO
  Generalize simple like this:
    > (number (asInterval (m9))-(fromIntegral $ signum (m9))) `mod` 7
    
-}

-- | Whether the given interval is a (harmonic) dissonance.
isDissonance :: Interval -> Bool
isDissonance x = case number (simple x) of
  2 -> True
  7 -> True
  _ -> False

-- | Whether the given interval is a (harmonic) consonance.
isConsonance :: Interval -> Bool
isConsonance x = isPerfectConsonance x || isImperfectConsonance x

-- | Whether the given interval is a perfect (harmonic) consonance.
isPerfectConsonance :: Interval -> Bool
isPerfectConsonance x = case number (simple x) of
  1 -> True
  4 -> True
  5 -> True
  _ -> False

-- | Whether the given interval is an imperfect (harmonic) consonance.
isImperfectConsonance :: Interval -> Bool
isImperfectConsonance x = case number (simple x) of
  3 -> True
  6 -> True
  _ -> False

-- | Whether the given interval is a melodic dissonance.
isMelodicDissonance :: Interval -> Bool
isMelodicDissonance x = not $ isMelodicConsonance x

-- | Whether an interval is melodic consonance.
isMelodicConsonance :: Interval -> Bool
isMelodicConsonance x = quality x `elem` [Perfect, Major, Minor]

{-
type Chord = Set Interval

majorTriad, minorTriad :: Chord
majorTriad = Set.fromList [_P1, _M3, _P5]
minorTriad = Set.fromList [_P1, m3,  _P5]

min7, maj7 :: Chord -> Chord
min7 = (<> Set.fromList [m7])
maj7 = (<> Set.fromList [_M7])


-}
