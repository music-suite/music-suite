
module Music.Pitch.Common.Harmony (
        isConsonance,
        isPerfectConsonance,
        isImperfectConsonance,
        isDissonance,
  ) where

import Music.Pitch.Common.Number
import Music.Pitch.Common.Quality
import Music.Pitch.Common.Interval

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
