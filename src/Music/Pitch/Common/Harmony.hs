
module Music.Pitch.Common.Harmony (
        isConsonance,
        isPerfectConsonance,
        isImperfectConsonance,
        isDissonance,
  ) where

import Music.Pitch.Common.Number

-- | Whether the given interval is a consonance.
isConsonance :: HasNumber a => a -> Bool
isConsonance a = isPerfectConsonance a || isImperfectConsonance a

-- | Whether the given interval is a perfect consonance.
isPerfectConsonance :: HasNumber a => a -> Bool
isPerfectConsonance a = case number a of
    1 -> True
    4 -> True
    5 -> True

-- | Whether the given interval is an imperfect consonance.
isImperfectConsonance :: HasNumber a => a -> Bool
isImperfectConsonance a = case number a of
    3 -> True
    6 -> True

-- | Whether the given interval is a dissonance.
isDissonance :: HasNumber a => a -> Bool
isDissonance a = case number a of
    2 -> True
    7 -> True
