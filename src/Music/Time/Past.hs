
module Music.Time.Past where

import Music.Time.Split
import Music.Time.Reverse

-- |
-- Past represents a value occuring /up to/ some point in time.
--
-- It may be seen as a note whose era is a left-open time interval.
--
newtype Past a = Past { getPast :: (a, Time) }

past :: Past a -> Time -> Maybe a
past (Past (x, t)) t'
  | t' <= t    = Just x
  | otherwise  = Nothing

