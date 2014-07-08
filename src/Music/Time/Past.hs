
module Music.Time.Past where

import Music.Time.Split
import Music.Time.Reverse
import Music.Time.Segment
import Music.Time.Behavior

-- |
-- Past represents a value occuring /up to/ some point in time.
--
-- It may be seen as a note whose era is a left-open time interval.
--
newtype Past a = Past { getPast :: (a, Time) }

-- | Query a past value.
past :: Past a -> Time -> Maybe a
past (Past (x, t)) t'
  | t' <= t    = Just x
  | otherwise  = Nothing

-- | Project a segment (backwards) up to the given point.
pastSeg :: Past (Segment a) -> Behavior (Maybe a)
pastSeg = undefined
