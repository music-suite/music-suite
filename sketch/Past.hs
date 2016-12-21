
module Music.Time.Past (
        -- Past(..),
        -- Future(..),
        -- past,
        -- future,
        -- indexPast,
        -- firstTrue,

        -- pastSeg,
        -- futureSeg,
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens
import           Control.Monad
import           Data.Functor.Couple
import           Data.List            (group, sort, sortBy, takeWhile)
import           Data.List.Ordered
import           Data.Maybe
import           Data.Ord             (comparing)
import           Data.Semigroup

import           Music.Time.Behavior
import           Music.Time.Juxtapose

-- |
-- 'Past' represents a value occuring /before and at/ some point in time.
--
-- It may be seen as a note whose era is a left-open, right-inclusive time interval.
--
newtype Past a = Past { getPast :: (Min (Maybe Time), a) }
  deriving (Eq, Ord, Functor)

-- |
-- 'Future' represents a value occuring /at and after/ some point in time.
--
-- It may be seen as a note whose era is a left-open, right-inclusive time interval.
--
newtype Future a = Future { getFuture :: (Max (Maybe Time), a) }
  deriving (Eq, Ord, Functor)

-- instance HasDuration (Past a) where
--   _duration _ = 0
--
-- instance HasDuration (Future a) where
--   _duration _ = 0
--
-- instance HasPosition (Past a) where
--   _position (Past ((extract . extract) -> t,_)) _ = t
--
-- instance HasPosition (Future a) where
  -- _position (Future (extract -> t,_)) _ = t

-- | Query a past value. Semantic function.
past :: Past a -> Time -> Maybe a
past (Past (extract -> t, x)) t'
  | Just t' <= t    = Just x
  | otherwise       = Nothing

-- | Query a future value. Semantic function.
future :: Future a -> Time -> Maybe a
future (Future (extract -> t, x)) t'
  | Just t' >= t    = Just x
  | otherwise       = Nothing

-- TODO more elegant
indexPast :: [Past a] -> Time -> Maybe a
indexPast ps t = firstTrue $ fmap (\p -> past p t) $ sortBy (comparing tv) ps
  where
    tv (Past (Min t, _)) = t

firstTrue :: [Maybe a] -> Maybe a
firstTrue = listToMaybe . join . fmap maybeToList
-- firstTrue = join . listToMaybe . dropWhile isNothing
--
-- -- | Project a segment (backwards) up to the given point.
-- pastSeg :: Past (Segment a) -> Behavior (Maybe a)
-- pastSeg = undefined
--
-- -- | Project a segment starting from the given point.
-- futureSeg :: Future (Segment a) -> Behavior (Maybe a)
-- futureSeg = undefined
