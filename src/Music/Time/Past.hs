
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}

module Music.Time.Past where

import Control.Lens -- DEBUG
import           Control.Applicative
import           Control.Comonad
import           Data.Functor.Couple
import           Data.Ord            (comparing)
import           Data.List           (takeWhile, sort, sortBy, group)
import           Data.Maybe
import           Data.Semigroup

import           Control.Monad
import           Music.Time.Behavior
import           Music.Time.Reverse
import           Music.Time.Segment
import           Music.Time.Split

import qualified Data.Set as Set
import qualified Data.Map as Map
import Numeric.Natural


-- TODO
type Positive = Natural

-- TODO move
-- TODO use positive
-- newtype Ordered a = Ordered { getOrdered :: Set.Set (a, Positive) }
-- OR
newtype Ordered a = Ordered { getOrdered :: Map.Map a Positive }

_toList :: Ordered a -> [a]
_toList (Ordered xs) = concatMap (uncurry $ flip $ replicate . fromIntegral) $ Map.toList xs

_fromList :: Ord a => [a] -> Ordered a
_fromList = fromMaybe (error "_fromList: Not sorted") . _safeFromList

-- Safe (but slow) conversion from lists to ordered lists
_safeFromList :: Ord a => [a] -> Maybe (Ordered a)
_safeFromList xs
  | xs /= sort xs = Nothing
  | otherwise     = Just $ Ordered $ Map.fromList $ map (\x -> (head x, fromIntegral $ length x)) $ group xs

-- Safe (but slow) prism from lists to ordered lists
ordered :: Ord a => Prism' [a] (Ordered a)
ordered = prism' _toList _safeFromList
{-
  where
    prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
    prism bt seta = dimap seta (either pure (fmap bt)) . right'
-}

unsafeOrdered :: Ord a => Iso' [a] (Ordered a)
unsafeOrdered = iso _fromList _toList
{-
  where
    iso sa bt = dimap sa (fmap bt)
-}
    
    

_null = Map.null . getOrdered
_length = length . _toList
-- _elem x (Ordered xs) = x


-- TODO move
instance Comonad Min where
  extract (Min x) = x
  duplicate (Min x) = Min (Min x)
instance Comonad Max where
  extract (Max x) = x
  duplicate (Max x) = Max (Max x)




-- |
-- 'Past' represents a value occuring /before and at/ some point in time.
--
-- It may be seen as a note whose era is a left-open, right-inclusive time interval.
--
newtype Past a = Past { getPast :: (Min Time, a) }
  deriving (Eq, Ord, Functor)

-- |
-- 'Future' represents a value occuring /at and after/ some point in time.
--
-- It may be seen as a note whose era is a left-open, right-inclusive time interval.
--
newtype Future a = Future { getFuture :: (Max Time, a) }
  deriving (Eq, Ord, Functor)

instance HasDuration (Past a) where
  _duration _ = 0

instance HasDuration (Future a) where
  _duration _ = 0

instance HasPosition (Past a) where
  _position (Past (extract -> t,_)) _ = t

instance HasPosition (Future a) where
  _position (Future (extract -> t,_)) _ = t

-- | Query a past value. Semantic function.
past :: Past a -> Time -> Maybe a
past (Past (extract -> t, x)) t'
  | t' <= t    = Just x
  | otherwise  = Nothing

-- | Query a future value. Semantic function.
future :: Future a -> Time -> Maybe a
future (Future (extract -> t, x)) t'
  | t' >= t    = Just x
  | otherwise  = Nothing

-- TODO more elegant
indexPast :: [Past a] -> Time -> Maybe a
indexPast ps t = firstTrue $ fmap (\p -> past p t) $ sortBy (comparing _onset) ps

firstTrue :: [Maybe a] -> Maybe a
firstTrue = listToMaybe . join . fmap maybeToList
-- firstTrue = join . listToMaybe . dropWhile isNothing

-- | Project a segment (backwards) up to the given point.
pastSeg :: Past (Segment a) -> Behavior (Maybe a)
pastSeg = undefined



