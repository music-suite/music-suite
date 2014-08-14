
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE CPP #-}

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
newtype Ordered a = Ordered { getOrdered :: {-Ord a =>-} Map.Map a Positive }
  deriving ({-, Foldable, Traversable-})

instance (Ord a) => Eq (Ordered a) where
  Ordered a == Ordered b = a == b

instance Ord a => Ord (Ordered a) where
  Ordered a < Ordered b = a < b

-- | Alas, not a functor
_map :: (Ord a, Ord b) => (a -> b) -> Ordered a -> Ordered b
_map f (Ordered m) = Ordered (Map.mapKeys f m)

_toList :: Ordered a -> [a]
_toList (Ordered xs) = concatMap (uncurry $ flip $ replicate . fromIntegral) $ Map.toList xs

_unsafeFromList :: Ord a => [a] -> Ordered a
_unsafeFromList = fromMaybe (error "_unsafeFromList: Not sorted") . _fromList

-- Safe (but slow) conversion from lists to ordered lists
_fromList :: Ord a => [a] -> Maybe (Ordered a)
_fromList xs
  | xs /= sort xs = Nothing
  | otherwise     = Just $ Ordered $ Map.fromList $ map (\x -> (head x, fromIntegral $ length x)) $ group xs

-- Safe (but slow) prism from lists to ordered lists
ordered :: Ord a => Prism' [a] (Ordered a)
ordered = prism' _toList _fromList
{-
  where
    prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
    prism bt seta = dimap seta (either pure (fmap bt)) . right'
-}

unsafeOrdered :: Ord a => Iso' [a] (Ordered a)
unsafeOrdered = iso _unsafeFromList _toList
{-
  where
    iso sa bt = dimap sa (fmap bt)
-}

_elem :: Ord a => a -> Ordered a -> Bool
_elem k = (> 0) . _occs k    
    
_occs :: Ord a => a -> Ordered a -> Int
_occs k = maybe 0 fromIntegral . Map.lookup k . getOrdered

_null :: Ordered k -> Bool
_null = Map.null . getOrdered

_length :: Ordered a -> Int
_length = length . _toList
-- _elem x (Ordered xs) = x


-- TODO move
#ifndef GHCI
instance Comonad Min where
  extract (Min x) = x
  duplicate (Min x) = Min (Min x)
instance Comonad Max where
  extract (Max x) = x
  duplicate (Max x) = Max (Max x)
#endif



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



