
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE CPP #-}

module Data.List.Ordered where

import           Control.Lens (Iso', iso, Prism', prism')
import           Control.Applicative
import           Control.Comonad
import           Data.Functor.Couple
import           Data.Ord            (comparing)
import           Data.List           (takeWhile, sort, sortBy, group)
import           Data.Maybe
import           Data.Semigroup
import           Control.Monad

import qualified Data.Set as Set
import qualified Data.Map as Map
import Numeric.Natural


-- TODO full definition
type Positive = Natural


-- TODO move
#ifndef GHCI
instance Comonad Min where
  extract (Min x) = x
  duplicate (Min x) = Min (Min x)
instance Comonad Max where
  extract (Max x) = x
  duplicate (Max x) = Max (Max x)
#endif

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
map :: (Ord a, Ord b) => (a -> b) -> Ordered a -> Ordered b
map f (Ordered m) = Ordered (Map.mapKeys f m)

toList :: Ordered a -> [a]
toList (Ordered xs) = concatMap (uncurry $ flip $ replicate . fromIntegral) $ Map.toList xs

unsafeFromList :: Ord a => [a] -> Ordered a
unsafeFromList = fromMaybe (error "unsafeFromList: Not sorted") . fromList

-- Safe (but slow) conversion from lists to ordered lists
fromList :: Ord a => [a] -> Maybe (Ordered a)
fromList xs
  | xs /= sort xs = Nothing
  | otherwise     = Just $ Ordered $ Map.fromList $ 
    Prelude.map (\x -> (head x, fromIntegral $ Prelude.length x)) $ group xs

-- Safe (but slow) prism from lists to ordered lists
ordered :: Ord a => Prism' [a] (Ordered a)
ordered = prism' toList fromList
{-
  where
    prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
    prism bt seta = dimap seta (either pure (fmap bt)) . right'
-}

unsafeOrdered :: Ord a => Iso' [a] (Ordered a)
unsafeOrdered = iso unsafeFromList toList
{-
  where
    iso sa bt = dimap sa (fmap bt)
-}

elem :: Ord a => a -> Ordered a -> Bool
elem k = (> 0) . occs k    
    
occs :: Ord a => a -> Ordered a -> Int
occs k = maybe 0 fromIntegral . Map.lookup k . getOrdered

null :: Ordered k -> Bool
null = Map.null . getOrdered

length :: Ordered a -> Int
length = Prelude.length . toList
-- elem x (Ordered xs) = x
