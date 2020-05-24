{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK hide #-}

module Music.Score.Internal.Util
  ( unRatio,
    through,
    composed,
    replic,
    divideList,
    retainUpdates,
    retainUpdates2,
    swap,
    inspecting,
    single,
    withNext,
    rotr,
    rotated,
    tripr,
    tripl,
    merge,
    rots,
    splitWhile,
    filterOnce,
    breakList,
    mapIndexed,
    unf,
    mapF,
    mapT,
    mapFTL,
    mapL,
    dup,
    mergeBy,
    mergeBy',
    partial,
    partial2,
    partial3,
    list,
    showRatio,
    mapWithNext,
    mapWithPrevNext,
    mapWithPrev,
    toDouble,
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad.Plus
import qualified Data.Char
import qualified Data.List
import qualified Data.Monoid
import qualified Data.Ratio

-- | Divide a list into parts of maximum length n.
-- > category : List
-- > depends : base
divideList :: Int -> [a] -> [[a]]
divideList n xs
  | length xs <= n = [xs]
  | otherwise = [take n xs] ++ (divideList n $ drop n xs)

-- | Group a list into sublists whereever a predicate holds. The matched element
--   is the first in the sublist.
--
--   > splitWhile isSpace "foo bar baz"
--   >    ===> ["foo"," bar"," baz"]
--   >
--   > splitWhile (> 3) [1,5,4,7,0,1,2]
--   >    ===> [[1],[5],[4],[7,0,1,2]]
--
-- > category : List
-- > depends : base
splitWhile :: (a -> Bool) -> [a] -> [[a]]
splitWhile p xs = case splitWhile' p xs of
  [] : xss -> xss
  xss -> xss
  where
    splitWhile' p [] = [[]]
    splitWhile' p (x : xs) = case splitWhile' p xs of
      (xs : xss) -> if p x then [] : (x : xs) : xss else (x : xs) : xss
      _ -> error "splitWhile"

-- | Break up a list into parts of maximum length n, inserting the given list as separator.
--   Useful for breaking up strings, as in @breakList 80 "\n" str@.
--
-- > category : List
-- > depends : base
breakList :: Int -> [a] -> [a] -> [a]
breakList n z = Data.Monoid.mconcat . Data.List.intersperse z . divideList n

-- |  Map over the indices and elements of list.
--  > category : List
--  > depends : base
mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed f = zipWith f [0 ..]

-- test

-- | Duplicate an element.
-- > category: Combinator, Tuple
-- > depends: base
dup :: a -> (a, a)
dup x = (x, x)

-- | Unfold a partial function. This is a simpler version of 'Data.List.unfoldr'.
-- > category: Function, List
-- > depends: base
unf :: (a -> Maybe a) -> a -> [a]
unf f = Data.List.unfoldr (fmap dup . f)

mapF :: forall b. (b -> b) -> [b] -> [b]

mapT :: forall b. (b -> b) -> [b] -> [b]

mapL :: forall b. (b -> b) -> [b] -> [b]

-- |
-- Map over first elements of a list.
-- Biased on first element for shorter lists.
-- > category: List
-- > depends: base
mapF f = mapFTL f id id

-- |
-- Map over all but the first and last elements of a list.
-- Biased on middle elements for shorter lists.
-- > category: List
-- > depends: base
mapT f = mapFTL id f id

-- |
-- Map over last elements of a list.
-- Biased on last element for shorter lists.
-- > category: List
-- > depends: base
mapL f = mapFTL id id f

-- |
-- Map over first, middle and last elements of list.
-- Biased on first, then on first and last for short lists.
--
-- > category: List
-- > depends: base
mapFTL :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapFTL f g h = go
  where
    go [] = []
    go [a] = [f a]
    go [a, b] = [f a, h b]
    go xs =
      [f $ head xs]
        ++ map g (tail $ init xs)
        ++ [h $ last xs]

-- |
-- Extract the first consecutive sublist for which the predicate returns true, or
-- the empty list if no such sublist exists.
-- > category: List
-- > depends: base
filterOnce :: (a -> Bool) -> [a] -> [a]
filterOnce p = Data.List.takeWhile p . Data.List.dropWhile (not . p)

-- | Returns all rotations of the given list. Given an infinite list, returns an infinite
-- list of rotated infinite lists.
-- > category: List
-- > depends: base
rots :: [a] -> [[a]]
rots xs = init (zipWith (++) (Data.List.tails xs) (Data.List.inits xs))

-- |
-- > category: List
-- > depends: base
rotl :: [a] -> [a]
rotl [] = []
rotl (x : xs) = xs ++ [x]

-- |
-- > category: List
-- > depends: base
rotr :: [a] -> [a]
rotr [] = []
rotr xs = last xs : init xs

-- |
-- > category: List
-- > depends: base
rotated :: Int -> [a] -> [a]
rotated = go
  where
    go n as
      | n >= 0 = iterate rotr as !! n
      | n < 0 = iterate rotl as !! abs n
      | otherwise = error "Impossible"

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 = curry . curry . (. tripl)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 = (. untripl) . uncurry . uncurry

untripl :: (a, b, c) -> ((a, b), c)
untripl (a, b, c) = ((a, b), c)

tripl :: ((a, b), c) -> (a, b, c)
tripl ((a, b), c) = (a, b, c)

tripr :: (a, (b, c)) -> (a, b, c)
tripr (a, (b, c)) = (a, b, c)

-- TODO mo
partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

partial2 :: (a -> b -> Bool) -> a -> b -> Maybe b

partial3 :: (a -> b -> c -> Bool) -> a -> b -> c -> Maybe c

partial2 f = curry (fmap snd . partial (uncurry f))

partial3 f = curry3 (fmap (view _3) . partial (uncurry3 f))

-- | Case matching on lists.
-- > category: List
-- > depends: base
list :: r -> ([a] -> r) -> [a] -> r
list z f [] = z
list z f xs = f xs

-- | Merge lists.
-- > category: List
-- > depends: base
merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

-- | Merge lists.
-- > category: List
-- > depends: base
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f = mergeBy' $ (fmap . fmap) orderingToBool f
  where
    orderingToBool LT = True
    orderingToBool EQ = True
    orderingToBool GT = False

mergeBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeBy' pred xs [] = xs
mergeBy' pred [] ys = ys
mergeBy' pred (x : xs) (y : ys) =
  case pred x y of
    True -> x : mergeBy' pred xs (y : ys)
    False -> y : mergeBy' pred (x : xs) ys

-- | Compose all functions.
-- > category: Function
-- > depends: base
composed :: [b -> b] -> b -> b
composed = Prelude.foldr (.) id

-- | Separate a ratio.
-- > category: Math
-- > depends: base
unRatio :: Data.Ratio.Ratio a -> (a, a)
unRatio x = (Data.Ratio.numerator x, Data.Ratio.denominator x)

-- | Nicer printing of ratio as ordinary fractions.
-- > category: Math
-- > depends: base
showRatio :: Data.Ratio.Ratio Integer -> String
showRatio (id -> (unRatio -> (x, 1))) = show x
showRatio (id -> (unRatio -> (x, y))) = "(" ++ show x ++ "/" ++ show y ++ ")"

-- Replace all contigous ranges of equal values with [Just x, Nothing, Nothing ...]
-- > category: List
-- > depends: base
retainUpdates :: Eq a => [a] -> [Maybe a]
retainUpdates = snd . Data.List.mapAccumL g Nothing
  where
    g Nothing x = (Just x, Just x)
    g (Just p) x = (Just x, if p == x then Nothing else Just x)

retainUpdates2 :: (Eq a, Eq b) => [(a, b)] -> [(Maybe a, Maybe b)]
retainUpdates2 = snd . Data.List.mapAccumL g Nothing
  where
    g Nothing (x, y) = (Just (x, y), (Just x, Just y))
    g (Just (p, q)) (x, y) =
      ( Just (x, y),
        ( if p == x then Nothing else Just x,
          if q == y then Nothing else Just y
        )
      )

-- Generic version of 'replicate'.
-- > category: List
-- > depends: base
replic :: Integral a => a -> b -> [b]
replic n = replicate (fromIntegral n)

-- Swap components.
-- > category: Tuple
-- > depends: base
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Interleave a list with the next consecutive element.
--
-- For any xs
--
-- > lenght xs == length (withNext xs)
--
-- If @xs@ is a finite list
--
-- > isNothing  $ snd $ last $ withNext xs == True
-- > all isJust $ snd $ init $ withNext xs == True
--
-- If @xs@ is an infinite list
--
-- > all isJust $ snd $ withNext xs == True
--
-- > category: List
-- > depends: base
withNext :: [a] -> [(a, Maybe a)]
withNext = fmap (\(p, c, n) -> (c, n)) . withPrevNext

withPrev :: [a] -> [(Maybe a, a)]
withPrev = fmap (\(p, c, n) -> (p, c)) . withPrevNext

-- withNext = go
--     where
--         go []       = []
--         go [x]      = [(x, Nothing)]
--         go (x:y:rs) = (x, Just y) : withNext (y : rs)

withPrevNext :: [a] -> [(Maybe a, a, Maybe a)]
withPrevNext xs = zip3 (pure Nothing ++ fmap Just xs) xs (fmap Just (tail xs) ++ repeat Nothing)

-- Map over a list with the next consecutive element.
--
-- > category: List
-- > depends: base
mapWithNext :: (a -> Maybe a -> b) -> [a] -> [b]
mapWithNext f = map (uncurry f) . withNext

mapWithPrev :: (Maybe a -> a -> b) -> [a] -> [b]
mapWithPrev f = map (uncurry f) . withPrev

mapWithPrevNext :: (Maybe a -> a -> Maybe a -> b) -> [a] -> [b]
mapWithPrevNext f = map (uncurry3 f) . withPrevNext

--------

toDouble :: Real a => a -> Double
toDouble = realToFrac

through ::
  Applicative f =>
  Lens' s a ->
  Lens s t a b ->
  Lens (f s) (f t) (f a) (f b)
through lens1 lens2 = lens getter (flip setter)
  where
    getter = fmap (view lens1)
    setter = liftA2 (over lens2 . const)
{-# INLINE through #-}

single :: Prism' [a] a
single = prism' return $ \xs -> case xs of
  [x] -> Just x
  _ -> Nothing
{-# INLINE single #-}

-- Like Data.Ord.comparing
-- (Are both variants of contramap?)
inspecting :: Eq a => (b -> a) -> b -> b -> Bool
inspecting p x y = p x == p y
