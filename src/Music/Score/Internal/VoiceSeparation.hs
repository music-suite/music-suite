{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK hide #-}

module Music.Score.Internal.VoiceSeparation
  ( partitionSimplistic,
  )
where

-- import Math.Combinat.Partitions.Set ()

-- import BasePrelude
import Control.Applicative
import Control.Monad
import Control.Monad.Logic (Logic, observe, observeAll)
import Control.Monad.Logic.Class (ifte, interleave)
import Control.Monad.Random (evalRand)
import Data.Foldable (asum)
import Data.Set (Set)
import qualified Data.Set as S
import System.Random (mkStdGen)
import "random-shuffle" System.Random.Shuffle (shuffleM)

{-
      We are looking for a partition of the set of notes *in each bar/staff* such that
        (voice sep should be post tie split, post chord merge, pre quantization, i.e. as late as possible,
         as complexity grows so quickly!)
      - Each subset is a voice (no overlapping notes)
      - The number of subsets (size of the partition) is as small as possible
      - The sum of all adjacent steps in each subvoice (its cumulative step size) is as small as possible
      - The ambitus of each subvoice (max pitch-min pitch) is as small as possible
    The number of partitions to consider grows quickly, so we really need a heuristic method
      https://en.wikipedia.org/wiki/Bell_number
    Type (Score a -> [NooverlapScore a]) or (Score a -> [Voice (Maybe a)])


-}

{-
Returns a set partition of the the given values.

- The first function provides a way to reject a partition as invalid.
- The first function provides a way to to weight partitions.

A partition with relatively high weight *should* be returned (no guarantees here).

-}

{-
This is probably to general.

It is essentially brute-force at is has to generate all partitions
(using setPartitionsWithKParts or similar), and then filter.

Yes we have lazy evalutation, but look at the numbers below.

    partitionWeighted :: Set a -> (Set a -> Bool) -> (Set a -> Double) -> Set (Set a)
-}

{-
When applied to (bar/part-wise) voice sep, k will almost always be in [1..4].

Prelude Math.Combinat.Partitions.Set> countSetPartitionsWithKParts 4 (16*4)
14178431382758465933500213885109548501
Prelude Math.Combinat.Partitions.Set> countSetPartitionsWithKParts 4 (16)
171798901
Prelude Math.Combinat.Partitions.Set> countSetPartitionsWithKParts 3 (16)
7141686
Prelude Math.Combinat.Partitions.Set> countSetPartitionsWithKParts 2 (16)
32767
Prelude Math.Combinat.Partitions.Set>
Prelude Math.Combinat.Partitions.Set> countSetPartitionsWithKParts 1 (16)
1

Possibly good strategy
  If k and m are small, use setPartitionsWithKParts/partitionWeighted.


-}

{-
Partition a set such that each subset matches the predicate.

Simplistic implementation:
  - Pick elements from unchosen elements such that the predicate matches
  - If we can not pick anymore elements, proceed to next subset

Fast, but unweighted (in terms of voice sep: may generate strange partitions).
Essentially:
  Remove elements from a set S until it passes the predicate, this becomes a subset.
  Repeat until no elements left.
-}
partitionSimplistic :: ([a] -> Bool) -> [a] -> [[a]]
partitionSimplistic p = recur []
  where
    recur doneSubsets [] = doneSubsets
    recur doneSubsets remainingElements =
      case observe $ pickNewSubset p remainingElements of
        (newSubset, remainingElements) -> recur (newSubset : doneSubsets) remainingElements
    choose :: [a] -> Logic (a, [a])
    choose [] = empty
    choose xs = asum $ fmap g [0 .. length xs - 1]
      where
        g n = pure $ case rotate n xs of
          (a : as) -> (a, as)
          [] -> error "Impossible"
        rotate n xs = drop n xs ++ take n xs
    chooseSuchThat :: (a -> Bool) -> [a] -> Logic (a, [a])
    chooseSuchThat p xs = mfilter (p . fst) $ choose xs
    aShuffle :: [a] -> [a]
    -- Unpredictable shuffle (based on seed)
    aShuffle xs = evalRand (shuffleM xs) (mkStdGen 12983)
    -- aShuffle = id
    -- aShuffle = reverse
    -- aShuffle = rotate 12 where rotate n xs = drop n xs ++ take n xs

    pickNewSubset ::
      ([a] -> Bool) ->
      [a] ->
      Logic ([a], [a]) -- (picked subset, remaining)
    pickNewSubset p initialSet = recur [] initialSet
      where
        recur alreadyPicked toConsider = do
          -- NOTE aShuffle must return some partition (the choice affects order,
          -- but not correctness)

          -- If its first argument succeeds at all, then the results will be fed into the success branch.
          -- Otherwise, the failure branch is taken.
          ifte
            (chooseSuchThat (\picked -> p (picked : alreadyPicked)) $ aShuffle toConsider)
            (\(picked, notPicked) -> recur (picked : alreadyPicked) notPicked)
            (return (alreadyPicked, toConsider))
{-
-- For testing

-- NOTE all predicates used here should satisfy
--  p xs ==> p (tail xs)
-- Name of that property?

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

-- allDistinct
noDuplicates :: Eq a => [a] -> Bool
noDuplicates [] = True
noDuplicates (x : xs) = all (/= x) xs

testShuffle :: forall a. [a] -> [a]
testShuffle xs = evalRand (shuffleM xs) (mkStdGen 1828372878)
-}

{-
TODO test correctness, i.e. that
  concat (partitionSimplistic p xs) =:= xs
    where
     p = anything
     x =:= y = sort x == sort y

forM_ [0..50] $ \n -> print $ partitionSimplistic noDuplicates  $ testShuffle $ [1..n] ++ [1..n] ++ [3,66,1,2]

-}
