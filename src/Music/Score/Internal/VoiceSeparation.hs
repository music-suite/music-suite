
{-# LANGUAGE PackageImports #-}
module Music.Score.Internal.VoiceSeparation ()

where

import Data.Set (Set)
import qualified Data.Set as S
-- import Math.Combinat.Partitions.Set ()
import Control.Monad.Logic (Logic, observe, observeAll)
import Control.Monad.Logic.Class (interleave, ifte)
-- import BasePrelude
import Control.Applicative
import Control.Monad
import Data.Foldable (asum)

import Control.Monad.Random (evalRand)
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
-}
partitionWeighted :: Set a -> (Set a -> Bool) -> (Set a -> Double) -> Set (Set a)
partitionWeighted = undefined


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
Fast, but unweighted (in terms of voice sep: may generate strange partitions).
Essentially:
  Remove elements from a set S until it passes the predicate, this becomes a subset.
  Repeat until no elements left.
-}
partitionSimplistic :: ([a] -> Bool) -> [a] -> [[a]]
partitionSimplistic p = recur []
  where
    recur voices [] = voices
    recur voices notes =
      case observe $ partitionSimplistic1 [] notes p of
        (newVoice, remainingNotes) -> recur (newVoice : voices) remainingNotes

    -- Choose one element (all combinations)
    choose :: [a] -> Logic (a, [a])
    choose [] = empty
    choose xs = asum $ fmap (\n -> pure $ let (a:as) = rotate n xs in (a, as)) [0..length xs - 1]
      where
        rotate n xs = drop n xs ++ take n xs
    chooseSuchThat :: (a -> Bool) -> [a] -> Logic (a, [a])
    chooseSuchThat p xs = mfilter (p . fst) $ choose xs


    -- Unpredictable shuffle (based on seed)
    aShuffle :: [a] -> [a]
    aShuffle xs = evalRand (shuffleM xs) (mkStdGen 12983)
    -- aShuffle = id
    -- aShuffle = rotate 12 where rotate n xs = drop n xs ++ take n xs

    {-
    i have selected these values
    please try to combine them with these other values
    -}
    partitionSimplistic1
      :: [a] -- already picked
      -> [a] -- other values to consider
      -> ([a] -> Bool)
      -> Logic ([a], [a]) -- (picked subset, remaining)
    partitionSimplistic1 alreadyPicked toConsider p = do
      -- NOTE aShuffle must return some partition (which don't matter for correctness
      -- but will affect order)
      ifte (chooseSuchThat (\picked -> p (picked:alreadyPicked)) $ aShuffle toConsider)
          (\(picked, notPicked) -> partitionSimplistic1 (picked:alreadyPicked) notPicked p)
          (return (alreadyPicked, toConsider))


-- For testing

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

noDuplicates :: Eq a => [a] -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = not $ any (== x) xs
