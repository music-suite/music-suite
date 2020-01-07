
module Main where

import Music.Prelude
import Util
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified System.Process
import qualified System.Random
import Data.Bifunctor (first, second)
import Data.Traversable (sequenceA)
{-

Fanfare (around 5 min?, possibly longer depending on material)
For variable number of brass (reference ensemble: 8 tpt, 6 tbn)

Can we develop a material in a general way for a varying number of instruments (5-10, or even more disparate)
Eventually, a "temporal/spacial" piece for, say 50-60 individual players (all generated from similar functions)

Basic ideas:
  Trumpets:
    Starting on c' and eventually falling chromatically, eventually dropping one fundamental.
    I.e. c' b bb (a?) ab, then fall to g and repeat.
    If we do not fall more than M3 we can play it on two instrument types, if pitches are chosen carefully (i.e. Bb and C or C and D instruments).

    C and D!
    (Avoid lowest 2 positions on trombone, so we can transpose M2 down and do a version for Bb and C trumpets)

    Develop by
      Starting on higher fundamentals
        This means more "restarts", but fewer half pitches to move down. Start moving up and down several times (compare trombones below).
  
  Trombones:
    Start together on low pitch, then eventually move upwards into complex chord.
    First part moves first, reachers top pitch. Second part moves second, reaches 2nd from top etc.
    Introduce repetition before movement (so lower parts can breath and repeat before they start to move)
    Movement is out of sync
    Possibility: All lower parts accentuate when the movement of a new part starts (so accents gradually become softer)

    Harmony based on combination of P5, d5 and M2 (as in the Muliebris chord)
    
    Develop by
      Starting lower and going higher?
      Moving down before moving up
      Eventually just moving up and down, creating an indefinate texture?
    

-}
music = text "Hello" c


{-
-- >>> overtone 6
-- bb
overtone    = ([c_,c,g,c',e',g',bb',c'',d'',e'',fs''::Pitch] !!)
fundamental = overtone 0
-}

-- >>> fmap ((asPitch c .+^).brassPosition) [0..6]
-- [c,b_,bb_,a_,ab_,g_,fs_]
brassPosition :: Int -> Interval
brassPosition x = negateV $ spell usingFlats (fromIntegral x :: Semitones)

{-
TODO
- Function of int (variation)
  - Function of part (Int?)
    - Define structure
    - Define the pitches
    - Define the duration patterns

-}
-- overtones (1=fund) [3,2] [4,3,2] [5,4,3,2], [6,5,4,3,2]

-- All pitches (top to bottom) from a given overtone down to (but not including) the previous overtone
--
-- >>> betweenOvertones 4
-- [e',eb',d',db']
--
-- >>> betweenOvertones 3
-- [c',b,bb,a,ab,g,gb]
--
betweenOvertones :: Int -> [Pitch]
betweenOvertones n = enumDownChromaticFromTo (overtone n) (overtone (n - 1))

trumpetPitches :: [[Pitch]]
trumpetPitches = fmap (take 7 . betweenOvertones) [1..7]
-- Limit to 7!

showPitch2D :: [[Pitch]] -> Music
showPitch2D xss = rcat $ fmap (scat . fmap toNote) xss
showPitch1D :: [Pitch] -> Music
showPitch1D xs  = scat $ fmap toNote $ xs

showRhythm2D :: [[Duration]] -> Music
showRhythm2D xss = rcat $ fmap (scat . fmap (`stretch` c)) xss
showRhythm1D :: [Duration] -> Music
showRhythm1D xs  = scat $ fmap (`stretch` c) $ xs
{-
:o showPitch2D $ trumpetPitches
:o showPitch1D $ trumpetPitches !! 0

Sequence in the piece
  trumpetPitches !! 2
  trumpetPitches !! 1

  trumpetPitches !! 3
  trumpetPitches !! 2
  trumpetPitches !! 1

  trumpetPitches !! 4
  trumpetPitches !! 3
  trumpetPitches !! 2
  trumpetPitches !! 1

  trumpetPitches !! 5
  trumpetPitches !! 3
  trumpetPitches !! 2
  trumpetPitches !! 1
  
-}
-- BREATH BREATH BREATH BREATH BREATH BREATH BREATH BREATH
ns  = fmap (\n -> [n,n-1..1]) [2..5]

-- levels: cue, fundamental, pitch
ns2 :: [[[Pitch]]]
ns2 = fmap (fmap (trumpetPitches !!)) ns

-- Good so far
ns3 :: [[[Music]]]
ns3 = (fmap.fmap.fmap) (\p -> toNote p) ns2

ns4 :: [[[Music]]]
ns4 = fmap (\pss -> zip2DWith (\p d -> text (show $ d*4) $ stretch d p) pss (compress 4 manyRhSeries)) ns3

applyRh2 :: Transformable c => [[c]] -> [[c]]
applyRh2 pss = zip2DWith (\p d -> {-text (show $ d*4) $-} stretch d p) pss (compress 4 manyRhSeries)

applyRh :: Transformable c => Int -> [c] -> [c]
applyRh n ps = zipWith (\p d -> stretch d p) ps (nthRhSeries n)

-- How to do time?
-- Just a (bad) sketch
testTrumpets :: Music
testTrumpets = level ff $ rcat $ set parts' trumpets $ fmap (\n -> times n padBar |> fullTrumpetFall n) [1..8]
  where
    padBar :: Music
    padBar          = colorBlue $ c'
    fullTrumpetFall :: Int -> Music
    fullTrumpetFall n = ps
        where
          ps = scat $ map (breakInto (12) . compress 4) $ applyRh n $ concat $ chooseExp $ ns3
          -- chooseExp = {-Only use first exposistion for now-} head
          chooseExp = concat
          
-- Break something into notes of the given duration
-- (x^duration)*n must be a whole number or you will get bad results
breakInto :: (Monoid s, Transformable s, HasPosition s, Semigroup s) => Duration -> s -> s
breakInto n x = stretchTo (x^.duration) $ times (floor $ n * x^.duration) x


{-
A series for the number of quarter notes to hold a pitch before switching

Currently it is
  [4,4..]

We want something quasi-simplistic, using mainly 4, 6 or 8 but occassionally an odd number
to throw things out of sync. We need to be able to generate 8 or more variants with some
similarity and dissimilarity. They can start in a similar way because we offset the initial
note anyway.
-}
rhSeries :: [Duration]
rhSeries = flip weightRands rands 
  [ (8 :: Weight, 4)
  , (8 :: Weight, 6)
  , (8 :: Weight, 8)
  , (4 :: Weight, 5)  
  , (2 :: Weight, 3)  
  ]

nthRhSeries n = drop (n+87) rhSeries

manyRhSeries :: [[Duration]]
manyRhSeries = fmap nthRhSeries [1..]  






