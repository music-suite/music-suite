
module Main where

import Music.Prelude
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified System.Random
import Data.Bifunctor (first, second)
import Data.Traversable (sequenceA)
import qualified Data.List
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
main = defaultMain music
music = testTrumpets


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
showPitch2D xss = rcat $ fmap (pseq . fmap fromPitch) xss
showPitch1D :: [Pitch] -> Music
showPitch1D xs  = pseq $ fmap fromPitch $ xs

showRhythm2D :: [[Duration]] -> Music
showRhythm2D xss = rcat $ fmap (pseq . fmap (`stretch` c)) xss
showRhythm1D :: [Duration] -> Music
showRhythm1D xs  = pseq $ fmap (`stretch` c) $ xs
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
ns3 = (fmap.fmap.fmap) (\p -> fromPitch p) ns2

ns4 :: [[[Music]]]
ns4 = fmap (\pss -> zip2DWith (\p d -> text (show $ d*4) $ stretch d p) pss (compress 4 manyRhSeries)) ns3

applyRh2 :: Transformable c => [[c]] -> [[c]]
applyRh2 pss = zip2DWith (\p d -> {-text (show $ d*4) $-} stretch d p) pss (compress 4 manyRhSeries)

applyRh :: Transformable c => Int -> [c] -> [c]
applyRh n ps = zipWith (\p d -> stretch d p) ps (nthRhSeries n)

-- How to do time?
-- Just a (bad) sketch
testTrumpets :: Music
testTrumpets = filterWithTime (\t _ _ -> 0 <= t && t < 30) $
  level ff $ rcat $ set parts' trumpets $ fmap (\n -> times n padBar |> fullTrumpetFall n) [1..8]
  where
    padBar :: Music
    padBar          = colorBlue $ c'
    fullTrumpetFall :: Int -> Music
    fullTrumpetFall n = ps
        where
          ps = pseq $ map (breakInto (12) . compress 4) $ applyRh n $ concat $ chooseExp $ ns3
          -- chooseExp = {-Only use first exposistion for now-} head
          chooseExp = concat

-- Break something into notes of the given duration
-- (x^duration)*n must be a whole number or you will get bad results
breakInto :: (Monoid s, Transformable s, HasPosition s, Semigroup s) => Duration -> s -> s
breakInto n x = case _era x of
  Nothing -> x
  Just e -> stretchTo (_duration e) $ times (floor $ n * _duration e) x


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

type Weight = Double -- 0 <= x <= 1

-- | A very simple random weight function.
weightRands
  :: [(Weight, a)]    -- ^ Weighted mvalues
  -> [Double]         -- ^ Random supply (0 <= x <= 1)
  -> [a]
  -- -> [(Weight, a)]
weightRands weights supply = fmap (snd . head) $ res
  where
    totalWeight       = sum (fmap fst weights)
    normalizedWeights = fmap (first (/ totalWeight)) weights
    offsetWeights     = snd $ Data.List.mapAccumL f 0 normalizedWeights
    f prevOffset (normWeight, x) = (prevOffset+normWeight, (prevOffset+normWeight, x))

    res = fmap (\dice -> dropWhile (\(offsetWeight,_) -> offsetWeight < dice) offsetWeights) supply

-- >>> overtone 6
-- bb
overtone    = ([c_,c,g,c',e',g',bb',c'',d'',e'',fs''::Pitch] !!)
fundamental = overtone 0

nthRhSeries n = drop (n+87) rhSeries

manyRhSeries :: [[Duration]]
manyRhSeries = fmap nthRhSeries [1..]

zip2DWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zip2DWith = zipWith . zipWith

-- TODO generate these with seed + state machine instead
-- Doubles in (0 < x < 1)
-- CAREFUL repeats rather quickly
rands = cycle [7.429257318751614e-2,0.2912059078902136,0.31316463977497133,0.20826641302553717,0.9252094596705723,0.5549124288611756,0.9459849915609237,0.2539982820601603,0.8193232924218949,0.7662724751120842,0.4599122669271656,0.3907047410083274,0.5853175935378737,0.9146050127586565,0.8207504452236798,0.36722274208620387,0.17174466481871775,0.6345812736975377,0.1781840788432929,2.2964727413095076e-3,0.36980527335856606,0.6926664558433664,0.19074319803533468,5.0637055843367085e-2,0.6998123042913051,0.19441792139541858,0.17501322559912125,0.7747553835066961,0.7223025502427426,0.5405834509795854,0.3012583155102816,0.96117512132372,0.23442457134378847,0.5441792677875034,0.12892485538545817,0.14715625510331498,0.3917895927601821,0.3188506503016785,0.19465030205841105,0.925972977719653,0.7365806906896263,0.35974614942184024,0.16677233081944642,0.20375549696064932,0.5083925641653336,0.2609166117739342,0.86022096597922,0.33861470238404656,0.5502777269718722,0.889235544017378,0.6192809742452946,0.15999096482447772,0.6951403956336047,0.20751491355173346,0.155827092255028,4.9524380192758066e-2,0.6569146760869068,0.31629168218832326,0.7768175928893872,0.6072666119093576,0.7798397242602304,0.587196771856074,0.4944814778402107,0.11339655339344068,0.4344921882652599,0.5890631055003754,6.902428418997575e-2,0.9028181830228169,0.5251483954386853,7.637096319366798e-2,2.208206575913485e-2,0.9874986048405298,0.24554111651687283,0.788529131285573,0.9586959460272685,0.2835197340167279,0.22368697664440051,1.3808239165837954e-2,0.92804380334872,0.8991598134103386,0.4601663876720137,0.3380183056213647,0.9160893452479851,5.087015561015551e-2,0.6146588191005711,0.20435285724643404,0.39067391443070465,0.3018070032861324,7.20230777280163e-2,0.44442392578468903,0.6627283687478381,0.1349687075146485,0.8572690692269058,0.6901211700608283,0.5870546068441326,0.7618199810169137,0.11054218108040792,0.7202179857503165,0.4851205236565409,0.33865326166614496,0.44252894128447606,0.2981660375118682,0.12304603358274102,0.9723347883834453,0.32934721777660136,0.9655082674733565,0.7159432547700425,0.13469674088984707,0.327998596581796,0.22232138773877386,0.1529627302617378,0.2595354151088708,0.8918741759873379,0.5874563438650867,0.4111224413709862,0.12953581023416283,9.972169155295718e-2,0.5985090981476547,0.3745528360091088,0.9041782861602056,0.3797743365922063,0.6088536963716557,0.6467188524655093,0.1694348026517104,0.42162830616184976,7.390587752884803e-2,0.43922809245180683,0.9852679692902845,0.21243657675645078,0.1263045213030769,0.9035994653838951,0.8107990341000378,0.48036268304228713,0.2050444604903917,9.872781889509485e-2,0.3046361844269062,0.5320425880817639,0.2120554864489842,0.7786683785420414,0.4623125989885445,0.4016694960206635,8.076134539937829e-2,0.6634982395409994,0.973444365088678,7.889841611542592e-2,0.33838227603391924,0.9772571216273699,0.9518179123312442,0.5762269847162659,0.2214678055625472,3.6513639430700406e-2,0.8046926041612403,0.6831026792434226,0.9694283676285113,0.8976488890080049,0.22966999840929636,0.6120451525100346,0.7710526455673758,6.360326837104602e-2,0.4583933945481846,0.19584261975013784,0.28369743337551645,2.109268474175252e-4,0.10003518366844599,0.28196737420826457,0.40326576685247595,0.6962118581985136,8.68063205289007e-2,0.432513253581985,0.4500688887334541,0.9158064405362926,0.9255192335428079,0.527412423802402,0.682542158991272,0.2927410266549708,0.3090004396249467,0.8860179900312615,0.8064212225829261,0.4936523693374626,0.13089072026882165,0.9155837586628148,0.3976721685961464,0.3936454773155089,0.16132255677964236,0.63543775220042,0.8616304614749221,0.4001080997466532,6.0934911654278445e-2,0.7707254331377812,0.9330000804824173,0.6963242352075858,0.9317808816173841,0.1148351548972667,0.4440004721000297,0.5737132022206706,3.4776036927188336e-2,9.304020505086674e-2,0.6696153826910349,0.9482111963793334,0.6834996823597761]





