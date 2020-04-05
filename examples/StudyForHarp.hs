{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Data.List as List
import qualified Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Music.Prelude

{-
[Difficulty: 1] Feldman-style etude for piano or harp. Just chords with various spacing and attack order (from arp up, arp down etc). Any permitation of 4 or 5. Always 16-th notes, sustained.

>>> length $ Data.List.permutations [1..5]
120
>>> length $ Data.List.permutations [1..4]
24


TODO
  Come up with a way to select permuatations (how many to use?)
  Harmonic material?
    No octave doublings!
    Pitch content
    Ambitus

-}

music =
  title "Study 1" $ composer "Hans Hoeglund 2014" $
    _8vb
      ( set
          parts'
          -- harp FIXME does't render in Lilypond if 'harp' is set (MIDI works)
          mempty
          rh
      )
  where
    rh = pseqPaused (fmap (\x -> fromInteger $ floor $ x * 7) rands) $ compress 4 $ map (\(a, b, c) -> realize a b c) material

main = defaultMain music

pitchClassMaterial :: [PitchClassSet]
pitchClassMaterial =
  [ pitchClassSet [c, e, f, g],
    pitchClassSet [c, e, f, g],
    pitchClassSet [c, e, fs, g],
    pitchClassSet [c, e, f, g],
    pitchClassSet [c, e, g, fs, g],
    pitchClassSet [c, e, f, g, bb],
    pitchClassSet [c, e, g, fs, gs],
    pitchClassSet $ map (pitchToPitchClass . down (_M2 :: Interval)) [c, e, g, f, gs],
    pitchClassSet $ map (pitchToPitchClass . down (_M2 :: Interval)) [c, e, g, f, gs],
    pitchClassSet [c, e, g, fs, g],
    pitchClassSet [c, e, g, a, gs],
    pitchClassSet [c, e, g, a, bb],
    pitchClassSet [c, e, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, eb, g, a, gs],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, eb, g, fs, gs],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, eb, g, fs, gs],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, eb, g, fs, gs],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, eb, g, fs, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, fs, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, fs, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, fs, bb],
    pitchClassSet [c, e, g, fs, gs],
    pitchClassSet [c, e, g, fs, bb],
    pitchClassSet [c, e, g, a, bb],
    pitchClassSet [c, e, g, f, bb],
    pitchClassSet $ map (pitchToPitchClass . down (_M2 :: Interval)) [c, e, g, f, bb],
    pitchClassSet $ map (pitchToPitchClass . down (_M2 :: Interval)) [c, e, g, f, bb],
    pitchClassSet $ map (pitchToPitchClass . down (_M2 :: Interval)) [c, e, g, f, gs],
    pitchClassSet [c, e, g, f, gs],
    pitchClassSet [c, e, gs, f, gs],
    pitchClassSet [c, e, gs, a, gs],
    pitchClassSet [c, e, gs, a, gs],
    pitchClassSet [c, e, g, a, gs],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, gs],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, gs],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, eb, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, eb, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, eb, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, eb, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, eb, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, bb],
    pitchClassSet $ map (pitchToPitchClass . up (_M2 :: Interval)) [c, e, g, a, bb],
    pitchClassSet [c, e, g, a, bb],
    pitchClassSet [c, e, g, a, bb],
    pitchClassSet [c, e, g, a, bb],
    pitchClassSet [c, e, g, f, bb],
    pitchClassSet $ map (pitchToPitchClass . down (_M2 :: Interval)) [c, e, g, f, bb],
    pitchClassSet $ map (pitchToPitchClass . down (_M2 :: Interval)) [c, e, g, f, bb],
    pitchClassSet $ map (pitchToPitchClass . down (_M2 :: Interval)) [c, e, g, f, bb],
    pitchClassSet [c, e, g, f, bb],
    pitchClassSet [c, e, bb, f, gs],
    pitchClassSet [c, e, gs, a, gs],
    pitchClassSet [c, e, gs, a, gs],
    pitchClassSet [c, e, g, f, gs],
    pitchClassSet [c, e, gs, f, gs],
    pitchClassSet [c, e, gs, fs, gs],
    pitchClassSet [c, e, gs, fs, gs]
  ]

registerMaterial :: [Ambitus Interval Pitch]
registerMaterial =
  [ (c, c') ^. ambitus,
    (c, c') ^. ambitus,
    (c, c') ^. ambitus,
    (c, c') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (c_, _8va c') ^. ambitus,
    (c_, _8va c') ^. ambitus,
    (c_, _8va c') ^. ambitus,
    (c_, _8va c') ^. ambitus,
    (c_, _8va c'') ^. ambitus,
    (c_, _8va c'') ^. ambitus,
    (c_, _8va c'') ^. ambitus,
    (c_, _8va c'') ^. ambitus,
    (c_, _8va c'') ^. ambitus,
    (c_, c'') ^. ambitus,
    (c_, c') ^. ambitus,
    (c_, c') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c') ^. ambitus,
    (c, c') ^. ambitus,
    (c, c') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (c, _8va c') ^. ambitus,
    (c, _8va c') ^. ambitus,
    (c, _8va c') ^. ambitus,
    (c, _8va c') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (_8vb c_, _8vb c') ^. ambitus,
    (_8vb c_, _8vb c') ^. ambitus,
    (_8vb c_, _8vb c') ^. ambitus,
    (_8vb c_, _8vb c') ^. ambitus,
    (_8vb c_, _8vb c'') ^. ambitus,
    (_8vb c_, _8vb c'') ^. ambitus,
    (_8vb c_, _8vb c'') ^. ambitus,
    (_8vb c_, _8vb c'') ^. ambitus,
    (_8vb c_, _8vb c'') ^. ambitus,
    (_8vb c_, _8vb c'') ^. ambitus,
    (_8vb c_, _8vb c') ^. ambitus,
    (c_, c') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c') ^. ambitus,
    (c, c') ^. ambitus,
    (c, c') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (c, c') ^. ambitus,
    (c, c') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (_8va c, _8va c'') ^. ambitus,
    (c, _8va c') ^. ambitus,
    (c, _8va c') ^. ambitus,
    (c, _8va c') ^. ambitus,
    (c, _8va c') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (c, c'') ^. ambitus,
    (_8vb c_, _8vb c') ^. ambitus,
    (_8vb c_, _8vb c') ^. ambitus,
    (_8vb c_, _8vb c') ^. ambitus,
    (_8vb c_, _8vb c') ^. ambitus,
    (_8vb c_, _8vb c'') ^. ambitus,
    (_8vb c_, _8vb c'') ^. ambitus,
    (_8vb c_, _8vb c'') ^. ambitus,
    (_8vb c_, _8vb c'') ^. ambitus,
    (_8vb c_, _8vb c'') ^. ambitus
  ]

permMaterial :: [Permutation]
permMaterial =
  [ Perm4 0,
    Perm4 0,
    Perm4 0,
    Perm4 1,
    Perm4 0,
    Perm4 0,
    Perm4 6,
    Perm5 2,
    Perm5 2,
    Perm5 6,
    Perm5 6,
    Perm5 6,
    Perm4 2,
    Perm5 1,
    Perm5 2,
    Perm5 6,
    Perm4 2,
    Perm5 6,
    Perm5 6,
    Perm5 6,
    Perm4 6,
    Perm4 7,
    Perm4 6,
    Perm5 6,
    Perm5 6,
    Perm4 1,
    Perm5 6,
    Perm5 6,
    Perm5 (-5),
    Perm4 (-2),
    Perm4 (-1),
    Perm4 (-1),
    Perm4 (-1),
    Perm4 $ 1 + 0,
    Perm4 $ 1 + 0,
    Perm4 $ 1 + 0,
    Perm4 $ 1 + 1,
    Perm4 $ 1 + 0,
    Perm4 $ 1 + 0,
    Perm4 $ 1 + 6,
    Perm5 $ 1 + 2,
    Perm5 $ 1 + 2,
    Perm5 $ 1 + 6,
    Perm5 $ 1 + 6,
    Perm5 $ 1 + 6,
    Perm4 $ 1 + 2,
    Perm5 $ 1 + 1,
    Perm5 $ 1 + 2,
    Perm5 $ 1 + 6,
    Perm4 $ 1 + 2,
    Perm5 $ 1 + 6,
    Perm5 $ 1 + 6,
    Perm5 $ 1 + 6,
    Perm4 $ 1 + 6,
    Perm4 $ 1 + 7,
    Perm4 $ 1 + 6,
    Perm5 $ 1 + 6,
    Perm5 $ 1 + 6,
    Perm4 $ 1 + 1,
    Perm5 $ 1 + 6,
    Perm5 $ 1 + 6,
    Perm5 $ (-5),
    Perm4 $ (-2),
    Perm4 $ (-1),
    Perm4 $ (-1),
    Perm4 $ (-1),
    Perm4 0,
    Perm4 0,
    Perm4 0,
    Perm4 1,
    Perm4 0,
    Perm4 0,
    Perm4 6,
    Perm5 2,
    Perm5 2,
    Perm5 6,
    Perm5 6,
    Perm5 6,
    Perm4 2,
    Perm5 1,
    Perm5 2,
    Perm5 6,
    Perm4 2,
    Perm5 6,
    Perm5 6,
    Perm5 6,
    Perm4 6,
    Perm4 7,
    Perm4 6,
    Perm5 6,
    Perm5 6,
    Perm4 1,
    Perm5 6,
    Perm5 6,
    Perm5 (-5),
    Perm4 (-2),
    Perm4 (-1),
    Perm4 (-1),
    Perm4 (-1)
  ]

material :: [(PitchClassSet, Ambitus Interval Pitch, Permutation)]
material = zip3 pitchClassMaterial registerMaterial permMaterial

{-
showMaterial m = pcm' </> rm' </> pm'
  where
    (pcm, rm, pm) = unzip3 m
    pcm' = pseq $ fmap fromPitchClassSet pcm
    rm'  = pseq $ fmap showAmbitus rm
    pm'  = pseq $ fmap showPermutation pm
-}

realize :: PitchClassSet -> Ambitus Interval Pitch -> Permutation -> Score StandardNote
realize pcs reg perm = music
  where
    pitches = Set.toList $ Set.map pitchClassToPitch pcs :: [Pitch]
    transposedPitches = spreadPitchesIntoAmbitus reg {- List.sort-} pitches
    sortedTransposedPitches = List.sort transposedPitches
    pitchSequence = fmap (cycle sortedTransposedPitches !!) (indexPermutation perm)
    music = level pp $ legato $ stretchTo 1 $ pseq $ fmap fromPitch (pitchSequence :: [Pitch])

-----

-- Spread pitches (assumed to be in the first octaves) over the whole ambitus
-- Order is retained

-- TODO better algorithm here
--  * Given an ambitus of n octaves, distribute pitches into these
--    * If given to many pitches, put extremes in the middle
--    * If given to few pitches, skip the extra octaves

spreadPitchesIntoAmbitus :: Ambitus Interval Pitch -> [Pitch] -> [Pitch]
spreadPitchesIntoAmbitus amb pitches =
  zipWith {-octavesUp-}
    fifthsUp
    (ambitusOctaveList amb (fromIntegral $ length pitches))
    pitches
  where
    ambitusOctaveList :: Ambitus Interval Pitch -> Integer -> [Integer] -- 0 for same, -1 for one down etc
      -- ambitusOctaveList amb n = fmap fromIntegral $ (repeat lowestOctave) -- TODO
    ambitusOctaveList amb n = fmap (+ lowestOctave) $ fromNumPitchesAndOctaves n numOctaves
      where
        lowestOctave = fromIntegral $ ambitusLowestOctave amb
        numOctaves = fromIntegral $ (ambitusOctaves amb `max` 0)
    fromNumPitchesAndOctaves :: Integer -> Integer -> [Integer]
    fromNumPitchesAndOctaves = go
      where
        go 4 0 = [0, 0, 0, 0]
        go 4 1 = [0, 0, 0, 0]
        go 4 2 = [0, 0, 1, 1]
        go 4 3 = [0, 1, 1, 2]
        go 4 4 = [1, 2, 3, 4]
        go 5 0 = [0, 0, 0, 0, 0]
        go 5 1 = [0, 0, 0, 0, 0]
        go 5 2 = [0, 0, 0, 1, 1]
        go 5 3 = [0, 1, 1, 2, 2]
        go 5 4 = [0, 1, 1, 2, 3]
        go 5 5 = [1, 2, 3, 4, 5]
        go m n = error $ "fromNumPitchesAndOctaves: Unexpected: " ++ show m ++ " " ++ show n

newtype PitchClass = PC {getPC :: (Name, Accidental)}
  deriving (Eq, Ord, Show)

instance IsPitch PitchClass where
  fromPitch x = let p = fromPitch x in PC (name p, accidental p)

type PitchSet = Set Pitch

type PitchClassSet = Set PitchClass

pitchClassToPitch :: PitchClass -> Pitch
pitchClassToPitch (PC (n, a)) = mkPitch n a

pitchToPitchClass :: Pitch -> PitchClass
pitchToPitchClass p = PC (name p, accidental p)

fromPitchSet :: PitchSet -> Score StandardNote
fromPitchSet = ppar . fmap fromPitch . Set.toList

fromPitchClassSet :: PitchClassSet -> Score StandardNote
fromPitchClassSet = fromPitchSet . Set.map pitchClassToPitch

pitchClassSet :: [PitchClass] -> PitchClassSet
pitchClassSet = Set.fromList

data Permutation
  = Perm4 Int -- Index into Data.List.permutations [1..4], mod 24
  | Perm5 Int -- Index into Data.List.permutations [1..5], mod 120

permutationLength :: Permutation -> Int
permutationLength (Perm4 _) = 4
permutationLength (Perm5 _) = 5

indexPermutation :: Permutation -> [Int]
indexPermutation (Perm4 n) = Data.List.permutations [0 .. 3] !! (n `mod` 24)
indexPermutation (Perm5 n) = Data.List.permutations [0 .. 4] !! (n `mod` 120)

showPermutation :: (Enum a, IsPitch a) => Permutation -> Score a
showPermutation p = stretchTo 1 $ pseq . fmap ([c ..] !!) $ indexPermutation p

pseqPad :: Duration -> [Score a] -> Score a
pseqPad t xs = removeRests $ pseq $ map (\x -> fmap Just x |> stretch t rest) xs

pseqPaused :: [Duration] -> [Score a] -> Score a
pseqPaused ts xs = removeRests $ pseq $ zipWith (|>) (fmap (`stretch` rest) ts) (map (fmap Just) xs)

fifthsUp :: Integer -> Pitch -> Pitch
fifthsUp n = up (_P5 ^* n)

rands = cycle [7.429257318751614e-2, 0.2912059078902136, 0.31316463977497133, 0.20826641302553717, 0.9252094596705723, 0.5549124288611756, 0.9459849915609237, 0.2539982820601603, 0.8193232924218949, 0.7662724751120842, 0.4599122669271656, 0.3907047410083274, 0.5853175935378737, 0.9146050127586565, 0.8207504452236798, 0.36722274208620387, 0.17174466481871775, 0.6345812736975377, 0.1781840788432929, 2.2964727413095076e-3, 0.36980527335856606, 0.6926664558433664, 0.19074319803533468, 5.0637055843367085e-2, 0.6998123042913051, 0.19441792139541858, 0.17501322559912125, 0.7747553835066961, 0.7223025502427426, 0.5405834509795854, 0.3012583155102816, 0.96117512132372, 0.23442457134378847, 0.5441792677875034, 0.12892485538545817, 0.14715625510331498, 0.3917895927601821, 0.3188506503016785, 0.19465030205841105, 0.925972977719653, 0.7365806906896263, 0.35974614942184024, 0.16677233081944642, 0.20375549696064932, 0.5083925641653336, 0.2609166117739342, 0.86022096597922, 0.33861470238404656, 0.5502777269718722, 0.889235544017378, 0.6192809742452946, 0.15999096482447772, 0.6951403956336047, 0.20751491355173346, 0.155827092255028, 4.9524380192758066e-2, 0.6569146760869068, 0.31629168218832326, 0.7768175928893872, 0.6072666119093576, 0.7798397242602304, 0.587196771856074, 0.4944814778402107, 0.11339655339344068, 0.4344921882652599, 0.5890631055003754, 6.902428418997575e-2, 0.9028181830228169, 0.5251483954386853, 7.637096319366798e-2, 2.208206575913485e-2, 0.9874986048405298, 0.24554111651687283, 0.788529131285573, 0.9586959460272685, 0.2835197340167279, 0.22368697664440051, 1.3808239165837954e-2, 0.92804380334872, 0.8991598134103386, 0.4601663876720137, 0.3380183056213647, 0.9160893452479851, 5.087015561015551e-2, 0.6146588191005711, 0.20435285724643404, 0.39067391443070465, 0.3018070032861324, 7.20230777280163e-2, 0.44442392578468903, 0.6627283687478381, 0.1349687075146485, 0.8572690692269058, 0.6901211700608283, 0.5870546068441326, 0.7618199810169137, 0.11054218108040792, 0.7202179857503165, 0.4851205236565409, 0.33865326166614496, 0.44252894128447606, 0.2981660375118682, 0.12304603358274102, 0.9723347883834453, 0.32934721777660136, 0.9655082674733565, 0.7159432547700425, 0.13469674088984707, 0.327998596581796, 0.22232138773877386, 0.1529627302617378, 0.2595354151088708, 0.8918741759873379, 0.5874563438650867, 0.4111224413709862, 0.12953581023416283, 9.972169155295718e-2, 0.5985090981476547, 0.3745528360091088, 0.9041782861602056, 0.3797743365922063, 0.6088536963716557, 0.6467188524655093, 0.1694348026517104, 0.42162830616184976, 7.390587752884803e-2, 0.43922809245180683, 0.9852679692902845, 0.21243657675645078, 0.1263045213030769, 0.9035994653838951, 0.8107990341000378, 0.48036268304228713, 0.2050444604903917, 9.872781889509485e-2, 0.3046361844269062, 0.5320425880817639, 0.2120554864489842, 0.7786683785420414, 0.4623125989885445, 0.4016694960206635, 8.076134539937829e-2, 0.6634982395409994, 0.973444365088678, 7.889841611542592e-2, 0.33838227603391924, 0.9772571216273699, 0.9518179123312442, 0.5762269847162659, 0.2214678055625472, 3.6513639430700406e-2, 0.8046926041612403, 0.6831026792434226, 0.9694283676285113, 0.8976488890080049, 0.22966999840929636, 0.6120451525100346, 0.7710526455673758, 6.360326837104602e-2, 0.4583933945481846, 0.19584261975013784, 0.28369743337551645, 2.109268474175252e-4, 0.10003518366844599, 0.28196737420826457, 0.40326576685247595, 0.6962118581985136, 8.68063205289007e-2, 0.432513253581985, 0.4500688887334541, 0.9158064405362926, 0.9255192335428079, 0.527412423802402, 0.682542158991272, 0.2927410266549708, 0.3090004396249467, 0.8860179900312615, 0.8064212225829261, 0.4936523693374626, 0.13089072026882165, 0.9155837586628148, 0.3976721685961464, 0.3936454773155089, 0.16132255677964236, 0.63543775220042, 0.8616304614749221, 0.4001080997466532, 6.0934911654278445e-2, 0.7707254331377812, 0.9330000804824173, 0.6963242352075858, 0.9317808816173841, 0.1148351548972667, 0.4440004721000297, 0.5737132022206706, 3.4776036927188336e-2, 9.304020505086674e-2, 0.6696153826910349, 0.9482111963793334, 0.6834996823597761]
