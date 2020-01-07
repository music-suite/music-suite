{-# LANGUAGE FlexibleContexts #-}
module Main where

import Music.Prelude hiding (
    flutes1,    flutes2,
    oboes1,     oboes2,
    clarinets1, clarinets2,
    bassoons1,  bassoons2,
    trumpets1,  trumpets2,
    trombones1, trombones2,
    offsetVs,
    Chord,
    Mode
    )
import Data.Foldable (Foldable)
import Music.Pitch.Scale -- Not yet standard
import qualified Music.Score
import qualified Data.List
import qualified Data.Ord
import qualified Data.Either
import qualified Data.Maybe
import qualified Debug.Trace
import qualified Data.Foldable
import qualified Music.Time.Internal.Convert
import Music.Time.Internal.Util (rotate)
-- import Util

{-

-}

motive' :: [Pitch]
motive' = offsetPoints c [-_P4,-_M2,-_P4]

motive :: Motive
motive = compress 16 $ voiceFromDurs [2,1,1,2] motive'


{-
  Total: 160 bars

  Use recursive proc for harmony (brass) + lower strings:
    16  Vla (=10b)
    8   Vla (=20b)
    4   Vc  (=40b)
    2   Vc  (=80b)
    1   Db  (=160bars)

  Violins all share 32-layer (=5b)
  I.e. we need to generate 32 violins expositions, varying as per below
-}

{-
  For each voice (out of 10+?)
    Version of motive (vary harmony – P4 vs A4, M2 vs m2)
    Offset of motive (also order, on/off)
    Transposition of motive (that is, range)
    Dynamics, solo/non-solo?

  Extremes we WANT to touch upon:
    Both I & II sequencing [5..1] at the same time or nearly so (inward)
      Initiated by crotales
    Both I & II sequencing [1..5] at the same time or nearly so (outward)
      Terminated by crotales
    I [5,4..1] followed by II [1..5] (L to R pan)
    II [5,4..1] followed by I [1..5] (R to L pan)
    Several expositions following one another in I, II doing something else
    Several expositions following one another in II, I doing something else
    I moving out or in while II being stationary (i.e. at front)

    Version of the above with soloists (a) instead of tutti, gli altri (b) playing something else (i.e. sustaining)

    Motive being flung back and forth through ensemble (in various transpositions)
-}


{-
  Way to compose such "variations"
    They are all zip lists! I.e. given
      The ZL containing all motive versions
      The ZL containing all offsets
      The ZL containing all transpositions
    Use liftA3 to combine all into versions!
    Then somehow (how?) shuffle into a likeable order

-}
type Variations = ZipList
-- Applicative
-- TODO make version that keeps track of number of variations (i.e. Variation a = (ZipList a, Option (Min Int)) or similar)

vary (ZipList xs) (ZipList ys) = ZipList (xs `merge` ys)
  where
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys) = (x:y:merge xs ys)

{-
  XXX how to handle this w.r.t. to number of variations, inf-length lists etc?
-}

motiveVs :: Variations Motive
motiveVs = pure motive `vary` (pure motive `vary` (pure $ up _P8 motive))

offsetVs :: Variations Time
offsetVs = (pure 0 `vary` pure (1/8)) `vary` (pure (5/8) `vary` pure (7/8)) `vary` pure (3/8) `vary` pure (2/8)

transpVs :: Variations Interval
transpVs = (pure _P1 `vary` pure _M2) `vary` (pure _P5 `vary` pure _M6)

combinedVs :: ZipList (Aligned (Voice Pitch))
combinedVs = liftA3 k motiveVs offsetVs transpVs
  where
    k m o t = aligned o 0 $ up t m

renderMotive :: Aligned (Voice Pitch) -> Music
renderMotive = renderAlignedVoice . fmap2 fromPitch

-- disp = openLilypond $ anExp

main = defaultMain anExp

anExp :: Music
anExp = timeSignature (3/8) $ scat $ Data.List.intersperse (c |* (15/8)) $ take 20 $
  fmap (rcat . set parts' violins) $ splitInto 20 $ getZipList $ fmap renderMotive combinedVs


splitInto :: Int -> [a] -> [[a]]
splitInto n xs = take n xs : splitInto n (drop n xs)



-- viewSib = sys "open private/studies/Piece2/Export test 1 - Full Score.pdf"

orch :: Music
orch = level mf $ flip doubleParts c $ orchParts

orchParts :: [Part]
orchParts = [flutes1,flutes2,oboes1,oboes2,clarinets1,clarinets2,bassoons1,bassoons2]
  <> divide 4 horns <> divide 3 trumpets <> divide 3 trombones <> [tutti tuba]
  <> [timp,hrp,cel]
  <> divide 2 violins <> [violas] <> [cellos] <> [doubleBasses]
[flutes1,flutes2] = divide 3 flutes
[oboes1,oboes2] = divide 3 oboes
[clarinets1,clarinets2] = divide 3 clarinets
[bassoons1,bassoons2] = divide 3 bassoons
[horns1,horns2,horns3,horns4] = divide 4 horns
[trumpets1,trumpets2,trumpets3] = divide 3 trumpets
[trombones1,trombones2,trombones3] = divide 3 trombones
basses = tutti doubleBass
timp = tutti timpani
[timp1,timp2] = divide 2 timp
cel = tutti celesta
vib = tutti vibraphone
hrp = harp
tub = tutti tuba





voiceFromDurs :: [Duration] -> [a] -> Voice a
voiceFromDurs ds x = mconcat $ zipWith stretch ds $ fmap pure x
-- OR voiceFromDurs x y = (^.voice) $ fmap (^.note) $ zip x y
-- TODO asum vs. msum vs. mconcat

type Motive = Voice Pitch

fmap2 = fmap . fmap
