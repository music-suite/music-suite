
{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, ViewPatterns, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, TupleSections #-}

module Piece2 where

import Music.Prelude hiding (
    flutes1,    flutes2,
    oboes1,     oboes2,
    clarinets1, clarinets2,
    bassoons1,  bassoons2,
    trumpets1,  trumpets2,
    trombones1, trombones2
    )
import Data.Foldable (Foldable)
import Data.Tree (Tree(..), unfoldTree, drawTree)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Music.Score
import qualified Data.List
import qualified Data.Ord
import qualified Data.Either
import qualified Data.Maybe
import qualified Control.Comonad
import qualified Debug.Trace
import qualified Music.Time.Internal.Convert
import Music.Time.Internal.Util (rotate)
import Util
-- FIXME why?
import Music.Prelude.Inspectable ()
{-

MIST for orchestra
12-13 min (180 bars, 4/4 at tempo q=54)
Don't use strict tempo throughout – experiment with 3/4 etc and add some acc/rit in post

3(II=fl/picc,III=fl/picc/afl)
3(II=ob/ob'am,III=ob/CA)
3(II=bb/ebkl,III=bb/baskl)
3(III=cbsn)
433(II=tpt/fh,III=tpt/fh)(TTB)1
  timp[large cymbal on timp, bowed and w large mallets]
  2perc[BD,med and large TD,bright sus cym,low sus cymb,vib,chimes w brushes]
  str
  
  Tpt mutes: Straight, Bucket and Harmon
  Tbn mutes: Straight and Harmon
  Tuba mutes: Straight
  Strings: 12/10/8/6/4

The piece is based on drones, all made with characteristic "floaters".
  - Very slow and low, using mainly consonant inverval (n*^_P8, P5)
  - Slow and mid-reg (M3, m3, M6, m6, M2 etc)
  - Higher, even more dissonant (whole-and semitone clusters, microtones?)
  - Some extremely fast, "shimmering" effects (obscured with micropolophony, overlapping or similar)
      Offset solo violin jetes, doubled by noise perc, flute jet whistle etc
  - Constant noisy percussion (3 sizes of cymbal, tam, thundersheet, snare drum, tenor drum, bass drum)
Generally just change one pitch at a time

PROCEDURE
Don't think too much about orchestration in beginning – Just start making some floaters (based on harmonic principle above)
Scale up floaters, rounding duration to closest 1/4
Build sequences of floaters (by rendering as score and composing in sequence – some different durational levels)
THEN orchestrate

Floater duration:
  Long: 20-30 bars
  Mid:  5-10  bars
  Short 1-2   bars
Floater properties:
  Very rarely strict L or R aligned (i.e. multiple notes starting/stopping a t same time)
  Ratio between longest and shortest note may vary up to 1:2, but is usually closer to 1:1
  I.e. we rely more on differences in phase than "frequency"

NOTE if we start putting *voices - with alternating pitches* (rather than just single notes)
in the floaters, then the dur ratios become significant!
For now stick to long notes (KISS principle)

Pitch material
  Think about number of notes in a chord (and also orchestration!)
  Whatever pitch material we use in tuttis will be very important in other parts of the piece!
  Chords of 4,8,12,10,up to 20 pitches (not PCs!)
-}


simpleShape :: Alignment -> [Duration] -> Shape
simpleShape a ds = Shape (const (repeat a, cycle ds))

-- Make a shape from a discrete function of part count
dependentShapeD :: (Int -> [Alignment]) -> (Int -> [Duration]) -> Shape
dependentShapeD f g = Shape (\n -> let (as,ds) = (f n, g n) in (as, cycle ds))

-- Make a shape from a continous function (0...1)
dependentShapeC :: (Fractional a, Fractional b) => (a -> Alignment) -> (b -> Duration) -> Shape
dependentShapeC f g = Shape $ \n -> let xs = cycle [fromIntegral x/fromIntegral n | x <- [0..n]]
  in (fmap (f . realToFrac) xs, fmap (g . realToFrac) xs)

leftLeaningShape :: [Duration] -> Shape
leftLeaningShape  = simpleShape 0
centeredShape     = simpleShape (1/2)
rightLeaningShape = simpleShape 1

leftLeaningShape' :: (Int -> [Duration]) -> Shape
leftLeaningShape'  f = dependentShapeD (const $ repeat 0)     f
centeredShape'     f = dependentShapeD (const $ repeat (1/2)) f
rightLeaningShape' f = dependentShapeD (const $ repeat 1)     f


-- The most basic shape: a rhythmic unison.
basicLeftShape :: Shape
basicLeftShape = leftLeaningShape (repeat 1)
basicRightShape = rightLeaningShape (repeat 1)
basicCenteredShape = centeredShape (repeat 1)

-- A slightly random, off-beat shape, repeating on 5 voices.
beginShape :: Shape
beginShape = Shape $ const (
  cycle [1,    (1/3),(1/2),(2/3),0    ],
  cycle [(1/2),(3/4),(2/3),(3/4),(1/2)]
  )

-- Another "kind of random" shape.
randShape :: Shape
randShape = Shape $ const (cycle [0,1/3,1/2,2/3], cycle [1/2,3/4,2/3])

-- Leaning with linear durations (also quite simple). Works for any number of parts.
topToFull :: Shape
topToFull       = compress 8 $ rightLeaningShape'  $ \n -> map ((+ 2) . (*) (4/12)) $ [1..fromIntegral n]
fullToTop       = compress 8 $ leftLeaningShape'   $ \n -> map ((+ 2) . (*) (4/12)) $ [1..fromIntegral n]
topToFullToTop  = compress 8 $ centeredShape'      $ \n -> map ((+ 2) . (*) (4/12)) $ [1..fromIntegral n]
bottomToFull    = compress 8 $ rightLeaningShape'  $ \n -> map ((+ 2) . (*) (4/12)) $ reverse $ [1..fromIntegral n]
fullToBottom    = compress 8 $ leftLeaningShape'   $ \n -> map ((+ 2) . (*) (4/12)) $ reverse $ [1..fromIntegral n]
bottomToFullToBottom 
                = compress 8 $ centeredShape'      $ \n -> map ((+ 2) . (*) (4/12)) $ reverse $ [1..fromIntegral n]

topToFull2       = compress 8 $ rightLeaningShape'  $ \n -> map ((+ 2) . (*) (4/12)) $ fmap ((*12).(^2).(/12)) $ [1..fromIntegral n]
topToFull3       = compress 8 $ rightLeaningShape'  $ \n -> map ((+ 2) . (*) (4/12)) $ fmap ((*12).(^3).(/12)) $ [1..fromIntegral n]
fullToTop2       = compress 8 $ leftLeaningShape'   $ \n -> map ((+ 2) . (*) (4/12)) $ fmap ((*12).(^2).(/12)) $ [1..fromIntegral n]
fullToTop3       = compress 8 $ leftLeaningShape'   $ \n -> map ((+ 2) . (*) (4/12)) $ fmap ((*12).(^3).(/12)) $ [1..fromIntegral n]

bottomToFull2    = compress 8 $ rightLeaningShape'  $ \n -> map ((+ 2) . (*) (4/12)) $ fmap ((*12).(^2).(/12)) $ reverse $ [1..fromIntegral n]
bottomToFull3    = compress 8 $ rightLeaningShape'  $ \n -> map ((+ 2) . (*) (4/12)) $ fmap ((*12).(^3).(/12)) $ reverse $ [1..fromIntegral n]

-- Linear phase, const duration
rhombusShape = compress 3 $ dependentShapeC id (const 1)
rhombusShape' d = compress 3 $ dependentShapeC (/ d) (const 1)
-- Linear phase and duration.
torsoShape = compress 3 $ dependentShapeC id (+1)

-- Every 5 pitches to left or right.
-- TODO this could be defined in terms of basicLeftShape and basicRightShape
-- TODO generally: more shape combinators
checkerShape :: Shape
checkerShape  = compress 2 $ Shape $ \n -> (cycle $ (>>= replicate 5) [0,1],repeat 1)

rectShape :: Shape
rectShape  = interpShape basicLeftShape basicRightShape 0.3

circShape :: Shape
circShape   = dependentShapeC (const 0.5) (realToFrac.circ'.toDouble)
  where
    circ' = (+ 0.001) . (* 2) . circ (0.5,0) 0.5
    {-
    r is radius
    (a,b) is center (0,0)
    (x,y) is any point in circle
    (x-a)^2 + (y-b)^2 = r2

    => (x-a)^2 + (y-b)^2 = r^2
    => (y-b)^2 = r^2 - (x-a)^2
    => (y-b) = sqrt(r^2 - (x-a)^2)
    => y = sqrt(r^2 - (x-a)^2) - b
    -}
    circ (a,b) r x = sqrt (r^2 - (x-a)^2) - b

bellShape :: Shape
bellShape    = dependentShapeC (const 0.5) ((+0.2).(*0.5).(+1).realToFrac.sin.(*((2*3.1415))).(+0.75).toDouble)
bellShape2   = dependentShapeC (const 0.5) ((+0.2).(*0.5).(+1).realToFrac.(*4).sin.(*((2*3.1415))).(+0.75).toDouble)
bellShapeL   = dependentShapeC (const 0) ((+0.2).(*0.5).(+1).realToFrac.sin.(*((2*3.1415))).(+0.75).toDouble)
bellShape2L  = dependentShapeC (const 0) ((+0.2).(*0.5).(+1).realToFrac.(*4).sin.(*((2*3.1415))).(+0.75).toDouble)
bellShapeSomewhatL   = dependentShapeC (const 0.1) ((+0.2).(*0.5).(+1).realToFrac.sin.(*((2*3.1415))).(+0.75).toDouble)
bellShape2SomewhatL  = dependentShapeC (const 0.1) ((+0.2).(*0.5).(+1).realToFrac.(*4).sin.(*((2*3.1415))).(+0.75).toDouble)
bellShapeR   = dependentShapeC (const 1) ((+0.2).(*0.5).(+1).realToFrac.sin.(*((2*3.1415))).(+0.75).toDouble)
bellShape2R  = dependentShapeC (const 1) ((+0.2).(*0.5).(+1).realToFrac.(*4).sin.(*((2*3.1415))).(+0.75).toDouble)

vaseShape :: Shape
vaseShape  = dependentShapeC (const 0.5) ((+0.2).(*0.5).(+1).realToFrac.cos.(*(2*3.1415)).(+0).toDouble)

slopeShape :: Shape
slopeShape = dependentShapeC ((*0.5).(+1).realToFrac.cos.(*(2*3.1415)).(+0).toDouble) (const 1)

allShapes :: Music
allShapes = stretch 4 $ pseqPad 1 $ fmap ({-stretchTo 1.-}renderFloater.flip makeFloater (pitchMat^.chord)) $ fmap (stretch 4) $
  [
  circShape,
  randShape,
  
  topToFull,
  fullToTop,
  topToFullToTop,
  -- bottomToFull,
  -- fullToBottom,
  -- bottomToFullToBottom,

  -- topToFull2,
  -- topToFull3,
  
  rhombusShape,
  torsoShape,
  checkerShape,
  rectShape,
  
  bellShape,
  -- bellShape2,
  bellShapeL,
  -- bellShape2L,
  -- bellShapeR,
  -- bellShape2R,
  -- bellShapeSomewhatL,
  -- bellShape2SomewhatL,
  
  vaseShape,
  slopeShape,
  -- stretch 1.5 vaseShape,
  -- stretch 1.5 slopeShape,

  combineShape checkerShape bellShape2
  -- combineShape rhombusShape fullToTop,
  
    -- basicRightShape,
  -- basicCenteredShape,
  -- basicLeftShape
  ]
  where
    -- pitchMat = otWithExtra
    -- pitchMat = enumDiatonicFromTo c c''
    pitchMat = enumDiatonicFromTo c__ c''
                        

diatonicField  = (takeEvery 1 (enumDiatonicFromTo c___ c'''))^.chord
  where
    takeEvery n []     = []
    takeEvery n (x:xs) = x : takeEvery n (drop (n - 1) xs)
diatonicField2 = (takeEvery 2 (enumDiatonicFromTo c___ c'''))^.chord
  where
    takeEvery n []     = []
    takeEvery n (x:xs) = x : takeEvery n (drop (n - 1) xs)
diatonicField3 = (takeEvery 3 (enumDiatonicFromTo c___ c'''))^.chord
  where
    takeEvery n []     = []
    takeEvery n (x:xs) = x : takeEvery n (drop (n - 1) xs)

vlowReg :: Chord Pitch -> Chord Pitch
vlowReg   = trimChord $ (c___, c_   )^.ambitus
lowReg    = trimChord $ (c__,  c    )^.ambitus
midReg    = trimChord $ (c_,   c'   )^.ambitus
highReg   = trimChord $ (c,    c''  )^.ambitus
xhighReg  = trimChord $ (c',   c''  )^.ambitus
xxhighReg = trimChord $ (c'',  c''' )^.ambitus
fullReg   = trimChord $ (c___, c''' )^.ambitus


{-
Construct the piece primarily out of 2 layers:
  2) A slow layer, consisting of a single voice, notes 20s-3min
  1) A number of fast layers, each consisting of a single voice (notes 1-3s)

  OR if you will
    The piece is made out of a [Aligned (Voice (Maybe (Floater StandardNote)))]
    The "slow" layer is l-aligned to 0 and stretches through the piece, the other
     layers are more local.
  
  Keep the slow layer simple (but coherent with the movement of the piece!)
-}
fast :: [Aligned (Voice (Maybe (Floater StandardNote)))]
fast = 
  mconcat [
  (set (parts'._instrument) clarinet . mconcat) [
    [
    -- aligned 10  center smalls,
    aligned 25  center smalls2, -- In winds
    aligned 37  center smalls3
    ],
    _8va [
    aligned 60   center (level pp dummy),
    aligned 70   center (_8va $ level pp dummy),
    aligned 80   center (_15va $ level pp dummy),
    aligned 90   center (_15va $ level pp dummy),
    aligned 100  center (_15va $ level pp dummy),
    aligned 110  center (_15va $ level pp dummy),
    aligned 120  center (_8va $ level pp dummy),
    aligned 130  center (_8va $ level pp dummy),
    aligned 140  center (_8va $ level pp dummy),
    aligned 150  center (_8vb $ level pp dummy),
    aligned 160  center (_15va $ level pp dummy)
    ],
    [
    aligned (recapOffset+23)  center smalls3,
    aligned (recapOffset+23+15)  center smalls2
    -- aligned (recapOffset+23+15+15)  center smalls
    ]
  ]
  ,
  (set (parts'._instrument) horn . delay 5) [
    aligned 60   center (dummy),
    aligned 70   center (_8vb $ dummy),
    -- aligned 80   center (dummy),
    -- aligned 90   center (dummy),
    -- aligned 100  center (dummy),
    -- aligned 110  center (dummy),
    aligned 120  center (dummy),
    aligned 130  center (_8vb $ dummy),
    aligned 140  center (dummy),
    aligned 150  center (_8vb $ dummy),
    aligned 160  center (_15vb $ dummy)
  ]
  ]
  
  where
    -- First section (all play "inside"), strings in mid reg -> winds mid reg -> strings full reg
    recapOffset = 210
    smalls2 = fmap (stretch 2) [
      1   *| rest,  1   *| norest circShape   (midReg diatonicField3),
      1   *| rest,  1.5 *| norest circShape   (highReg diatonicField3)
      ]^.voice
    smalls3 = fmap (stretch 2) [
      1   *| rest,  1   *| norest circShape   (lowReg diatonicField3),
      1   *| rest,  2.5 *| norest bellShape2  (highReg diatonicField2),
      1   *| rest,  1.5  *| norest circShape  (lowReg diatonicField3)
      ]^.voice
    dummy = fmap (stretch 8) [
      1   *| rest,  1   *| norest circShape   (lowReg diatonicField3)
      ]^.voice

-- slowHarmPart1_1 = midReg diatonicField2
-- slowHarmPart1_2 = midReg diatonicField3 <> highReg diatonicField3
-- slowHarmPart1_3 = lowReg diatonicField3 <> highReg diatonicField3
-- slowHarmPart1_4 = lowReg diatonicField3 <> highReg diatonicField2
-- slowHarmPart1_5 = lowReg diatonicField3
-- slowHarmPart2_1 = xxhighReg diatonicField <> highReg diatonicField
-- slowHarmPart2_2 = fullReg diatonicField3
-- slowHarmPart2_3 = slowHarmPart1_1 <> xhighReg diatonicField2
-- slowHarmPart2_4 = xxhighReg diatonicField <> vlowReg diatonicField3
-- slowHarmPart2_5 = fullReg diatonicField2
-- slowHarmPart2_6 = vlowReg diatonicField3
                                                               
slowHarmPart1_1 = midReg (chords5!!1)                             -- Dbl bass and vla
slowHarmPart1_2 = midReg (chords5!!2) <> highReg (chords5!!2)
slowHarmPart1_3 = lowReg (chords5!!2) <> highReg (chords5!!2)     -- Vlns group 1
slowHarmPart1_4 = lowReg (chords5!!2) <> highReg (chords5!!1)     -- Vlns group 2
slowHarmPart1_5 = lowReg (chords5!!2)

slowHarmPart2_1 = xxhighReg (chords1!!6) <> highReg (chords1!!6)  -- The harm rise
slowHarmPart2_2 = chords1!!3                                      -- The harm fall

slowHarmPart2_6 = vlowReg (chords1!!6)                            -- Before middle section
slowHarmPart2_3 = slowHarmPart1_1 <> xhighReg (chords1!!6)        -- Big middle section
slowHarmPart2_4 = xxhighReg (chords1!!6) <> vlowReg (chords1!!6)
slowHarmPart2_5 = fullReg (chords1!!6)

{-
Most important chords:
  chords1!!6
  chords1!!3 -- both used at climax
  
  chords5!!1
  chords5!!2
  
    Secondary (slightly nicer)
      chords1!!5
      chords1!!4
    Secondary (very dissonant)
      chords5!!3
      chords5!!4
      chords5!!6
      chords5!!5
-}

slow :: Aligned (Voice (Maybe (Floater StandardNote)))
slow = aligned 0 left $ stretch 6 sl
  where
    sl = mconcat [
        level _p [
          -- Db+Vc harm (inter: ord)
          0   *| rest,  2   *| norest circShape (slowHarmPart1_1),
          -- WW (inter: str ord)
          0   *| rest,  1   *| norest circShape (slowHarmPart1_2),
          -- Str ord tasto (inter: W)
          
          0   *| rest,  1.5 *| norest circShape (slowHarmPart1_3),
          -- Str ord tasto (inter: solo groups, nat/pont)
          0   *| rest,  3.5 *| norest basicCenteredShape (slowHarmPart1_1),
          0   *| rest,  1.5 *| norest circShape (slowHarmPart1_4),
          
          -- Str ord tasto+WW (inter: muted brass)
          0   *| rest,  2   *| norest fullToBottom (slowHarmPart1_5)
        ]^.voice,

        level pp [
          -- Str harm
          1   *| rest,  5.5  *| (level pp $ norest fullToTop (slowHarmPart2_1))
        ]^.voice,
        level _f [
          -- Str harm/ord
          0   *| rest,  4   *| norest (rhombusShape' 0.2) (slowHarmPart2_2),
          -- Vc+Db ord
          0   *| rest,  2.5 *| norest bottomToFull (slowHarmPart2_6),
          -- Brass+str?
          0   *| rest,  5   *| norest bottomToFull (slowHarmPart2_3),
          0   *| rest,  2.5 *| norest basicCenteredShape (slowHarmPart2_4),
          0   *| rest,  4   *| norest fullToBottom (slowHarmPart2_5)
          ]^.voice,

          level mp [
          0   *| rest,  1.5 *| norest circShape (slowHarmPart1_3),
          0   *| rest,  3.5 *| norest basicCenteredShape (slowHarmPart1_1),
          0   *| rest,  1.5 *| norest circShape (slowHarmPart1_4),
          0   *| rest,  2   *| norest fullToBottom (slowHarmPart1_5),
          0   *| rest,  2   *| norest circShape (slowHarmPart1_1),
          0   *| rest,  2   *| norest circShape (slowHarmPart1_1)
        ]^.voice
      ]

renderLayer :: (HasParts' a, PartOf a ~ Part) => Aligned (Voice (Maybe (Floater a))) -> Score a
renderLayer = tempo (metronome (1/4) 72) 
  . join . mcatMaybes 
  . renderAlignedVoice . fmap3 (set era (0<->1) . renderFloater)
  where fmap3 = fmap.fmap.fmap
    
norest :: Shape -> Chord Pitch -> Note (Maybe (Floater StandardNote))
norest x y = pure $ Just $ makeFloater x y

fast', slow', piece :: Music
fast' = pcat $ fmap renderLayer fast
slow' = renderLayer slow
piece = slow' <> fast'
example = piece





fmChords :: [[Hertz]]
fmChords = 
  -- map fmChord' [2,3,5,6,1.1232]
  map fmChord [_M3,m3,_A4,_A5,_P5,_M2,m2]
  where
    fmChord' f = fmap fdiff [1..9] `merge` fmap fsum [1..9]
      where
        carr = 392                                :: Hertz
        modu = carr*f    :: Hertz
        fsum  i = abs $ carr + modu*(fromInteger i)
        fdiff i = abs $ carr - modu*(fromInteger i)
    fmChord i = fmap fdiff [1..9] `merge` fmap fsum [1..9]
      where
        carr = 392                                :: Hertz
        modu = carr*((getIntonation $ intone (c,1) justT) (c.+^ i)) :: Hertz
        fsum  i = abs $ carr + modu*(fromInteger i)
        fdiff i = abs $ carr - modu*(fromInteger i)

                                                                        


chords1 :: [Chord Pitch]
chords1 = fmap (^.chord) $ [[c___,c__,g__,d_,bb_,g,a,bb,c',f',d'',ds'',e'',f'',fs'',g'',gs''],[c___,c__,g__,d_,bb_,g,a,bb,c',f',d'',ds'',gs'',e'''],[c___,c__,g__,d_,bb_,g,a,bb,c',f',d'',ds'',f'',gs'',e''',fs''',g'''],[c___,c__,g__,d_,bb_,c,f,g,c',ds'',f'',fs'',gs'',d''',e''',g'''],[c___,c__,g__,d_,bb_,c,f,g,c',d'',ds'',gs'',e'''],[f___,bb___,c__,f__,c_,f_,g_,c,d',g',a',d''],[c__,c_,ab_,bb_,c,c',g',e'',f'',g'',db'''],[c__,c_,ab_,bb_,c,fs,cs',as',b',cs'',g''],[a___,a__,e_,f_,a_,fs,cs',as',b',cs'',g''],[c__,db__,e__,gb__,c_,g_,e,g,f',fs'']]
-- chords2 :: [Chord Pitch]
-- chords2 = fmap (^.chord) $ [[c___,c__,a__,fs_,d,bs],[c___,c__,a__,fs_,d,c',a'],[c___,c__,a__,fs_,d,c',gs'],[c___,c__,a__,c_,ab_,gb,d']]
-- chords3 :: [Chord Pitch]
-- chords3 = fmap (^.chord) $ [[c___,c__,a__,fs_,d,c'],[c___,c__,a__,fs_,d,c',a'],[c___,c__,a__,fs_,d,c',gs'],[c___,c__,a__,c_,ab_,gb,d'],[c__,ab__,eb_,c,c,e,gs,d',fs',as'],[c__,gb__,eb_,a_,as,cs',e',g'],[c__,gb__,eb_,a_,c,as,cs',e',g'],[c__,gb__,c_,a_,eb,cs',e',g',as'],[c__,gb__,eb_,c,a,e',g',as',cs''],[c__,gb__,eb_,c,a,g',as',cs'',e'']]
-- chords4 :: [Chord Pitch]
-- chords4 = fmap (^.chord) $ [[c_,d_,e_,f_,g_,a_],[gb_,ab_,bb_,cb,db,eb],[c,d,e,f,g,a],[gb,ab,bb,cb',db',eb'],[c',d',e',f',g',a'],[gb',ab',bb',cb'',db'',eb''],[gb_,g_,ab_,a_,bb_,cb,c,db,d,eb,e,f,gb,g,ab,a,bb,cb',c',db',d',eb',e',f',gb',g',ab',a',bb',cb'',db'',eb''],[gb_,g_,a_,bb_,c,d,e,gb,g,ab,bb,cb',d',e',f',gb',g',bb',cb'',eb''],[ab_,cb,db,eb,f,a,c',db',eb',ab',ab',db''],[gb_,g_,ab_,a_,bb_,cb,c,db,d,eb,e,f,gb,g,ab,a,bb,cb',c',db',d',eb',e',f',gb',g',ab',a',bb',cb'',db'',eb''],[gb,g,ab,a,cb'',db'',eb''],[gb_,g_,ab_,a_,bb_,cb,c,db,d,eb,e,f,bb,cb',c',db',d',eb',e',f',gb',g',ab',a',bb'],[ab__,bb__,cb_,db,d,eb,g,ab,ab,a,bb,db'',eb''],[gb_,g_,a_,c,db,d,eb,e,f,gb,bb,cb',c',e',f',gb',g',a',cb'']]
chords5 :: [Chord Pitch]
chords5 = fmap (^.chord) $ fmap2 (octavesDown 2) $ fmap2 (^.from pitchHertz) fmChords

allChords = pseq $ fmap (uncurry addText . fmap inspectableToMusic) [
  ("chords1",chords1),
  -- ("chords3", chords3),
  -- ("chords4", chords4),
  ("chords5",chords5)
  ]




{-
Randomness idea:
>>> openMusicXml$  set (parts'._instrument) violin $ rcat $ simplifyPitches $ take 40 $ zipWith (flip transform) (fmap (\x-> up (_M2^*(floor$x*16)) (c::Music))(drop 205 rands)) $ fmap (\x->(realToFrac$floor$x*(16*2))/16<->32/16) rands
-}




orch = level mf $ flip doubleParts c $ orchParts
orchParts :: [Part]
orchParts = [flutes1,flutes2,flutes3,oboes1,oboes2,{-"tutti"-}tutti corAnglais,clarinets1,clarinets2,clarinets3,bassoons1,bassoons2,bassoons3]
  <> divide 4 horns <> divide 3 trumpets <> divide 3 trombones <> [tutti tuba]
  <> [timp,hrp,cel]
  <> divide 2 violins <> [violas] <> [cellos] <> [doubleBasses]
[flutes1,flutes2,flutes3] = divide 3 flutes
[oboes1,oboes2,oboes3] = divide 3 oboes
[clarinets1,clarinets2,clarinets3] = divide 3 clarinets
[bassoons1,bassoons2,bassoons3] = divide 3 bassoons
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














-- {-
-- This is a "recursive" composition of floaters, the top layer being faster than the bottom layer.
-- It has a nice shape to it, but is also *extremely* regular. Also the total lack of phasing differences
-- (based on freq relations rather than phase relations) means it has a tendency to thin out/fatten.
-- Commbine with phasing somehow?
-- 
-- TODO how to rewrite the expression below to allow for that kind of variation? 
-- -}
-- recursiveFloaters :: Music
-- recursiveFloaters = 
--   startAt 0 $
--   set (parts'._instrument) violin $ mconcat $ sendToSubParts [
--     octavesUp ( 2) $ (>>= renderFloater) $ renderAlignedVoice $ aligned 0 0.5 $ beginning 100 $ stretch (2^1) $ timesN' 100 $ layer1,
--     octavesUp ( 1) $ (>>= renderFloater) $ renderAlignedVoice $ aligned 0 0.5 $ beginning 100 $ stretch (2^2) $ timesN' 100 $ layer2,
--     octavesUp ( 0) $ (>>= renderFloater) $ renderAlignedVoice $ aligned 0 0.5 $ beginning 100 $ stretch (2^3) $ timesN' 100 $ layer3,
--     octavesUp (-1) $ (>>= renderFloater) $ renderAlignedVoice $ aligned 0 0.5 $ beginning 100 $ stretch (2^4) $ timesN' 100 $ layer4
--   ]
--   where
--     sendToSubParts xs = let m = length xs in zipWith (\n ->  prependDiv n m) [0..m] xs
--     prependDiv n m =  over (parts'._subpart._Wrapped') (division n m:)
--     
--     layer1 :: Voice (Floater StandardNote)
--     layer1 = [pure flo1,rr,pure flo1,rr,pure flo1,rr,pure flo1,rr]^.voice
--     layer2 = [pure flo1,rr,pure flo1,rr,pure flo1,rr,pure flo1,rr]^.voice
--     layer3 = [pure flo1,rr,pure flo1,rr,pure flo1,rr,pure flo1,rr]^.voice
--     layer4 = [pure flo1,rr,pure flo1,rr,pure flo1,rr,pure flo1,rr]^.voice
-- 
--     flo1 = makeFloater beginShape [c,d,e,f,g]
--     -- Empty floater to fill out space between other floaters
--     rr = stretch 0.5 (pure mempty)
--     
-- 
{-
This sounds very nice, but the time structure is much to regular!
REMEMBER TO DO LATER: copy layers/recursiveFloaters and experiment with varying durations in the layers, alternate floaters/pitch material
and less repetition in the layers.

ANOTHER THING
A single layer of really small, irreggular floaters (chamber orchenstrations/subparts of the orchestra).

ANOTHER THING
Come up with some *really big* floaters (will be orchestrated as tuttis).
Think more about pitch material.
-}


-- -- otWithExtra = (scaleToList $ modeToScale c__ ot) <> enumChromaticFromTo c'' c'''
-- 
-- bigFlo1NS = makeFloater randShape $ otWithExtra
-- 
-- bigFlo1  = makeFloater (stretch 15 randShape) $ otWithExtra
-- bigFlo2  = makeFloater topToFull $ otWithExtra
-- bigFlo1a = makeFloater (stretch 15 randShape) $ reverse $ otWithExtra
-- bigFlo2a = makeFloater topToFull $ reverse $ otWithExtra
-- bigFlo3  = makeFloater fullToTop $ reverse $ otWithExtra
-- bigFlo4  = makeFloater topToFullToTop $ reverse $ otWithExtra
-- bigFlo3a = makeFloater fullToTop $ otWithExtra
-- bigFlo4a = makeFloater topToFullToTop $ otWithExtra
-- 
-- -- Interpolate between different shapes
-- example2 = pseq $ fmap (renderFloater.flip makeFloater [c..c'']) $ fmap (interpShape randShape topToFull) [0,0.1..1]
-- -- ... pitches
-- example3 = pseq $ fmap (renderFloater.makeFloater basicLeftShape) $ fmap (interpPitches [c,e',g'] [f,d',a']) [0,0.1..1]
-- 
-- example = mempty
--   |> pseqPad 5 [
--      renderFloater bigFlo1,
--      renderFloater bigFlo2,
--      renderFloater bigFlo3,
--      renderFloater bigFlo4,
-- 
--      renderFloater bigFlo1a,
--      renderFloater bigFlo2a,
--      renderFloater bigFlo3a,
--      renderFloater bigFlo4a
--   ]
--   |> (pseq $ zipWith stretch (take 5  [1.5^x | x<-[1..] ]) $ repeat $ renderFloater bigFlo1NS)
--   |> (pseq $ zipWith stretch (take 10 [1.1^x | x<-[1..] ]) $ repeat $ renderFloater bigFlo2)
--   |> (renderFloater bigFlo3 <> recursiveFloaters)
--   |> renderFloater bigFlo3
-- 

 
{-
-- Show chords in ascending order of dissonance. 
showDiss :: [Chord Pitch] -> Music
showDiss = asScore . pseq . fmap (\ps -> addText (take 12 $ show $ chordDiss ps) $ pcat $ fmap fromPitch'' ps) 
  . Data.List.sortBy (Data.Ord.comparing chordDiss) . fmap (^.from chord)

-- Example
disses :: [Chord Pitch]
disses = fmap (^.chord) [
  [c,d,e],
  [c,e,g,bb],
  [c,e,g,bb,d'],
  [c,e,g,bb,d',fs'],
  [c,e,gs,b],
  [c,e,gs,bb],
  [ab_,c,fs],
  [c,eb,gb,b],
  [c,b,d',f'],
  [c,e,g,bb,db],
  [c,e,gb,bb],
  [e,b,db,f,ab],
  chordToList $ functionToChord c majorTriad,
  chordToList $ functionToChord a minorTriad,
  chordToList $ functionToChord c augmentedChord,
  chordToList $ functionToChord c diminishedChord,
  chordToList $ functionToChord b_ diminishedChord,
  chordToList $ functionToChord c halfDiminishedChord,
  chordToList $ functionToChord e halfDiminishedChord,
  chordToList $ functionToChord c majorMinorSeventhChord,
  chordToList $ functionToChord c majorMajorSeventhChord,
  
  interpPitches (chordToList $ functionToChord c augmentedChord) (chordToList $ functionToChord fs' halfDiminishedChord) (0/12),
  interpPitches (chordToList $ functionToChord c augmentedChord) (chordToList $ functionToChord fs' halfDiminishedChord) (1/12),
  interpPitches (chordToList $ functionToChord c augmentedChord) (chordToList $ functionToChord fs' halfDiminishedChord) (2/12),
  interpPitches (chordToList $ functionToChord c augmentedChord) (chordToList $ functionToChord fs' halfDiminishedChord) (3/12),
  interpPitches (chordToList $ functionToChord c augmentedChord) (chordToList $ functionToChord fs' halfDiminishedChord) (4/12),
  interpPitches (chordToList $ functionToChord c augmentedChord) (chordToList $ functionToChord fs' halfDiminishedChord) (5/12),
  interpPitches (chordToList $ functionToChord c augmentedChord) (chordToList $ functionToChord fs' halfDiminishedChord) (6/12),
  interpPitches (chordToList $ functionToChord c augmentedChord) (chordToList $ functionToChord fs' halfDiminishedChord) (7/12),
  interpPitches (chordToList $ functionToChord c augmentedChord) (chordToList $ functionToChord fs' halfDiminishedChord) (8/12),
  interpPitches (chordToList $ functionToChord c augmentedChord) (chordToList $ functionToChord fs' halfDiminishedChord) (9/12),
  [c]
  ]

-}

-- 
-- -- Approximation of the first 17 notes of the harmonic series
-- ot :: Mode Pitch
-- ot = modeFromSteps ([_P8,_P5,_P4,_M3,m3,m3,_M2,_M2,_M2,_M2,m2,_A1,m2,_A1,m2,m2]) _P1
-- {-
-- 
-- -}
-- 
-- otTree :: Tree Pitch
-- otTree = unfoldTree (\p -> (p,filter (\p2 -> p < p2 && p2 < c''') $ drop 2 $ scaleToList $ modeToScale p m)) (octavesDown 5 c)
--   where
--     m = ot
--     -- m = invertMode 1 ot
-- 
-- treeToScore :: Tree a -> Score a
-- treeToScore (Node x xs) = pure x |> (compress 1 $ pseq $ fmap treeToScore xs)
-- 
-- showOt = putStr $ drawTree $ fmap show otTree
-- sibOt  = openMusicXml $ asScore $ compress 32 $ treeToScore $ fmap fromPitch'' $ otTree

-- 
-- tutti1 = sortBy (comparing (highestPitch.instrumentRange.view _instrument)) $ tuttiParts
-- tutti2 = sortBy (comparing (lowestPitch.instrumentRange.view _instrument)) $ tuttiParts
-- tutti3 = sortBy (comparing (lowestPitch.instrumentRange.view _instrument)) $ takeEvery 3 tuttiParts
-- tuttiProb = [piccoloFlutes,altoFlutes,ebClarinets,bassClarinets,contraBassoons]
-- 
-- tuttiParts :: [Part]
-- tuttiParts = mconcat
--     [
--     divide 12 violins1,
--     divide 10 violins2,
--     divide 8  violas,
--     divide 6  cellos,
--     divide 4  doubleBasses,
--     
--     divide 2  piccoloFlutes,
--     divide 1  flutes,
--     divide 3  oboes,
--     divide 1  ebClarinets,
--     divide 2  clarinets,
--     divide 3  bassoons,
--     divide 4  horns,
--     divide 3  trumpets,
--     divide 3  trombones,
--     divide 1  tubas
--     ]
-- tuttiStrings = filter (isStringInstr.view _instrument) tuttiParts
-- 
-- floaters = [
-- 
--   stretch 1 $ makeFloater' randShape ((chords5!!0)) tutti1,
--   stretch 1 $ makeFloater' topToFull ((chords5!!1)) tutti2,
--   stretch 1 $ makeFloater' randShape ((chords5!!2)) tutti2,
--   stretch 1 $ makeFloater' topToFull ((chords5!!3)) tutti3,
--   stretch 1 $ makeFloater' fullToTop ((chords5!!4)) tutti3, -- nice trans:
--   stretch 1 $ makeFloater' torsoShape ((chords5!!5)) tutti1,
-- 
--   
--   stretch 1 $ makeFloater' torsoShape ((chords4!!0)) tutti2,
--   stretch 1 $ makeFloater' torsoShape ((chords4!!1)) tutti2,
--   stretch 1 $ makeFloater' checkerShape ((chords4!!2)) tutti1,
--   stretch 1 $ makeFloater' rectShape ((chords4!!3)) tuttiParts,
--   stretch 1 $ makeFloater' rectShape ((chords4!!4)) tutti2,
--   stretch 1 $ makeFloater' rectShape ((chords4!!5)) tutti2,
--   mempty
--   ]
-- 
-- floaters2 = [  
--   stretch 1 $ makeFloater' torsoShape ((chords1!!5)) tutti2,
--   stretch 1 $ makeFloater' torsoShape ((chords1!!5)) tuttiParts,
--   stretch 1 $ makeFloater' checkerShape ((chords1!!5)) tuttiParts,
--   stretch 1 $ makeFloater' rectShape ((chords1!!5)) tuttiParts,
--   stretch 1 $ makeFloater' randShape ((chords1!!5)) tutti2,
--   
--   
--   stretch 1 $ makeFloater' topToFull ((chords3!!0)) tutti3,
--   stretch 1 $ makeFloater' fullToTop ((chords3!!1)) tutti3,
--   stretch 1 $ makeFloater' topToFullToTop ((chords3!!2)) tutti3,
--   stretch 1 $ makeFloater' topToFull ((chords3!!3)) tutti3,
--   stretch 1 $ makeFloater' fullToTop ((chords3!!4)) tutti3,
--   stretch 1 $ makeFloater' topToFullToTop ((chords3!!5)) tutti3,   
-- 
--   mempty
--   ]
-- 
-- {-
-- List of all harmony
-- Good stuff in here but needs cleaning/simplifying
-- -}
-- floaters3 = [
--   stretch 1 $ makeFloater' basicCenteredShape ((chords1!!0)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords1!!1)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords1!!2)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords1!!3)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords1!!4)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords1!!5)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords1!!6)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords1!!7)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords1!!8)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords1!!9)) tuttiStrings,
-- 
--   stretch 1 $ makeFloater' basicCenteredShape ((chords3!!0)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords3!!1)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords3!!2)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords3!!3)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords3!!4)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords3!!5)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords3!!6)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords3!!7)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords3!!8)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords3!!9)) tuttiStrings,
-- 
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!0)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!1)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!2)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!3)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!4)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!5)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!6)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!7)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!8)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!9)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!10)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!11)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!12)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords4!!13)) tuttiStrings,
-- 
--   stretch 1 $ makeFloater' basicCenteredShape ((chords5!!0)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords5!!1)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords5!!2)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords5!!3)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords5!!4)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords5!!5)) tuttiStrings,
--   stretch 1 $ makeFloater' basicCenteredShape ((chords5!!6)) tuttiStrings,
-- 
--   mempty
--   ]
-- 

testTutti :: [Part] -> Chord Pitch -> Music
testTutti tutti ch =
  asScore $ pcat $ zipWith (set parts') tutti (map fromPitch'' $ pitchMaterial)
  where
    pitchMaterial = ch^.from chord

testTuttis :: [Chord Pitch] -> [[Part]] -> [Music]
testTuttis chords tutti = liftA2 testTutti tutti chords

  





dss :: [Duration]
dss = [1,1,2,1] ++ (tail dss) `merge` (init dss)
vs :: [Voice Pitch]
vs = fmap (takeV 100 . flip dropV bv) [1,2,4,5,1]
  where bv = mconcat $ zipWith stretch dss (cycle [c,d::Voice Pitch]) |/ 8





