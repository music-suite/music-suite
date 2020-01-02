
{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleContexts #-}

module Main where

import Music.Prelude
import qualified Data.List as List
import Util

{-
Canon texture: harmonics. Intersperesed with very high/soft wind and percussion textures.
Eventually move down: double bass harmonics <> horns etc.
-}

music = text "Hello!" c

{-
Harmonics to use:
  Vl:  G:1,2,3  D::1,2,3 A:1,2,3 E:1,2,3
  Vla: C:1,2,3,4,5 G:1,2,3,4,5 D:1,2,3,4,5 A:1,2,3,4,5
  Vc:  C:1,2,3,4,5,6 G:1,2,3,4,5,6 D:1,2,3,4,5,6 A:1,2,3,4,5,6
  Db:  E:1,2,3,4,5,6,7,8 A:1,2,3,4,5,6 D:1,2,3,4,5,6 G:1,2,3,4,5,6
-}

higestHarmonic i
  | i == violin = 3
  | i == viola = 4
  | i == cello = 6
  | i == doubleBass = 7

availablePitches :: [Pitch]
availablePitches = 
  List.nub $ List.sort $ concat $ liftA2 (\i n -> fmap (harmonicPitch i n) [0..higestHarmonic i]) [violin, viola, cello, doubleBass] [0..3]

-- TODO build overlapping thing...
canon1 :: Voice (Instrument, StringNumber, HarmonicNumber)
canon1 = 
  [ pure (doubleBass,1,7) -- a
  , pure (cello,0,6) -- bb
  , pure (doubleBass,3,4) -- b
  , pure (cello,0,7) -- b OR viola
  ]^.voice

canon1' = fmap (\(i,n,h)->harmonicPitch i n h) canon1





  
  
  