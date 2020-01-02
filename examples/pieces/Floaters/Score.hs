
{-# LANGUAGE FlexibleInstances, TypeFamilies, DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Main where

import Music.Prelude
import qualified Music.Score as S
import Util

{-
By "floater" i mean a chord which in which the elements/notes are not
fully aligned. Gradual change of texture etc.

TODO make a 10-15 minute piece:
  - Just floaters (as floater1 etc)
  - Orchestrate in strings/winds and various combinations
  - Also work with dynamics
  - Use sequencing and overlapping
  
  - Everything is quantized to 1/4-notes to keep things simple
-}
testFloater :: Floater StandardNote -> IO ()
testFloater x  = displayAndAudify $ stretch 2 $ (fmap) (set (parts'._instrument) violin) $ x
testFloater' x                    = stretch 2 $ (fmap) (set (parts'._instrument) violin) $ renderFloater' $ x




floater1 :: IsPitch a => Floater a
floater1 = Floater $ zipWith (aligned 0) [0,0.5,0.75] [c,d,e,f,g,a,b,c']

floater2 :: IsPitch a => Floater a
floater2 = Floater $ zipWith (aligned 0) 
  (fmap (/ 8) [1,2,5,6,1,7,2,3])
  [c,d,e,f,g,a,b,c']

floater3 :: IsPitch a => Floater a
floater3 = Floater $ zipWith (aligned 0) 
  (fmap (/ 8) [6,1,2,5,1,7,2,3])
  (cycle [gs,ds,as])



music = text "Hello!" c