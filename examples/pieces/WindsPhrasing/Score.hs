
{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleContexts #-}

module Main where

import Music.Prelude
import Util

main = openBook music

{-
Work with basic phrases and dynamics in winds.
Set off against long pedals in strings.

Possibly:
  - Work more with dynamics
  - Instead of just repeating (times 5), use repetition with variation (Int -> a).
  
-}
x = removeRests $ compress 16 $ times 10 $ ((times 5 $ legato $ (scat [e,f,e,ds]) |> scat[g,f,e,ds]) |> rest^*2)

fls  = up   _P12 $ set (parts'._instrument) flute    $ tempo presto (ucat [x,delay 2.5 x,delay 5 x])
obs  = up   _P8  $ set (parts'._instrument) oboe     $ tempo presto (ucat [x,delay 4 x,delay 5.5 x])
cls  = down _P1  $ set (parts'._instrument) clarinet $ tempo presto (ucat [x,delay 3 x,delay 5 x])
bsns = down _P5  $ set (parts'._instrument) bassoon  $ tempo presto (ucat [x,delay 3 x,delay 7 x])

-- music = cls `sustain` set parts' violins1 (level pp b')
music = 
  delay 6 (fls `sustain` set parts' violas (level pp b'))
    <>
  delay 12 (obs `sustain` set parts' violins2 (level pp b'))
    <>
  delay 0 (cls `sustain` set parts' violins1 (level pp b'))
    <>
  delay 18 (bsns `sustain` set parts' cellos (level pp b'))
    <>
  mempty





