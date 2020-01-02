
module Main where

import Music.Prelude
import Util

{-
  Represent a melody in abstract form as contour and scale.
  Realize a single contour over various scales.
  Extract pitch material from one melody and move to another etc.
  
  Compare Parsons Code for Melodic Contours
    
  
  ......
  
  Possibly different idea
    Melody as an hierarchical structure
    Certain "more important" notes, other points as decorations of these

    Appogiatura:
    (b)c
    Acc??tura:
    c(d c)
    Passing notes:
    c (d e f) g
    
    On one level
     c (g fs)g (a e d)c'
    And so on
     c (d e f) (g fs)g (a e d)c'
     c ((b_ c)d (eb) e f) (g(d b_ d g b d' b g) (f) fs)g (a(e d ds e bb a) e(ds d cs c b e) d)c'
    
-}
music = text "Hello!" c