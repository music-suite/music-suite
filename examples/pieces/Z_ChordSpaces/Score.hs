
module Main where

import Music.Prelude
import Util

{-
  Ways to generate and organize harmony:

    1) Discrete:
      Start out from a single tonic and/or chord, build related chords from that,
      thereby generating/unfolding a graph. Compare Hindemith COMC, Rameaux TdlH etc.

    2) Continous:
      Define a 2D (or 3D) harmonic space. Create parts by building trajectories
      in that space. Orchestrate for some odd harmonic intstrument combinations
      (i.e. 2 pianos, 2 harps, 4 steel guitars or similar).
-}
main = defaultMain music
music = text "Hello!" c
