{-# LANGUAGE OverloadedStrings #-}

-- |
-- Simple canon on a familiar subject.
--

module Main where

import Music.Prelude
import Control.Lens (set)

frere :: Music
frere = mempty
  |> times 2 (pseq [c,d,e,c]|/4)
  |> times 2 (pseq [e,f,g|*2]|/4)
  |> times 2 (pseq [g,a,g,f,pseq [e,c]|*2]|/8)
  |> times 2 (pseq [c,g_,c|*2]|/4)

frere2 = delay 2 frere <> frere
frere4 = delay 4 frere2 <> frere2

info = title "Frere Jaques" . composer "Trad." . tempo (metronome (1/4) 120)
music = info frere4

main = defaultMain music

