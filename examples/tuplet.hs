
{-# LANGUAGE OverloadedStrings #-}

-- This example shows tuplets of various ratios and durations.
module Main where

import Music.Prelude

music :: Music
music = rcat
  [ group 5 g |> (g|*3)
  , group 3 fs |> (fs|*3)
  , group 3 f |> group 5 f |> group 3 f |> group 7 f
  , group 3 e |> group 5 e |> c |> group 7 e
  , ds |> group 5 ds |> ds |> group 9 ds
  , times 2 $ group 5 d |> group 3 d
  , stretch 2 (group 5 d |> group 3 d)
  , stretch 1.5 (group 5 d |> group 3 d)
  , stretch 0.5 $ times 4 $ group 5 d |> group 3 d
  , times 4 cs|/5 |> cs|*(1/5+1/3) |> d|*2
  , group 5 c |> group 3 c |> c|*2
  ]

main :: IO ()
main = defaultMain music

