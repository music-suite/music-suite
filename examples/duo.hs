
module Main where

import Music.Prelude

toLydian :: Music -> Music
toLydian = pitches' %~ (\p -> if p == c then cs else p)

subj1 = (|/2) $
    (legato.accent) (b_ |> c) |> (legato.accent) (c |> b_|*2)
        |> legato (pseq [b_, c, d])
        |> b_ |> c |> b_|*2
    |> legato (pseq [e, d, b_, c]) |> b_|*2
    |> pseq [d, e, b_] |> c|*2 |> b_

pres1 = subj1|*(2/2)
pres2 = subj1|*(2/2) </> delay 2 (subj1|*(3/2))

part1 = pres1 |> pres2
part2 = pres1 |> pres2

music = level pp $ compress 2 $ part1 |> toLydian part2

main :: IO ()
main = defaultMain music
