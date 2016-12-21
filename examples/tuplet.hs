
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Music.Prelude

test 1  = group 5 g |> (g|*3)                  
test 2  = group 3 fs |> (fs|*3)                  
test 3  = group 3 f |> group 5 f |> group 3 f |> group 7 f
test 4  = group 3 e |> group 5 e |> c |> group 7 e
test 5  = ds |> group 5 ds |> ds |> group 7 ds
-- all above ok
test 8 = times 5 d|/5 |> times 3 d|/3 -- ok
test 9 = times 4 cs|/5 |> cs|*(1/5+1/3)    -- not ok, needs to be bound from last quintuplet note

test 99 = group 5 c |> group 3 c |> c|*2


group n x = times n x |/ fromIntegral n

music :: Music
music = {-fadeIn 1 $-} {-fadeOut 1 $-} catSep $ map test [1..5]

main :: IO ()
main = open music

catSep = pcat . zipWith (set parts') (divide 100 mempty)
