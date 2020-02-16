
{-# LANGUAGE OverloadedStrings #-}

import Music.Prelude
import Control.Lens (set, over)

-- A simple subject
subj :: Music
subj  = times 2 $ pseq [c,c,d,b_,e,e]|/16

-- Each voice differ slighly in onset etc
voca :: Duration -> Music
voca v = delay ((4/8)|*v) $ removeRests $ pseq $ fmap (\i -> (id $ up (_M2^*i) subj) |> rest|*(15/8))
  $ [0..3]

-- The
music = id
  $ title "Phrases"
  $ composer "Anonymous"
  $ timeSignature (3/8)
  $ timeSignatureDuring ((14*3/8) <-> 200) (4/8)
  $ over phrases fuse
  $ over phrases (rotateValues 1)
  $ rcat
  $ map voca [0..3]

main  = defaultMain music

rotateValues :: Int -> Voice a -> Voice a
rotateValues n x = view voice $ fmap (view note) $ zip ds vs
  where
    (ds, vs) = unzip $ fmap (view $ from note) $ view notes x

rotate :: Int -> [a] -> [a]
rotate n xs = iterate rotate1 xs !! n
  where
    rotate1 [] = []
    rotate1 xs = last xs : init xs

