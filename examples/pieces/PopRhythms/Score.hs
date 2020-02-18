{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Music.Prelude hiding (snareDrum)
import Util
{-
Encoding of standard popular-music rhythms

Sources:
  http://www.drumlessons.com/drum-lessons/live-lessons-archive/7-essential-drum-beats/
  etc.
-}

music = fmap toNote basic1

-- TODO proper percussion support
openHiHat = d'
closeHiHat = d
hiHat = g'
bassDrum = f
snareDrum = c'

basic1 = toPattern
  [ "xxxxxxxx"
  , "b s b s " ]
basic1WithOpenClose = toPattern
  [ "+o+o+o+o"
  , "xxxxxxxx"
  , "b s b s " ]

toPattern :: [String] -> Score Pitch
toPattern patterns = compress (fromIntegral $ maximum $ fmap length patterns) $ ppar $ fmap (removeRests . pseq . fmap g) patterns
  where
    g ' ' = rest
    g 'x' = hiHat
    g 'b' = bassDrum
    g 's' = snareDrum



toNote = fromPitch
main = defaultMain music
