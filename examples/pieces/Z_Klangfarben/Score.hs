
{-# LANGUAGE TypeFamilies #-}

module Main where

import Music.Prelude
import qualified Music.Score
import Util

{-
Klangfarben melodien...
-}

-- main = displayAndAudify music
main = displayAndAudify $ music2

music2 = compress 4 $ pcat $ map renderAlignedVoice $
  times 2 (pure $ aligned 0 0 $ music1)
  
music1 = asVoice $ klangfarben 
  [violins,flutes,oboes,trumpets,tubas,clarinets,trombones,doubleBasses]
  (mconcat [c,e^*3,fs,g,a^/2,gs'^*3,g',fs',as,b,cs])

klangfarben :: HasParts' a => [Music.Score.Part a] -> Voice a -> Voice a
klangfarben ps v = (^.voice) $ zipWith (set parts') ps (v^.notes)

