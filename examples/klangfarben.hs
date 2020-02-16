
{-# LANGUAGE TypeFamilies #-}

module Main where

import Music.Prelude
import qualified Music.Score

{-
Klangfarben melodien...
-}

-- main = displayAndAudify music
main = defaultMain music2

music2 = compress 4 $ ppar $ map renderAlignedVoice $
  times 2 (pure $ aligned 0 0 $ music1)

music1 = klangfarben
  [violins,flutes,oboes,trumpets,tubas,clarinets,trombones,doubleBasses]
  (mconcat [c,e|*3,fs,g,a|/2,gs'|*3,g',fs',as,b,cs])

klangfarben :: HasParts' a => [Music.Score.Part a] -> Voice a -> Voice a
klangfarben ps v = (^.voice) $ zipWith (set parts') ps (v^.notes)

