
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This example shows a Webern style orchestration of a single voice
-- distributed throughout several parts.
--
-- We accomplish this with the zip-like function 'klangfarben'.
module Main where

import Music.Prelude
import qualified Music.Score

main :: IO ()
main = defaultMain music

music :: Music
music = compress 4 $ renderAlignedVoice $ aligned 0 0 $ orchestrated

orchestrated :: (HasParts' a, HasPitches' a, Music.Score.Pitch a ~ Pitch, Music.Score.Part a ~ Part, IsPitch a) => Voice a
orchestrated = _8vb $ klangfarben
  (cycle (vs++[violas,cellos,doubleBasses,flutes, oboes, clarinets,bassoons,horns,trumpets,trombones,tutti timpani]))
  -- [violins,flutes,oboes,trumpets,tubas,clarinets,trombones,doubleBasses]
  (mconcat $ [c,d,e,f,g,a,b,c',d',e',f',g',a',b',c'',d''])
  -- (mconcat [c,e|*3,fs,g,a|/2,gs'|*3,g',fs',as,b,cs])

[v1,v2,v3,v4,v5] = vs
vs = divide 5 violins

klangfarben :: HasParts' a => [Music.Score.Part a] -> Voice a -> Voice a
klangfarben ps v = (^.voice) $ zipWith (set parts') ps (v^.notes)

