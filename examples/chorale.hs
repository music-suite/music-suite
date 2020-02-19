
{-# LANGUAGE GADTs, OverloadedLists #-}

import Music.Prelude
import Control.Lens (set)
import qualified Music.Score as S


chorale :: (IsPitch a, HasParts' a, S.Part a ~ Part) =>
  [Voice (Maybe Pitch)] -> Score a
chorale = rcat . fmap renderVoice
  where
    renderVoice :: IsPitch a => Voice (Maybe Pitch) -> Score a
    renderVoice = fmap fromPitch . removeRests . renderAlignedVoice . aligned 0 0

music :: Music
music = compress 4 $ delay 3 $ chorale $ fmap mconcat
  [          [ e, a, g, f, e, d|*2, e, b, c', c', stretchTo 1 [b, a], b, a |* 3 ]
  , down _P8 [ c, f_, e_, a_, a_, f_, g_, c, gs_, a_, d_, e_, e_, a_ |*3 ]
  ]

main :: IO ()
main = defaultMain music
