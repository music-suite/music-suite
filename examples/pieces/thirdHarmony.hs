
import Music.Prelude
import Util (fromPitch'')

intervals  = [m3,_M3]
intervals2 = [d3,m3,_M3,_A3]

-- All possible combinations of the intervals smaller than one octave

ch' = c3 <> c4
 where
-- Three notes
  c3 = [[x,y] | x <- [0,1], y <- [0,1] ]
  -- Four notes
  c4 = [[x,y,z] | x <- [0,1], y <- [0,1], z <- [0,1] ]

ch :: [[Interval]]
ch = fmap (fmap (intervals !!)) ch'



cch' = c3 <> c4
 where
-- Three notes
  c3 = [[x,y] | x <- [0..3], y <- [0..3] ]
  -- Four notes
  c4 = [[x,y,z] | x <- [0..3], y <- [0..3], z <- [0..3] ]

cch :: [[Interval]]
cch = fmap (fmap (intervals2 !!)) cch'

{-
Dim
Min
Maj
Aug

Full-dim
Half-dim (inversion of Min6)
Min7 (inversion of Maj6)
Min#7
Maj7 (inversion of Min-6)
Min#7
Aug-min (inversion of Maj-6)
Aug
-}
music  = pseq $ fmap (ppar . fmap fromPitch'' . offsetPoints (c::Pitch)) ch
music2 = over pitches' (relative c $ spell usingSharps) $ pseq $ fmap (ppar . fmap fromPitch'' . offsetPoints (c::Pitch)) cch
