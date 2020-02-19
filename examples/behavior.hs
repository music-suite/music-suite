
{-# LANGUAGE TypeApplications #-}

-- This example shows how to use behaviors (functions of time) to control
-- various aspects of the music.
import Music.Prelude
import Control.Lens (set)

-- Converts a 'Double' to a 'Pitch' in semitones.
toPitch :: Double -> Pitch
toPitch x = c .+^ si x
  where
    si t = spell usingSharps $ floor @_ @Semitones t

music :: [Score (Behavior Pitch)]
music = fmap (\(p, x) -> set pitches (fmap toPitch p) x) sines

-- Here 'delay' and 'stretch' are used to adjust phase and frequency
-- of a sine.
--
-- Note that numeric operators are defined pointwise on behaviors.
-- The primitive 'sine' function has amplitude 1, so (sine * n) has
-- amplitude n.

sines :: [(Behavior Double, Score ())]
sines =
  [ (delay 0 $ stretch 12 $ sine * 7, times 24 c)
  , (delay 0 $ stretch 12 $ sine * 7, times 48 c |/ 2)
  , (delay 5 $ stretch 12 $ sine * 7, times 24 c)
  , (delay 5 $ stretch 12 $ sine * 7, times 48 c |/ 2)
  , (delay 5 $ stretch 12 $ sine * 3, times 24 c)
  , (delay 5 $ stretch 12 $ sine * 3, times 48 c |/ 2)
  ]

at :: IsPitch a => Behavior Pitch -> a
at = fromPitch . ($ 0) . view sampled

main :: IO ()
main = defaultMain $ rcat $ fmap (fmap at) $ music
