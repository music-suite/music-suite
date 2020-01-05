
{-# LANGUAGE OverloadedStrings #-}

import Music.Prelude
import Control.Lens (over)

-- A simple subject
subj :: Score (DynamicT (Behavior Amplitude) Pitch)
subj  = times 20 $ scat [c,d,e,f]|/8 |> scat [g,fs]|/2

music = id
  $ title "Dynamics"
  $ composer "Anonymous"
  $ fmap (over dynamics (! 0))
  $ 
    rcat $ map (\phase -> level (stretch phase sine*fff) $ subj) [5.0,5.2..6.0]

main = defaultMain music
