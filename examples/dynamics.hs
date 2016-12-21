
{-# LANGUAGE OverloadedStrings #-}

import Music.Prelude

-- A simple subject
subj  = times 20 $ scat [c,d,e,f]|/8 |> scat [g,fs]|/2

-- The 
music = id
  $ title "Dynamics"
  $ composer "Anonymous"
  $ fmap (over dynamics (! 0)) 
  $ 
    rcat $ map (\phase -> level (stretch phase sine*fff) $ subj) [5.0,5.2..6.0]

main = open music
