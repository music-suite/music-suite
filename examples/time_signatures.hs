
{-# LANGUAGE OverloadedStrings #-}

import Music.Prelude

main = open music

music = id

  $ title "Time signatures"

  -- Try commenting out some of these lines
  $ timeSignature (6/8)
  $ timeSignature (3/4)
  $ timeSignature (2/4)
  $ timeSignature ((4+3)/8)
  $ timeSignature ((3+4)/8)
  
  $ scat [c,c',b,bb,a,as,g|*2,scat [f,e,d,b_]|/2,d|*2,c|*2]|/8
