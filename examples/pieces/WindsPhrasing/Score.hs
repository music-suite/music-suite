
{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleContexts #-}

import Music.Prelude

main = defaultMain music

{-
"Haberdashery"

Work with basic phrases and dynamics in winds.
Set off against long pedals in strings.

Possibly:
  - Work more with dynamics
  - Instead of just repeating (times 5), use repetition with variation (Int -> a).

-}

x :: Music
x = compress 16 $ times 10 $ ((times 5 $ legato $ (pseq [e,f,e,ds]) |> pseq[g,f,e,ds]) |> rest |*2 )

fls  = up   _P12 $ set (parts'.instrument) flute    $ tempo presto (rcat [x,delay 2.5 x,delay 5 x])
obs  = up   _P8  $ set (parts'.instrument) oboe     $ tempo presto (rcat [x,delay 4 x,delay 5.5 x])
cls  = down _P1  $ set (parts'.instrument) clarinet $ tempo presto (rcat [x,delay 3 x,delay 5 x])
bsns = down _P5  $ set (parts'.instrument) bassoon  $ tempo presto (rcat [x,delay 3 x,delay 7 x])

-- music = cls `sustain` set parts' violins1 (level pp b')
music =
  delay 6 (fls `sustain` set parts' violas (level pp b'))
    <>
  delay 12 (obs `sustain` set parts' violins2 (level pp b'))
    <>
  delay 0 (cls `sustain` set parts' violins1 (level pp b'))
    <>
  delay 18 (bsns `sustain` set parts' cellos (level pp b'))
    <>
  br

br =
  arrangeFor (divide 3 horns ++ divide 3 trumpets ++ divide 3 trombones ++ [tubas]) $
    (level ppp $ times 4 $ stretch (14/5) $ pseq [rest |/2, rcat
      [cs',b,g,cs,ds,fs,g_,f_,e_,g__], rest])





