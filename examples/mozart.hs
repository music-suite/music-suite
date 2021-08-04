{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Music.Prelude
import qualified Music.Score

{-
    W.A. Mozart: Ave Verum (excerpt)

    Transcribed from autograph, see
        http://imslp.org/wiki/Ave_verum_corpus,_K.618_(Mozart,_Wolfgang_Amadeus)

    Divided as follows (including preceding accompaniement):

        stanza1:    Ave verum corpus natum de Maria virgine
        stanza2:    Vere passum immolatum in cruce pro homoni
        stanza3:    Cujus latus perforatum unda fluxit et sanguine
        stanza4:    Esto nobis praegustatum in mortis examine
-}

-- Vocal parts (TODO use actual voices!)
[vl1, vl2] = divide 2 violins

vla = violas

vc = cellos

-- Instruments
[sop, alt] = divide 2 flutes

ten = oboes

bc = clarinets

info =
  id
    . title "Ave Verum Corpus (excerpt)"
    . composer "W.A. Mozart"
    . keySignature (key d MajorMode)
    . tempo (metronome (1 / 4) 69)

music :: Music
music =
  info $ compress 4 $
    stanza1_voc <> stanza1_instr

-- Rhythm helper functions
lss l s1 s2 = l |* 2 |> s1 |> s2

ssl s1 s2 l = s1 |> s2 |> l |* 2

s3 s1 s2 s3 = s1 |> s2 |> s3

s4 s1 s2 s3 s4 = s1 |> s2 |> s3 |> s4

sl s l = s |> l |* 3

ls l s = l |* 3 |> s

fit2 x y = (x |> y) |/ 2

l4 l = l |* 4

ll l1 l2 = (l1 |> l2) |* 2

a2 = a |* 2

as2 = as |* 2

ab2 = ab |* 2

b2 = b |* 2

bs2 = bs |* 2

bb2 = bb |* 2

c2 = c |* 2

cs2 = cs |* 2

cb2 = cb |* 2

d2 = d |* 2

ds2 = ds |* 2

db2 = db |* 2

e2 = e |* 2

es2 = es |* 2

eb2 = eb |* 2

f2 = f |* 2

fs2 = fs |* 2

fb2 = fb |* 2

g2 = g |* 2

gs2 = gs |* 2

gb2 = gb |* 2

{-
    Can we "overload application" as in

    c       :: PitchL -> PitchL -> Score a
    (c d)   :: PitchL           -> Score a
    (c d) e ::                     Score a

    Alternatively, make music instance of IsString and use Lilypond syntax
-}

-- Stanza 1
stanza1_voc = stanza1_sop <> stanza1_alto <> stanza1_ten <> stanza1_bass

stanza1_sop =
  set parts' sop $ delay 8 $
    empty
      |> s3 a2 d' fs
      |> s3 a gs g2
      |> s4 g b a g
      |> ssl g fs fs
      |> ls e e
      |> s4 fs fs g g
      |> lss g (fit2 fs e) fs
      |> l4 e

stanza1_alto =
  set parts' alt $ delay 8 $
    empty
      |> ll fs fs
      |> ll e e
      |> s4 e g fs e
      |> ssl e d d
      |> ls cs cs
      |> s4 d d e e
      |> lss e (fit2 d cs) d
      |> l4 cs

stanza1_ten =
  set parts' ten $ delay 8 $ octavesDown 1 $
    empty
      |> ll a a
      |> ll b b
      |> ls a a
      |> ll a a
      |> ls e e
      |> s4 a a b b
      |> ls a a
      |> l4 e

stanza1_bass =
  set parts' bc $ delay 8 $ octavesDown 1 $
    empty
      |> ll d d
      |> ll d d
      |> ls cs cs
      |> ll d d
      |> ls a a
      |> s4 d d cs cs
      |> ls d d
      |> l4 a_

stanza1_instr = set parts' violins $ stanza1_vl1 <> stanza1_vl2 <> stanza1_vla <> stanza1_bc

stanza1_vl1 =
  empty
    |> s4 d a_ d e
    |> s4 fs d fs g
    |> lss a d' fs
    |> ssl a gs g
    |> s4 g b a g
    |> ssl g fs fs
    |> ls e e
    |> s4 fs fs g g
    |> lss g (fit2 fs e) fs
    |> l4 e

stanza1_vl2 =
  empty
    |> s4 d a_ d e
    |> s4 fs d fs g
    |> ll fs fs
    |> ll e e
    |> s4 e g fs e
    |> ssl e d d
    |> ls cs cs
    |> s4 d d e e
    |> lss e (fit2 d cs) d
    |> l4 cs

stanza1_vla =
  octavesDown 1 $
    empty
      |> s4 d a_ d e
      |> s4 fs d fs g
      |> ll a a
      |> ll b b
      |> ls a a
      |> ll a a
      |> ls e e
      |> s4 a a b b
      |> ls a a
      |> l4 e

stanza1_bc =
  octavesDown 1 $
    empty
      |> s4 d a_ d e
      |> s4 fs d fs g
      |> ll d d
      |> ll d d
      |> ls cs cs
      |> ll d d
      |> ls a a
      |> s4 d d cs cs
      |> ls d d
      |> l4 a_

main :: IO ()
main =
  defaultMain music
