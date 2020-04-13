import Data.List (intersperse)
import Music.Prelude

main = defaultMain music

music =
  -- timeSignatureDuring (0 <-> 10000) (3/2) $
  pseq $
    intersperse
      rest
      [ mempty,
        -- x1,
        -- x2,
        -- b1,
        c1,
        c1,
        c1
      ]

-- "not quite" resolutions
x1, x2 :: Music
x1 =
  set parts' violins $
    ppar
      [ pseq [c, b_],
        pseq [a, a],
        pseq [fs', g']
      ]
x2 =
  set parts' violins $
    ppar
      [ pseq [c, b_],
        pseq [a, a],
        pseq [fs', g'],
        pseq [c'', c'']
      ]

-- Basic motive with different conclusions
--
-- TODO echo/dying figure (the stacc notes): should fade out and lead to a rest
-- before the main BEBA motive is repeated
--
-- TODO abstract out more below
--
-- TODO introduce more rests
b1 :: Music
b1 =
  set parts' clarinets $ compress 16 $ level pp $
    times 1 (legato $ pseq [b_, e, b_, a_]) |> (staccato $ times 1 $ pseq [b_, b_, b_, b_])
      |> times 1 (legato $ pseq [b_, e, b_, a_])
      |> (staccato $ times 2 $ pseq [b_, b_, b_, b_])
      |> times 2 (legato $ pseq [b_, e, b_, a_])
      |> (b_ |* (4 * 3))

-- appleause, cowbells etc

-- sudden drama, quick rise
--
-- TODO make chord fall on downbeat (use Aligned)
--
-- TODO instead of chromatic rising scale use some (still dissonant/non-diatonic) subset
-- What is that pitch space?
c1 :: Music
c1 =
  ( legato $ over phrases' (cresc pp _f) $ accentLast $ compress 24
      $ set parts' flutes
      $ pseq
      $ fmap fromPitch
      $ enumChromaticFromTo c' g''
  )
    |> ( level ff $
           -- TODO orchestrate with brass/pno/strings?
           -- add repetition patterns (just restating chord notes, a la "Varseblivning")
           ( (level ff $ accent $ ppar [gs'', cs'', b, a])
               <> (level mp $ set parts' horns $ accent $ ppar [d, a_])
           )
       )
